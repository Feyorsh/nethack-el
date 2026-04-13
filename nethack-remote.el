;;; nethack-remote.el --- Play and spectate remote NetHack sessions -*- lexical-binding:t -*-

;; Copyright (C) 2026 George Huebner

;; Author: George Huebner <george@feyor.sh>
;; Maintainer: George Huebner <george@feyor.sh>
;; Created: Sat Apr 4 09:57:13 2026

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This package lets you play nethack-el on (patched) dgamelaunch
;; servers or over plain SSH.
;;
;; You can also spectate nethack-el sessions on a dgamelaunch server.

;;; Code:

(require 'nethack)
(require 'nethack-lisprec)

(require 'cl-lib)

(defvar nethack-remote--lisprec-buffer nil)
(defvar nethack-remote-spectate-proc nil)

(defconst nethack-remote--dgamelaunch-watch-games-regexp
  (eval-when-compile
    (string-join
     '("\\([[:graph:]]\\))\\s-*\\(?:"
       "\\)?\\([[:alnum:]]+\\)"
       "\\([[:graph:]]+\\)"
       "lisp"
       "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)"
       "\\([0-9]+h [0-9]+m\\|[0-9]+m [0-9]+s\\|[0-9]+s\\| \\)"
       "\\([0-9]+\\)")
     "\\(?:\\s-*\\(?:\033\\[\\(?:[0-9]+;\\)?[0-9]+[mH]\\|\033(B\033\\[m\\|\033\\[[0-9]+b\\)\\s-*\\)+")))

(defcustom nethack-remote-connection-commands
  '(("SSH" "ssh nethack@%s" "ssh -o SetEnv=DGLAUTH=%d nethack@%s")
    ("Telnet" "telnet %s" "telnet -l %d %s"))
  "Connection commands for connecting to a remote nethack-el process."
  :type '(alist :key-type string :value-type (list string))
  :group 'nethack)

(defcustom nethack-remote-servers nil
  "Servers to play/spectate nethack-el on."
  :type '(list string)
  :group 'nethack)


(defun nethack-remote--ansi-filter (proc filter)
  (letrec ((wrap-filter-hook (lambda (&rest _)
                               (remove-hook 'nethack-at-prompt-hook wrap-filter-hook t)
                               (set-process-filter proc (lambda (process output)
                                                          ;; yes, I'm aware that we have to hope this comes in as one message
                                                          (if (not (string-match-p "\033\\[\\?1l\033>" output))
                                                              (funcall filter proc output)
                                                            (kill-process proc)))))))
    (add-hook 'nethack-at-prompt-hook wrap-filter-hook nil t)))

(defun nethack-remote--dos2unix-filter (proc output)
  (with-current-buffer (process-buffer proc)
    (let ((marker (process-mark proc)))
      (save-excursion
        (goto-char marker)
        (let ((inhibit-read-only t))
          (insert (string-replace "\r" "" output)))
        (set-marker marker (point)))))
  (timer-set-time nethack-lisprec--timer '(0 0 0 0)))

(defun nethack-remote--dgamelaunch-auth (proc state &optional username password)
  "Navigate dgamelaunch login before handing process to nethack-start."
  (set-process-filter proc
                      (lambda (process output)
                        (with-current-buffer (process-buffer process)
                          (let ((output-unstripped output))
                          (setq output (replace-regexp-in-string "\033\\[[0-9;]*[a-zA-Z]\\|\033[()][AB012]\\|\r" "" output))
                          (goto-char (point-max))
                          (insert output)
                            (when (search-backward-regexp "there was a problem with your last entry" nil t)
                              (kill-process process)
                              (user-error "could not login with username %s" username))
                            (when (search-backward-regexp "login failed\\. returning to game\\." nil t)
                              (user-error "could not login with username %s" username))
                            (pcase state
                              ('initial-play
                               ;; attempt to work on "raw" connections that don't use dgamelaunch
                               (if (search-backward-regexp nethack--lisp-header nil t)
                                   (progn
                                     (setq output (buffer-substring-no-properties (point) (point-max)))
                                     (erase-buffer)
                                     (nethack-remote--ansi-filter process #'nethack-filter)
                                     (set-process-filter process #'nethack-filter)
                                     (nethack-filter process output))
                                 (when (search-backward-regexp "\\([[:ascii:]]\\)) login" nil t)
                                   (setq state 'login-play)
                                   (process-send-string process (match-string 1)))))
                              ((or 'login-play 'login-spectate)
                               (when (search-backward-regexp "enter your username" nil t)
                                 (setq state (pcase state ('login-play 'sent-username-play) ('login-spectate 'sent-username-spectate)))
                                 (process-send-string process (concat (or username (setq username (read-string "Username: "))) "\n"))))
                              ((or 'sent-username-play 'sent-username-spectate)
                               (when (search-backward-regexp "enter your password" nil t)
                                 (setq state (pcase state ('sent-username-play 'sent-password-play) ('sent-username-spectate 'sent-password-spectate)))
                                 (process-send-string process (concat (or password (read-passwd "Password: ")) "\n")))
                               (when password (clear-string password)))
                              ('sent-password-play
                               (when (search-backward-regexp "\\([[:ascii:]]\\)) play nethack" nil t)
                                 (setq state 'sent-play)
                                 (process-send-string process (match-string 1))))
                              ('sent-play
                               ;; output of clear/refresh/endwin right before ttyrec_main
                               (when (search-backward-regexp "\033\\[\\?1049l\033\\[\\?1l\033>\\(.*\\)" nil t)
                                 (setq output (match-string 1))
                                 (erase-buffer)
                                 (nethack-remote--ansi-filter process #'nethack-filter)
                                 (nethack-start process)
                                 (nethack--prefilter process output)))
                              ('initial-spectate
                               (when (search-backward-regexp "\\([[:ascii:]]\\)) watch games in progress" nil t)
                                 (setq state 'list-games)
                                 (process-send-string process (match-string 1))))
                              ('list-games
                               (when (search-backward-regexp "the following games are in progress:" nil t)
                                 (delete-region (point) (point-max))
                                 (save-excursion (insert output-unstripped))
                                 (let (games)
                                   (save-restriction
                                     (narrow-to-region (point) (point-max))
                                     (while (search-forward-regexp nethack-remote--dgamelaunch-watch-games-regexp nil t)
                                       (push
                                        (let (g)
                                          (cl-loop
                                                for (start end)
                                                on (match-data)
                                                by #'cddr
                                                do (when (and start end) (push (buffer-substring-no-properties start end) g)))
                                          (cdr (reverse g)))
                                        games)))
                                   (let* ((inhibit-quit t)
                                          (minibuffer-allow-text-properties t)
                                          (game-listings (mapcar (lambda (g) `(,(propertize (cadr g) 'index (car g)) ,@(cddr g))) games))
                                          (completion-extra-properties
                                           (list :annotation-function
                                                 (lambda (p)
                                                   (apply #'format #("%s  %-5s   %s   %-10s %s" 4 7 (face font-lock-keyword-face) 11 12 (face font-lock-doc-face) 16 20 (face font-lock-number-face) 22 23 (face font-lock-type-face))
                                                          (make-string (- (apply #'max (mapcar (lambda (e) (length (cadr e))) games)) (length p)) ? )
                                                          (cdr (assoc-string p game-listings))))))
                                          (game (get-text-property 0 'index (completing-read "What game would you like to spectate? " game-listings nil t))))
                                     (erase-buffer)
                                     (setq state 'sent-watch)
                                     (process-send-string process game)))))
                              ('sent-watch
                               (goto-char (point-min))
                               (when (search-forward-regexp nethack--lisp-header nil t)
                                 (delete-region (point-min) (match-beginning 0))
                                 (goto-char (point-min))
                                 (set-marker (process-mark process) (point-max))
                                 (set-process-filter process #'nethack-remote--dos2unix-filter)
                                 (read-only-mode 1)
                                 (nethack-lisprec--playback-loop (point-min) (point-min) nil (current-buffer) t)))
                              ((or 'sent-password-spectate 'login-spectate)
                               (when (search-backward-regexp "enter your message here\\. it is to be one line only and \\([0-9]+\\) characters or less\\." nil t)
                                 (setq state 'sent-message-spectate)
                                 (let ((character-limit (string-to-number (match-string 1)))
                                       (msg (condition-case _ (read-string "Enter your message here: ") (quit " "))))
                                   (while (or (length> msg character-limit) (string-match-p "\n" msg))
                                     (run-with-timer 0.01 nil (lambda () (minibuffer-message (if (length> msg character-limit)
                                                                                                 (format "message must be less than %d characters" character-limit)
                                                                                               "message must be one line"))))
                                     (setq msg (condition-case _ (read-string "Enter your message here: ") (quit " "))))
                                   (process-send-string process (concat msg "\n")))))
                              ('sent-message-spectate
                               (read-only-mode -1)
                               (if (not (search-backward-regexp "--more--" nil t))
                                   (minibuffer-message "Scroll delivered!")
                                 (minibuffer-message "There was an error delivering your scroll.")
                                 (read-from-minibuffer "--more--"))
                               (set-process-filter process #'nethack-remote--dos2unix-filter)
                               (nethack-lisprec-autoplay-resume))))))))

(defmacro nethack-remote--defun-connector (name anonp docstring &rest body)
  `(progn
     (defun ,name (connection-command &optional host port)
       ,docstring
       (interactive (let* ((completion-extra-properties
                            (list :annotation-function
                                  (lambda (p)
                                    (format #("%s    %s" 6 8 (face font-lock-doc-face))
                                            (make-string (- (apply #'max (mapcar (lambda (e) (length (car e))) nethack-remote-connection-commands)) (length (car (assoc-string p nethack-remote-connection-commands)))) ? )
                                            (funcall #',(if anonp #'cadr #'caddr) (assoc-string p nethack-remote-connection-commands))))))
                           (protocol (completing-read "Protocol (or command to connect): " nethack-remote-connection-commands nil 'confirm-after-completion)))
                      (list (if-let ((cmd (assoc-string protocol nethack-remote-connection-commands)))
                                (funcall #',(if anonp #'cadr #'caddr) cmd)
                              protocol))
                      ))
       (let (username password)
         (when (and (string-match-p "%s" connection-command) (not host))
           (setq host (completing-read "Server: " nethack-remote-servers nil nil)))
         (when (and (string-match-p "%p" connection-command) (not port))
           (setq port (read-number "Port: ")))
         (when (string-match-p "%d" connection-command)
           (setq connection-command
                 (format-spec connection-command
                              `((?d . ,(or (getenv "DGLAUTH")
                                           (format "%s:%s"
                                                   (setq username (read-string "Username: "))
                                                   (setq password (read-passwd "Password: "))))))
                              'ignore)))
         ,@body))))

;;;###autoload (autoload 'nethack-remote "nethack-remote")
(nethack-remote--defun-connector
  nethack-remote nil
  "Start a game of Nethack with CONNECTION-COMMAND.

CONNECTION-COMMAND is a format string with three specs: %s for host,
%p for port, and %d for username:password.

As an example, one could login to Hardfought with (nethack-remote
\"ssh -o SetEnv=DGLAUTH=%d nethack@%s\" \"us.hardfought.org\")."
  (if (nethack-is-running)
      (progn (message "NetHack process already running...")
             (nethack-restore-windows))
    (nethack-kill-buffers)
    (when (get-buffer nethack-proc-buffer-name)
      (kill-buffer nethack-proc-buffer-name))
    (get-buffer-create nethack-proc-buffer-name)
    (let ((proc (open-network-stream "nh" nethack-proc-buffer-name
                                     host port :type 'shell :shell-command connection-command)))
      (nethack-remote--dgamelaunch-auth proc 'initial-play username password))))

(defun nethack-remote-spectate-quit ()
  "Stop watching the currently viewed nethack-el game."
  (interactive)
  (when-let* ((nethack-lisprec--timer)
              (buf (nth 3 (timer--args nethack-lisprec--timer)))
              (proc (get-buffer-process buf))
              ((eq (process-status proc) 'run)))
    (with-current-buffer buf (erase-buffer))
    (nethack-remote--dgamelaunch-auth proc 'initial-spectate)
    (if (not (y-or-n-p "Would you like to watch another game?"))
        (kill-process proc)
      (process-send-eof proc)
      (process-send-string proc "w")))
  (nethack-lisprec-cancel))

(defun nethack-remote-spectate-send-mail ()
  "Send mail to the nethack-el player being spectated."
  (interactive)
  (if-let* ((nethack-lisprec--timer)
            (buf (nth 3 (timer--args nethack-lisprec--timer)))
            (proc (get-buffer-process buf))
            ((eq (process-status proc) 'run)))
      (progn
  (nethack-lisprec-autoplay-pause)
  (with-current-buffer buf (read-only-mode -1))
  (nethack-remote--dgamelaunch-auth proc 'login-spectate)
  (process-send-string proc "m"))
    (user-error "Not currently spectating a nethack-el game.")))

(define-minor-mode nethack-remote-spectate-minor-mode
  "Minor mode for controlling lisprec playback."
  :keymap `((,(kbd "C-c C-m") . nethack-remote-spectate-send-mail)
            (,(kbd "C-c C-q") . nethack-remote-spectate-quit)))

;;;###autoload (autoload 'nethack-remote-spectate "nethack-remote")
(nethack-remote--defun-connector
  nethack-remote-spectate t
  "Spectate a game of nethack-el on a remote dgamelaunch server."
  (when (process-live-p nethack-proc) (user-error "Current NetHack process must end for lisprec playback to occur."))
  (when nethack-lisprec--timer (user-error "A lisprec recording is already playing."))
  (nethack-lisprec-global-mode +1)
  (setq nethack-lisprec--seek-to t)
  (setq nethack-lisprec-autoplay--enabled t)
  (setq nethack-proc nil)
  (nethack-kill-buffers)
  (nethack-reset-status-variables)

  (let* ((spectate-buffer (generate-new-buffer " *nethack spectate*"))
         (proc (open-network-stream "nh" spectate-buffer
                                    host port :type 'shell :shell-command connection-command)))
    (nethack-remote--dgamelaunch-auth proc 'initial-spectate username password)))



(provide 'nethack-remote)

;;; nethack-remote.el ends here
