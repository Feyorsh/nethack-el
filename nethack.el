;;; nethack.el --- run Nethack as a subprocess -*- lexical-binding:t -*-

;; Copyright (C) 2003,2005  Ryan Yeske and Shawn Betts

;; Author: Ryan Yeske <rcyeske@vcn.bc.ca>
;; Created: Sat Mar 18 11:31:52 2000
;; Version: $Id$
;; Keywords: games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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
;; Note: This package requires external libraries and works currently
;; only on GNU/Linux systems.
;;
;; Note: If you ever update it, you need to restart Emacs afterwards.
;;
;; To activate the package put
;;
;; (nethack-install)
;;
;; somewhere in your .emacs.el .

;;; Code:

(require 'nethack-compat)
(require 'nethack-api)
(require 'nethack-cmd)
(require 'nethack-keys)
(require 'nethack-options)
(require 'url)
(require 'tramp)

(defgroup nethack nil
  "Emacs lisp frontend to the lisp window port of Nethack 3.4.0."
  :group 'games)

(defcustom nethack-status-window-height 4
  "Height of the status window."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-message-window-height 10
  "Height of the message window."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-status-highlight-delay
  (if-let ((op (cdr-safe (nethack-options-set-p "statushilites"))))
      (string-to-number op)
    5)
  "The number of turns to keep a changed status field highlighted."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-status-buffer-format
  "n s d c i W C A S\nl g hH pP a eED t Grfv"
  "Format string for the status in `nh-status-buffer'."
  :type '(string)
  :group 'nethack)

(defcustom nethack-status-mode-line-format
  "s d c i W C g h p a e t"
  "Format string for the status on the mode-line."
  :type '(string)
  :group 'nethack)

(defcustom nethack-status-header-line-format
  "n <L,l> A   f"
  "Format string for the status on the header-line."
  :type '(string)
  :group 'nethack)

(defcustom nethack-status-style t
  "Decides how the status will be displayed. Valid values are :map, :header-line, :mode-line, or t."
  :type '(symbol)
  :options '(:map :mode-line :header-line t)
  :group 'nethack)

(defcustom nethack-purge-buffers t
  "When this variable is non-nil, kill all nethack buffers when nethack quits."
  :type '(boolean)
  :group 'nethack)

;;; Insert variables that control how the status gets displayed here.

(defcustom nethack-use-tiles nil
  "If non-nil, use XPMs to draw tiles."
  :type '(boolean)
  :group 'nethack)

(defcustom nethack-map-mode-hook nil
  "Functions to be called after setting up the Nethack map."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-menu-mode-hook nil
  "Functions to be called after setting up a Nethack menu."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-before-print-message-hook nil
  "Hook run before a message is printed."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-message-style t
  "Decides where messages appear. :map means messages display in
the map buffer. t means in a seperate buffer."
  :type '(symbol)
  :options '(:map t)
  :group 'nethack)

(defcustom nethack-prompt-style t
  "Decides where nethack-el prompts for input. :map means in the
map buffer. t means in the minibuffer."
  :type '(symbol)
  :options '(:map t)
  :group 'nethack)

(defcustom nethack-end-hook nil
  "Hook run when nethack has ended."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-status-attribute-change-functions nil
  "List of functions to call after a status attribute change.
Three arguments are passed to each function: the name of the
attribute, the new value, the old value, and the percent."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-load-hook nil
  "Hook run after loading nethack."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-add-menu-hook nil
  "Hook run after a menu option has been added."
  :type '(hook)
  :group 'nethack)


(defgroup nethack-faces nil
  "Customizations for faces used by Enethack."
  :group 'nethack)

(defface nethack-message-highlight-face
  '((t (:foreground "black" :background "green")))
  "The face used to highlight new text in the message window."
  :group 'nethack-faces)

(defface nethack-atr-none-face
  `((t ()))
  "Nethack default face."
  :group 'nethack-faces)

(defface nethack-atr-dim-face
  `((t (:weight light)))
  "Nethack dim face."
  :group 'nethack-faces)

(defface nethack-atr-blink-face
  `((t (:inverse-video t)))
  "Nethack blink face."
  :group 'nethack-faces)

(defface nethack-atr-uline-face
  `((t (:underline t)))
  "Nethack underline face."
  :group 'nethack-faces)

(defface nethack-atr-inverse-face
  `((t (:inverse-video t)))
  "Nethack inverse face."
  :group 'nethack-faces)

(defface nethack-atr-bold-face
  `((t (:bold t)))
  "Nethack bold face."
  :group 'nethack-faces)

(defface nethack-black-face
  `((t (:foreground "dark blue")))
  "nethack black face"
  :group 'nethack-faces)

(defface nethack-red-face
  `((((type tty) (class color))
     (:foreground "red"))
    (((class color))
     (:foreground "red"))
    (t (:foreground "gray")))
  "nethack red"
  :group 'nethack-faces)

(defface nethack-green-face
  `((((type tty) (class color))
     (:foreground "green"))
    (((class color) (background dark))
     (:foreground "lime green"))
    (((class color) (background light))
     (:foreground "lime green"))
    (t (:foreground "gray")))
  "nethack green"
  :group 'nethack-faces)

(defface nethack-brown-face
  `((((type tty) (class color))
     (:foreground "yellow"))
    (((class color) (background dark))
     (:foreground "chocolate"))
    (((class color) (background light))
     (:foreground "brown"))
    (t (:foreground "gray")))
  "nethack brown"
  :group 'nethack-faces)

(defface nethack-blue-face
  `((((type tty) (class color))
     (:foreground "blue"))
    (((class color) (background dark))
     (:foreground "dark blue"))
    (((class color) (background light))
     (:foreground "dark blue"))
    (t (:foreground "gray")))
  "nethack blue"
  :group 'nethack-faces)

(defface nethack-magenta-face
  `((((type tty) (class color))
     (:foreground "magenta"))
    (((class color) (background dark))
     (:foreground "dark magenta"))
    (((class color) (background light))
     (:foreground "dark magenta"))
    (t (:foreground "gray")))
  "nethack magenta"
  :group 'nethack-faces)

(defface nethack-cyan-face
  `((((type tty) (class color))
     (:foreground "cyan"))
    (((class color) (background dark))
     (:foreground "dark cyan"))
    (((class color) (background light))
     (:foreground "cyan4"))
    (t (:foreground "gray")))
  "nethack cyan"
  :group 'nethack-faces)

(defface nethack-gray-face
  `((((type tty) (class color))
     (:foreground "white"))
    (((class color) (background dark))
     (:foreground "lightgray"))
    (((class color) (background light))
     (:foreground "darkgray"))
    (t (:foreground "gray")))
  "nethack gray"
  :group 'nethack-faces)

(defface nethack-dark-gray-face
  `((((type tty) (class color))
     (:foreground "black" :bold t))
    (((class color) (background dark))
     (:foreground "darkgray"))
    (((class color) (background light))
     (:foreground "lightgray"))
    (t (:foreground "gray")))
  "nethack dark gray"
  :group 'nethack-faces)

(defface nethack-orange-face
  `((((type tty) (class color))
     (:foreground "red" :bold t))
    (((class color))
     (:foreground "orange"))
    (t (:foreground "gray")))
  "nethack light orange"
  :group 'nethack-faces)

(defface nethack-bright-green-face
  `((((type tty) (class color))
     (:foreground "green" :bold t))
    (((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "dark green"))
    (t (:foreground "gray")))
  "nethack bright green"
  :group 'nethack-faces)

(defface nethack-yellow-face
  `((((type tty) (class color))
     (:foreground "yellow" :bold t))
    (((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "yellow3"))
    (t (:foreground "gray")))
  "nethack yellow"
  :group 'nethack-faces)

(defface nethack-bright-blue-face
  `((((type tty) (class color))
     (:foreground "blue" :bold t))
    (((class color) (background dark))
     (:foreground "blue"))
    (((class color) (background light))
     (:foreground "blue"))
    (t (:foreground "gray")))
  "nethack bright blue"
  :group 'nethack-faces)

(defface nethack-bright-magenta-face
  `((((type tty) (class color))
     (:foreground "magenta" :bold t))
    (((class color))
     (:foreground "magenta"))
    (t (:foreground "gray")))
  "nethack bright magenta"
  :group 'nethack-faces)

(defface nethack-bright-cyan-face
  `((((type tty) (class color))
     (:foreground "cyan" :bold t))
    (((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "cyan3"))
    (t (:foreground "gray")))
  "nethack bright cyan"
  :group 'nethack-faces)

(defface nethack-white-face
  `((((type tty) (class color))
     (:foreground "white" :bold t))
    (((class color) (background dark))
     (:foreground "white"))
    (((class color) (background light))
     (:foreground "black"))
    (t (:foreground "gray")))
  "nethack white"
  :group 'nethack-faces)

(defface nethack-map-tile-face
  `((((type tty))
     nil)
    (t (:height 16)))
  "Map face with height less than the tile size (16 pixels)."
  :group 'nethack-faces)



;;; Installation

(defconst nethack-el-directory
  (file-name-as-directory (or (and load-file-name
                                   (file-name-directory load-file-name))
                              default-directory))
  "The directory from where this library was first loaded.")

(defcustom nethack-build-directory
  (expand-file-name "build" nethack-el-directory)
  "The directory in which to build nethack.
You can influence the location of the build directory by setting
this variable.  If `nethack-program' is set to a working
compatible version of NetHack complied with the lisp patch, then
`nethack-build-directory' is never used."
  :type '(string)
  :group 'nethack)

(defcustom nethack-program
  (expand-file-name "nethack" nethack-build-directory)
  "Program to run to start a game of Nethack.
If this variable is custom-set outside of the default
`nethack-build-directory', and it does indeed point to a working
compatible version of NetHack with the lisp patch, then
`nethack-build-directory' is never consulted during
installation."
  :type '(string)
  :group 'nethack)

(defcustom nethack-program-args nil
  "Arguments to pass to `nethack-program'."
  :type '(repeat string)
  :group 'nethack)

(defcustom nethack-environment nil
  "Additional environment variables to pass to NetHack process.
See https://nethackwiki.com/wiki/Environment_variable for more information."
  :type '(repeat string)
  :group 'nethack)

(defcustom nethack-wizmode nil
  "Whether NetHack should be launched in wizard (debug) mode."
  :type '(boolean)
  :group 'nethack)

(defcustom nethack-version
  "3.6.6"
  "The NetHack version to download, install, and bulid."
  :group 'nethack
  :type 'string)

;; It might be a bad, bad practice to make these functions, but it made sense at
;; the time.
(defun nethack-version-nodots ()
  "The NetHack version without separating dots."
  (replace-regexp-in-string "\\." "" nethack-version))

(defun nethack-query-for-version ()
  "Queries the user for the NetHack version.
Currently, the two supported versions are 3.6.6 and 3.4.3."
  (interactive)
  (read-answer "NetHack version "
               '(("3.6.6" ?6 "366")
                 ("3.4.3" ?3 "343"))))

(defun nethack-installed-p ()
  "Determine if a patched NetHack is installed.
Checks whether a NetHack executable exists, and if running it
results in an output with prefix ``(nhapi-raw-print'' with the correct NetHack
version and the correct version for the lisp-patch."
  (and nethack-program
       (file-executable-p nethack-program)
       (let ((version-string
              (shell-command-to-string
               (concat nethack-program " --version"))))
         (version<=
          nethack-el-earliest-compatible-version
          (and (string-match
                (concat "NetHack Version "
                        "[0-9]+\\.[0-9]+\\.[0-9]+\\(?:-[0-9]+\\)?"
                        " lisp-patch "
                        "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)")
                version-string)
               (match-string-no-properties 1 version-string))))))

(defun nethack-build (&optional
                      callback
                      no-download-p
                      build-directory)
  "Build the NetHack program in the background.
If CALLBACK is non-nil, it should be a function.  It is called
with the compiled executable as the single argument or nil, if
the build failed.

If NO-DOWNLOAD-P is non-nil, then no NetHack tarball will be
downloaded and one will already be assumed to be in
‘nethack-build-directory’/nethack.tgz.

If BUILD-DIRECTORY is non-nil, then `nethack-build-directory'
will be set to BUILD-DIRECTORY.  The NetHack executable will be
located within the BUILD-DIRECTORY.

Returns the buffer of the compilation process."
  (unless callback (setq callback #'ignore))
  (when build-directory
    (setq-default nethack-build-directory build-directory)
    (setq-default nethack-program
                  (expand-file-name "nethack" nethack-build-directory)))
  (unless (file-exists-p nethack-build-directory)
    (mkdir nethack-build-directory))
  ;; needs to make patch, hints(-3.6), and build
  ;; make patch simply patches
  ;; make hints runs ./setup.sh
  ;; make hints-3.6 runs ./setup.sh hints/linux-lisp
  ;; make build runs make all and make install in nethack-src
  (let* ((default-directory nethack-build-directory)
         (source-directory (expand-file-name "nethack-src" default-directory)))
    (unless (file-exists-p source-directory)
      (mkdir source-directory))
    (unless no-download-p (nethack-build-download))
    (nethack-build-untar)
    (nethack-build-patch)
    (nethack-build-setup)
    (nethack-build-compile callback)))

(defun nethack-build-download ()
  "Download the nethack source from nethack.org.
The source is saved as nethack.tgz within the
`default-directory'."
  (let* ((nethack-tar (concat "/nethack-" (nethack-version-nodots) "-src.tgz"))
         (nethack-url
          (concat "https://nethack.org/download/" nethack-version nethack-tar)))
    (url-copy-file nethack-url (expand-file-name "nethack.tgz"
                                                 default-directory)
                   t)))                 ; It's OK if it already exists.

(defun nethack-build-untar ()
  "Untar the nethack source out of nethack-tar.
Untars the file nethack.tgz located in ‘default-directory’ into
‘default-directory’/nethack-src.

Note that this is system specific to GNU tar and BSD tar, since
it relies on using the flag --strip-components."
  (shell-command
   (format "tar xzf %s/nethack.tgz -C %s %s"
           default-directory
           source-directory
           "--strip-components=1 --ignore-command-error")))

(defun nethack-build-patch ()
  "Patch the NetHack source with lisp patches."
  ;; cd nethack-src && patch -Nr- -p1 < ../../enh-$(NH_VER_NODOTS).patch || true
  (let ((default-directory source-directory))
    (process-file-shell-command
     "patch -Nr- -p1"
     (concat nethack-el-directory "enh-" (nethack-version-nodots) ".patch"))))

(defun nethack-build-setup ()
  "Setup the NetHack with ./setup.sh.
Uses the hints file for >3.6."
  ;; cd nethack-src/sys/unix && $(SHELL) ./setup.sh
  ;; or
  ;; cd nethack-src/sys/unix && $(SHELL) ./setup.sh hints/linux-lisp
  (let ((default-directory (expand-file-name "sys/unix" source-directory)))
    (process-file-shell-command
     (concat "./setup.sh"
             (when (version<= "3.6.0" nethack-version)
               " hints/linux-lisp")))))

(defun nethack-build-compile (callback)
  "Compile NetHack with make.
CALLBACK is called when the compilation finishes (with no
arguments).

Returns the buffer of the compilation process.

Requires ‘make’, ‘gcc’, ‘bison’ or ‘yacc’, ‘flex’ or ‘lex’, and
the ncurses-dev library for your system."
  ;; make all && make install
  (let* ((default-directory source-directory)
         (compilation-cmd
          ;; Right now, since there are two make arguments passed here, the
          ;; comint mode sees this as two different compiles and gives messages
          ;; in the order:  "Comint finished, Building the NetHack program
          ;; succeeded, Comint finished".  This is maybe a little bad as it
          ;; obscures the message that the build is done.  Still, it works for
          ;; now, so I'll just need to remember that it's currently a little
          ;; HACK-y.
          (format "PREFIX=%s make all install"
                  nethack-build-directory))
         (compilation-buffer
          (compilation-start compilation-cmd t))) ; Use compilation-shell-minor-mode

    (if (get-buffer-window compilation-buffer)
        (select-window (get-buffer-window compilation-buffer))
      (pop-to-buffer compilation buffer))
    (with-current-buffer compilation-buffer
      (setq-local compilation-error-regexp-alist nil)
      (add-hook 'compilation-finish-functions
                (lambda (_buffer _status)
                  (compilation-start compilation-cmd t)
                  (funcall callback))
                nil t)                  ; Locally add-hook
      (current-buffer))))


;;; Initialization

;;;###autoload
(defun nethack-install (&optional no-query-p
                                  no-download-p
                                  no-error-p
                                  launch-nethack-p)
  "Download, install, and patch nethack.
If the `nethack-program' is not running or does not appear to be
working, attempt to rebuild it.  If this build succeeded,
continue with the activation of the package.  Otherwise fail
silently, that is, no error is is signaled.

Build the program (if necessary) without asking first, if
NO-QUERY-P is non-nil.  Also, if NO-QUERY-P is non-nil, then
3.6.6 will be assumed to be the version to download and install.

Do not download (but do untar) if NO-DOWNLOAD-P is non-nil.

Do not signal an error in case the build failed, if NO-ERROR-P is
non-nil.

Call `nethack' upon a successful compilation if LAUNCH-NETHACK-P
is non-nil."
  (interactive)
  (unless (nethack-installed-p)
    (if (or no-query-p
            (y-or-n-p "Need to (re)build the NetHack program, do it now?"))
        (progn
          (setq-default nethack-version
                        (or (and no-query-p "3.6.6")
                            (nethack-query-for-version)))
          (nethack-build
           (lambda ()
             (let ((msg (format "Building the NetHack program %s"
                                (if (file-exists-p nethack-program)
                                    "succeeded" "failed"))))
               (if (not (file-exists-p nethack-program))
                   (funcall (if no-error-p #'message #'error) "%s" msg)
                 (message "%s" msg)
                 (when launch-nethack-p (nethack)))))
           no-download-p))
      (message "NetHack not activated"))))


;;; Process
(defvar nh-proc nil)
(defvar nh-proc-buffer-name "*nh-output*")
(defvar nh-proc-kill-buffer-on-quit t
  "When the process ends kill the process buffer if this is t.")
(defvar nh-log-buffer "*nh-log*")

;;;###autoload
(defun nethack ()
  "Start a game of Nethack.
The variable `nethack-program' is the name of the executable to run."
  (interactive)
  (if (nethack-installed-p)
      (if (nethack-is-running)
          (progn
            (message "NetHack process already running...")
            (nhapi-restore-window-configuration))
        ;; Start the process.
        (when (get-buffer nh-proc-buffer-name)
          (kill-buffer nh-proc-buffer-name))
        (nethack-start (let ((process-environment (append (when nethack-wizmode `(,(concat "NETHACKOPTIONS=@" nethack-options-file))) nethack-environment process-environment))
                             (default-directory (funcall (if (and nethack-wizmode (not (eq system-type 'windows-nt))) 'tramp-file-name-with-sudo 'identity) default-directory))
                             (nethack-program-args (append (when nethack-wizmode '("-D" "-u" "wizard")) nethack-program-args)))
                         (apply 'start-file-process "nh" nh-proc-buffer-name
                                nethack-program nethack-program-args)))))
  (nethack-install))

(defun nethack-is-running ()
  "Return T if nethack is already running."
  (and (processp nh-proc)
       (member (process-status nh-proc) '(open run))))

(defun nethack-start (process)
  "Given the process, start nethack. Assumes nethack is not already running."
  (save-excursion
    (setq nh-proc process)
    (setq nethack-options (nethack-options-parse))
    (nh-reset-status-variables)
    (set-process-filter nh-proc 'nh-filter)
    (set-process-sentinel nh-proc 'nh-sentinel)))

(defun nethack-toggle-tiles ()
  "Toggle the use of tiles on the map."
  (interactive)
  (setq nethack-use-tiles (not nethack-use-tiles))
  (nethack-command-redraw-screen 2))

;;;; Process code to communicate with the Nethack executable
(defconst nh-prompt-regexp
  "^\\(command\\|menu\\|dummy\\|direction\\|number\\|string\\)> *")

(defun nh-sentinel (proc msg)
  "Nethack background process sentinel.
PROC is the process object and MSG is the exit message."
  (with-current-buffer (process-buffer proc)
    (nh-log (buffer-substring (point-min) (point)))
    (eval-region (point-min) (point-max))
    (insert "Nethack " msg))
    ;; (when (not (string-equal msg "Nethack finished"))
    ;;    (pop-to-buffer (current-buffer)))

  (delete-process proc)
  (when nh-proc-kill-buffer-on-quit
    (kill-buffer (get-buffer nh-proc-buffer-name)))
  (when nethack-purge-buffers
    (nethack-kill-buffers))
  (let ((raw-print-buffer (get-buffer nh-raw-print-buffer-name)))
    (when raw-print-buffer
      (switch-to-buffer raw-print-buffer))))

(defvar nh-log-process-text t)
(defun nh-log (string)
  (when nh-log-process-text
    (with-current-buffer (get-buffer-create nh-log-buffer)
      (goto-char (point-max))
      (insert string))))

(defvar nh-at-prompt nil)
(defvar nh-at-prompt-hook nil
  "Called when there is a prompt. Takes one arg: the kind of prompt. Either \"command\" or \"menu\"")
(defun nh-filter (proc string)
  "Insert contents of STRING into the buffer associated with PROC.
Evaluate the buffer contents if we are looking at a prompt and then
delete the contents, perhaps logging the text."
  ;; insert output into process buffer
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (forward-line 0)
    (when (looking-at nh-prompt-regexp)
      (let ((prompt (match-string 1)))
        (nh-log (buffer-substring (point-min) (point)))
        (save-restriction
          (narrow-to-region (point-min) (point))
          (eval-buffer))
        (cond ((or (equal prompt "command")
                   (equal prompt "menu")
                   (equal prompt "dummy"))
               ;; I Don't think we need this...
               ;; (nhapi-print-status)
               (sit-for 0)
               (setq nh-at-prompt t)
               (run-hook-with-args 'nh-at-prompt-hook prompt)))))))

(defun nh-send (form)
  (let ((command (cond
                  ((null form) "()") ; the process doesn't handle `nil'
                  ((stringp form) form)
                  (t (prin1-to-string form)))))
    (with-current-buffer (process-buffer nh-proc) (erase-buffer))
    (process-send-string nh-proc (concat command "\n"))
    (nh-log (format ";;; %s\n" command))))

(defun nh-send-and-wait (form)
  (nh-send form)
  ;; wait until we get back to a "command" prompt before returning
  (setq nh-at-prompt nil)
  (while (and (member (process-status nh-proc) '(open run))
              (not nh-at-prompt))
    (accept-process-output nh-proc)))

;;; Buffer code (aka windows in Nethack)
(defvar nh-map-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "w   " table)
    (modify-syntax-entry ?\) "w   " table)
    (modify-syntax-entry ?\[ "w   " table)
    (modify-syntax-entry ?\] "w   " table)
    ;; Although the backslash isn't strictly necessary, it's needed so that
    ;; parinfer-rust-mode won't error here
    (modify-syntax-entry ?\{ "w   " table)
    (modify-syntax-entry ?\} "w   " table)
    table)
  "Syntax table used in the Nethack map.")

(defun nh-map-mode ()
  "Major mode for the main Nethack map window.

\\{nh-map-mode-map}"
  (use-local-map nh-map-mode-map)
  (set-syntax-table nh-map-mode-syntax-table)
  (setq mode-name "NetHack Map")
  (setq major-mode 'nh-map-mode)
  ;; make scroll-other-window work on the message buffer
  (setq-local other-window-scroll-buffer nh-message-buffer)
  (setq-local scroll-conservatively 0)  ; recenter
  (setq-local scroll-margin 3)
  ;; TODO still need to figure out how to automatically scroll horizontally
  (run-hooks 'nethack-map-mode-hook))

(define-derived-mode nh-message-mode text-mode "NetHack Messages"
  "Major mode for the Nethack message window"
  (setq buffer-read-only t))
(put 'nh-message-mode 'mode-class 'special)

(define-derived-mode nh-status-mode nil "NetHack Status"
  "Major mode for the Nethack status window"
  (setq buffer-read-only t))
(put 'nh-status-mode 'mode-class 'special)

(defun nethack-kill-buffers ()
  "Kill all nethack associated buffers except the nethack process
buffer."
  (when (buffer-live-p nh-map-buffer)
    (kill-buffer nh-map-buffer))        ; Preserve window for raw-print goodbye
  (dolist (buffer (list nh-status-buffer nh-message-buffer))
    (kill-buffer buffer))
  (mapc (lambda (x) (when (buffer-live-p (cdr x))
                      (kill-buffer (cdr x))))
        nh-menu-buffer-table)
  (kill-buffer (get-buffer nh-log-buffer)))




(run-hooks 'nethack-load-hook)

(provide 'nethack)


;;; VERSION:
(defconst nethack-el-version "0.13.3")
(defconst nethack-el-earliest-compatible-version "0.12.0")
(defun nethack-el-version ()
  (interactive)
  (message (format "nethack-el %s" nethack-el-version)))

;;; nethack.el ends here
