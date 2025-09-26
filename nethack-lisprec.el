;;; nethack-lisprec.el --- Record and playback nethack-el sessions -*- lexical-binding:t -*-

;; Copyright (C) 2025 George Huebner

;; Author: George Huebner <george@feyor.sh>
;; Maintainer: George Huebner <george@feyor.sh>
;; Created: Mon Sep 22 15:32:05 2025

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
;; This is the lisp counterpart of nethack's ttyrec tool.
;;
;; Output from the nethack process is saved to disk and later
;; "replayed", kinda like a noninteractive ZKP.
;; `nethack-safe-eval-region' tries to guard against malicious
;; inputs but isn't foolproof---consider yourself warned!

;;; Code:

(require 'dired-aux)

(defvar nethack-proc)
(defvar nethack-purge-buffers)
(declare-function nethack-kill-buffers "nethack")
(declare-function nethack-reset-status-variables "nethack")
(declare-function nethack-safe-eval-region "nethack")
(declare-function nethack-status-mode "nethack")
(declare-function nethack-message-mode "nethack")
(declare-function nethack-map-mode "nethack")
(declare-function nethack-menu-mode "nethack")


(defcustom nethack-lisprec-record nil
  "Whether nethack sessions should be recorded to lisprec files."
  :type '(boolean)
  :group 'nethack)

(defcustom nethack-lisprec-playback-finished-hook nil
  "Functions to be called after playback of a lisprec recording.

This does not run when canceling playback."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-lisprec-autoplay-fast-forward-multiplier 1.0
  "Scalar speed to fast forward lisprec autoplay at.

There are 0.5/`nethack-lisprec-autoplay-fast-forward-multiplier'
seconds between frames while fast forwarding."
  :type '(float)
  :group 'nethack)

(defcustom nethack-lisprec-autoplay-max-delay 2.0
  "Maximum seconds to wait before advancing to the next frame."
  :type '(float)
  :group 'nethack)


(defvar nethack-lisprec--timer nil)
(defvar nethack-lisprec-autoplay--enabled nil)
(defvar nethack-lisprec-autoplay--fast-forward nil)
(defvar nethack-lisprec--seek-to nil)

(define-minor-mode nethack-lisprec-minor-mode
  "Minor mode for controlling lisprec playback."
  :keymap `((,(kbd "C-c C-,") . nethack-lisprec-autoplay-toggle)
            (,(kbd "C-c C->") . nethack-lisprec-autoplay-fast-forward)
            (,(kbd "C-c C-/") . nethack-lisprec-seek-to)))

(defun nethack-lisprec-minor-mode-on ()
  "Turn on lisprec minor mode."
  (interactive)
  (nethack-lisprec-minor-mode +1))

;;;###autoload
(define-globalized-minor-mode nethack-lisprec-global-mode nethack-lisprec-minor-mode
  nethack-lisprec-minor-mode-on
  :predicate '(nethack-status-mode nethack-map-mode nethack-message-mode nethack-menu-mode)
  :group 'nethack)

(defun nethack-lisprec-autoplay-pause ()
  "Pause lisprec playback."
  (interactive)
  (setq nethack-lisprec-autoplay--fast-forward nil)
  (when nethack-lisprec--timer
    (setq nethack-lisprec-autoplay--enabled nil)
    (timer-set-time nethack-lisprec--timer `(,most-positive-fixnum 0 0 0))))

(defun nethack-lisprec-autoplay-resume ()
  "Resumes lisprec playback starting at the next event."
  (interactive)
    (setq nethack-lisprec--seek-to nil)
    (setq nethack-lisprec-autoplay--enabled t)
    (when nethack-lisprec--timer
      ;; NOTE: changing the timer's time to "now" does not actually execute it now, but whenever the scheduler next checks it
      (timer-set-time nethack-lisprec--timer '(0 0 0 0))))

(defun nethack-lisprec-autoplay-toggle ()
  "Pauses if playback is active and resumes if it's not."
  (interactive)
  (if nethack-lisprec-autoplay--enabled
      (nethack-lisprec-autoplay-pause)
    (nethack-lisprec-autoplay-resume)))

(defun nethack-lisprec--get-end-time ()
  (if-let* (nethack-lisprec--timer
            (timer-buf (nth 3 (timer--args nethack-lisprec--timer))))
      (with-current-buffer timer-buf
        (save-excursion
          (goto-char (point-max))
          (when (search-backward-regexp " ; \\((\\(?:[0-9]+ \\)\\{3\\}[0-9]+)\\)" nil t)
            (car (read-from-string (match-string 1))))))))

(defun nethack-lisprec--parse-time (time)
  "Attempts to parse TIME into a relative timestamp.

For example, \"2 minutes 3 seconds\", \"2:03\", \"123\", and 123 are all parsed
as (0 123 0 0)."
  (or (ignore-errors
        (if-let* ((time-units (split-string time ":"))
                  ((not (= (length time-units) 1)))
                  (scale 1)
                  (accumulated-mult (lambda (n)
                                      (unwind-protect
                                           (* (string-to-number n) scale)
                                        (setq scale (* scale 60)))))
                  (seconds (apply #'+ (mapcar accumulated-mult (nreverse time-units)))))
                  (list (logand (ash seconds -32) #xffffffff) (logand seconds #xffffffff) 0 0)))
      (let ((tmp-timer (run-at-time time nil #'identity)))
        (unwind-protect
             (when tmp-timer
               (time-subtract (timer--time tmp-timer) (current-time)))
          (cancel-timer tmp-timer)))))

(defun nethack-lisprec--format-time-string (time)
  "Returns \"hours:minutes:seconds\" in relative timestamp TIME."
  (let* ((seconds (logior (ash (nth 0 time) 32) (nth 1 time)))
         (hms `(,@(and (>= seconds (* 60 60)) (floor seconds (* 60 60)))
                  ,(floor (% seconds (* 60 60)) 60)
                  ,(% seconds 60))))
    (string-trim-left (mapconcat (lambda (n) (format "%02d" n)) hms ":") "0")))

(defun nethack-lisprec-autoplay-fast-forward-toggle ()
  "Turn fast forward on/off for the current lisprec."
  (interactive)
  (when nethack-lisprec--timer
    (setq nethack-lisprec-autoplay--fast-forward (not nethack-lisprec-autoplay--fast-forward))
    (if nethack-lisprec-autoplay--fast-forward
        (progn
          (nethack-lisprec-autoplay-resume)
          (nethack-lisprec--start-next-frame))
      (nethack-lisprec-autoplay-pause))))

(defun nethack-lisprec-seek-to (time)
  "Seek to absolute TIME in current lisprec.

Does nothing if TIME is nil."
  (interactive (list (let ((end-time (nethack-lisprec--get-end-time)))
                       (unless nethack-lisprec--timer (user-error "No lisprec recording currently playing")) ;; or parsing error...
                       (catch 'done
                         (while t
                           (when-let ((prompt (format "Seek to (0-%s): " (nethack-lisprec--format-time-string end-time)))
                                      (ret (nethack-lisprec--parse-time (read-string prompt))))
                             (throw 'done ret)))))))
  (when (and nethack-lisprec--timer time)
    (if (time-less-p time (nth 2 (timer--args nethack-lisprec--timer)))
      ;; restart the recording from the beginning and seek to TIME
      (let* ((buf (nth 3 (timer--args nethack-lisprec--timer)))
             (filename (buffer-local-value 'buffer-file-name buf)))
        (nethack-lisprec-cancel)
        (setq nethack-lisprec--seek-to time)
        (nethack-lisprec-playback filename))
      (setq nethack-lisprec--seek-to time)
      (nethack-lisprec--start-next-frame))))

(defun nethack-lisprec--start-next-frame ()
  (when nethack-lisprec--timer
    (let ((func (timer--function nethack-lisprec--timer))
          (args (timer--args nethack-lisprec--timer)))
      ;; avoid double-activation
      (cancel-timer nethack-lisprec--timer)
      (apply func args))))

(defun nethack-lisprec-next-frame ()
  "Advance to the next frame of the currently active lisprec."
  (interactive)
  (nethack-lisprec-autoplay-pause)
  (nethack-lisprec--start-next-frame))

(defun nethack-lisprec-cancel ()
  "Cancel lisprec playback."
  (interactive)
  (when nethack-lisprec--timer
    (cancel-timer nethack-lisprec--timer)
    (setq nethack-lisprec--timer nil)
    (setq nethack-lisprec--seek-to nil)
    (nethack-lisprec-autoplay-pause)))

(defun nethack-lisprec--playback-loop (start end time buf)
  (with-current-buffer buf
    (save-current-buffer
      (nethack-safe-eval-region start end))
    ;; save-excursion is intentionally not used here so you can observe the lisprec buffer
    (if (search-forward-regexp " ; \\((\\(?:[0-9]+ \\)\\{3\\}[0-9]+)\\)" nil t)
        (let* ((new-time (car (read-from-string (match-string 1))))
               (time-delta (time-subtract new-time time))
               (time-delta-scaled (time-convert (* nethack-lisprec-autoplay-fast-forward-multiplier
                                                   (time-convert time-delta 'integer))
                                                'list))
               (t-max (time-convert nethack-lisprec-autoplay-max-delay 'list)))
          (goto-char (1+ (match-beginning 0)))
          (setq nethack-lisprec--timer
                (run-at-time (cond (nethack-lisprec--seek-to ;; return nil, play next frame immediately
                                      (when (time-less-p nethack-lisprec--seek-to new-time)
                                        (progn
                                          (message "Seeked to %s" (nethack-lisprec--format-time-string new-time))
                                          (setq nethack-lisprec--seek-to nil))))
                                   (nethack-lisprec-autoplay--fast-forward
                                    (time-add (current-time) (if (time-less-p time-delta-scaled t-max) time-delta-scaled t-max)))
                                   (nethack-lisprec-autoplay--enabled
                                    (time-add (current-time) (if (time-less-p time-delta t-max) time-delta t-max)))
                                   (t `(,most-positive-fixnum 0 0 0)))
                             nil #'nethack-lisprec--playback-loop end (point) new-time buf)))
      (nethack-lisprec-cancel)
      (run-hooks 'nethack-lisprec-playback-finished-hook)
      (kill-buffer)
      (when nethack-purge-buffers
        (nethack-kill-buffers)))))

;;;###autoload
(defun nethack-lisprec-playback (filename)
  "Watch a recording of a nethack-el session stored in file FILENAME."
  (interactive "f")
  (when (process-live-p nethack-proc) (user-error "Current NetHack process must end for lisprec playback to occur."))
  (when nethack-lisprec--timer (user-error "A lisprec recording is already playing."))
  (nethack-lisprec-global-mode +1)
  (unless nethack-lisprec--seek-to
    (setq nethack-lisprec-autoplay--enabled t))
  (setq nethack-proc nil)
  (nethack-kill-buffers)
  (nethack-reset-status-variables)
  (unless (or (not (string-match-p ".*\\.gz$" filename)) (and (fboundp #'zlib-available-p) (zlib-available-p)))
    (let ((default-directory (make-temp-file "lisprec" t)))
      (setq filename (dired-compress-file (expand-file-name filename)))))
  (with-current-buffer (generate-new-buffer "*nethack lisprec*")
    ;; create a new buffer even if a buffer is already visiting FILENAME
    (setq buffer-file-name filename)
    (revert-buffer t t)
    (read-only-mode 1)

    (goto-char (point-min))
    (nethack-lisprec--playback-loop (point-min) (point-min) '(0 0 0 0) (current-buffer))))



(provide 'nethack-lisprec)

;;; nethack-lisprec.el ends here
