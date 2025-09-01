;;; nethack-compat.el --- compatibility file for various emacsen -*- lexical-binding:t -*-

;; Copyright (C) 2003,2005 Ryan Yeske and Shawn Betts

;; Author: Ryan Yeske <rcyeske@vcn.bc.ca>
;; Keywords:

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

;; Any stupid hacks required to get this thing to compile and run on
;; different emacsen should go in this file.  The goal is to keep the
;; rest of the files free from feature or version testing, if
;; possible.

;;; Code:

(require 'cl-lib)
(require 'gamegrid)

;; overlay is "deprecated" in XEmacs, but still exists
(if (featurep 'xemacs)
    (require 'overlay))


;;; utility/compatibility functions
(defun nh-propertize (string &rest properties)
  "Add text PROPERTIES to STRING and return the new string."
  (add-text-properties 0 (length string) properties string)
  string)

(defun nh-assq-delete-all (key alist)
  "Delete from ALIST all elements whose car is KEY.
Return the modified alist."
  ;; this is defined in emacs21 as `assq-delete-all'.
  (let ((tail alist))
    (while tail
      (when (eq (car (car tail)) key)
        (setq alist (delq (car tail) alist)))
      (setq tail (cdr tail)))
    alist))

(defun nh-window-buffer-height (window)
  "Return the height (in screen lines) of the buffer that WINDOW is displaying."
  (save-excursion
    (set-buffer (window-buffer window))
    (count-lines (point-min) (point-max))))

(defvar nh-map-read-mode-map
  (let ((m (make-sparse-keymap)))
    ;; FIXME: this is arguably a gross hack. Create insert functions
    ;; for numbers, letters and punctuation.
    (dotimes (i (- 126 32))
      (define-key m (vector (+ i 32)) 'nh-map-read-self-insert))
    ;; FIXME: this'll do for now
    (define-key m (kbd "RET") 'nh-map-read-submit)
    m))

(defun nh-map-read-self-insert (prefix)
  (interactive "p")
  (self-insert-command prefix))

(defun nh-map-read-submit ()
  (interactive)
  (throw 'exit nil))

(define-minor-mode nh-map-read-mode ()
  :init-value nil
  :keymap nh-map-read-mode-map)

(defun nh-read-from-map (prompt)
  ;; block if there's a message so the user can read it.
  (when (next-single-property-change (point-min) 'nethack-message)
    (nh-display-message-in-map "" t)
    (nhapi-clear-message))
  (nh-display-message-in-map (propertize prompt 'nh-prompt t 'rear-nonsticky t))
  (with-current-buffer nh-map-buffer
    ;; setup prompt and read input
    (nh-with-point
     (let ((inhibit-read-only t)
           (local-map (current-local-map)))
       (goto-char (next-single-property-change (point-min) 'nh-prompt))
       (delete-region (point) (line-end-position))
       (nh-map-read-mode 1)
       (use-local-map nil)
       (setq buffer-read-only nil)
       (unwind-protect
           (recursive-edit)
         (setq buffer-read-only t)
         (nh-map-read-mode -1)
         (use-local-map local-map))
       ;; extract input
       (goto-char (point-min))
       (let* ((beg (next-single-property-change (point) 'nh-prompt))
              (line (buffer-substring beg (line-end-position))))
         (delete-region (point-min) (line-end-position))
         (insert (make-string nh-map-width 32))
         line)))))

(defun nh-read-line (prompt)
  (cl-case nethack-prompt-style
    (:map
     (nh-read-from-map prompt))
    (t
     (read-from-minibuffer prompt))))

(defvar nh-last-message nil
  "Contains the last message displayed by nh-message.")

(defun nh-display-message-in-map (str &optional block dont-restore-point)
  (setf nh-last-message str)
  (with-current-buffer nh-map-buffer
    (let ((old-pnt (point-marker)))
      (unwind-protect
          (let ((inhibit-read-only t)
                (p (or (next-single-property-change (point-min) 'nethack-message)
                       (point-min))))
            (goto-char p)
            (when (or block
                      (and (> p (point-min))
                           (>= (+ p (length str) 1 (length " --more--")) nh-map-width)))
              (nh-overwrite-insert " --more--")
              (nh-pause)
              ;; clear the line
              (delete-region (point-min) (line-end-position))
              (insert (make-string nh-map-width 32))
              (goto-char (point-min)))
            (unless (= (point) (point-min))
              (setq str (concat " " str)))
            (nh-overwrite-insert (propertize str 'nethack-message t)))
        (unless dont-restore-point
          (goto-char old-pnt))))))

(defun nh-message (attr str &optional block dont-restore-point)
  (cl-case nethack-message-style
    (:map
     (nh-display-message-in-map str block dont-restore-point))
    (t
     (with-current-buffer nh-message-buffer
       (goto-char (point-max))
       (let ((inhibit-read-only t))
         (run-hooks 'nethack-before-print-message-hook)
         (insert str "\n"))
       ;; cover new text with highlight overlay
       (let ((start (overlay-start nh-message-highlight-overlay)))
         (move-overlay nh-message-highlight-overlay
                       start (point-max)))
       ;; scroll to show maximum output on all windows displaying buffer
       (let ((l (get-buffer-window-list (current-buffer))))
         (save-selected-window
           (mapc (lambda (w)
                   (select-window w)
                   (set-window-point w (- (point-max) 1))
                   (recenter -1))
                 l)))))))

(defun nh-clear-map-message ()
  (with-current-buffer nh-map-buffer
    (nh-with-point
     (let ((inhibit-read-only t))
       (goto-char (point-min))
       ;; A cheap overwrite
       (delete-region (point) (line-end-position))
       (insert (make-string nh-map-width 32))))))

(defun nh-clear-message ()
  (cl-case nethack-message-style
    (:map
     (nh-clear-map-message))
    (t
     (with-current-buffer nh-message-buffer
       (move-overlay nh-message-highlight-overlay
                     (point-max) (point-max))))))

;; XEmacs chars are not ints
(defalias 'nh-char-to-int (if (fboundp 'char-to-int)
                              'char-to-int
                            'identity))


(defun nh-read-key-sequence-vector (prompt)
  (cl-case nethack-prompt-style
    (:map
     (nh-display-message-in-map prompt nil t)
     (prog1
         (read-key-sequence-vector "")
       (nhapi-clear-message)))
    (t
     (let ((cursor-in-echo-area t))
       (read-key-sequence-vector prompt)))))

(defun nh-read-char-in-map (&optional prompt)
  (nh-display-message-in-map prompt nil t)
  (let ((char (read-char-exclusive)))
    (nh-clear-map-message)
    (nh-char-to-int char)))

(defun nh-read-char (&optional prompt)
  (cl-case nethack-prompt-style
    (:map
     (nh-read-char-in-map prompt))
    (t
     (let ((cursor-in-echo-area t))
       (message prompt)
       (let ((char (read-char-exclusive)))
         (message "")
         (nh-char-to-int char))))))

(defun nh-pause ()
  (while (not (memq (read-char-exclusive) '(32 13)))))

(defun nh-overwrite-insert (str)
  ;; A cheap overwrite for in-map message printing
  (delete-region (point) (min (+ (point) (length str))
                              nh-map-width))
  (insert (substring str 0 (min (length str)
                                (- nh-map-width (point))))))

(defmacro nh-with-point (&rest body)
  "Restore the point after running body."
  (let ((old-pnt (gensym)))
    `(let ((,old-pnt (point-marker)))
       (unwind-protect
           (progn
             ,@body)
         (goto-char ,old-pnt)))))

(defun nh-gamegrid-set-cell (x y ch)
  "Like `gamegrid-set-cell', but skips `gamegrid-set-face'; it doesn't
work for certain characters in the IBMgraphics symset, and to my
untrained eye it doesn't actually seem to do anything."
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (gamegrid-cell-offset x y))
      (delete-char 1)
      (insert-char ch 1))))


(provide 'nethack-compat)
;;; nethack-compat.el ends here
