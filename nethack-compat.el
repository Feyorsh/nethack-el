;;; nethack-compat.el --- compatibility file for various emacsen -*- lexical-binding:t -*-

;; Copyright (C) 2003,2005 Ryan Yeske and Shawn Betts

;; Author: Ryan Yeske <rcyeske@vcn.bc.ca>
;; Maintainer: George Huebner <george@feyor.sh>

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


(defvar nethack-map-buffer)
(defvar nethack-map-width)
(defvar nethack-prompt-string)
(defvar nethack-message-buffer)
(defvar nethack-message-style)
(defvar nethack-message-highlight-overlay)
(declare-function nethack-nhapi-clear-message "nethack-api")


;;; utility/compatibility functions
(defun nethack-propertize (string &rest properties)
  "Add text PROPERTIES to STRING and return the new string."
  (add-text-properties 0 (length string) properties string)
  string)

(defun nethack-assq-delete-all (key alist)
  "Delete from ALIST all elements whose car is KEY.
Return the modified alist."
  ;; this is defined in emacs21 as `assq-delete-all'.
  (let ((tail alist))
    (while tail
      (when (eq (car (car tail)) key)
        (setq alist (delq (car tail) alist)))
      (setq tail (cdr tail)))
    alist))

(defun nethack-window-buffer-height (window)
  "Return the height (in screen lines) of the buffer that WINDOW is displaying."
  (with-current-buffer (window-buffer window)
    (count-lines (point-min) (point-max))))

(defun nethack-read-line (prompt)
  (read-from-minibuffer prompt))

(defvar nethack-last-message nil
  "Contains the last message displayed by `nethack-message'.")

(defun nethack-display-message-in-map (str &optional block dont-restore-point)
  (setf nethack-last-message str)
  (with-current-buffer nethack-map-buffer
    (let ((old-pnt (point-marker)))
      (unwind-protect
           (let ((inhibit-read-only t)
                 (p (or (next-single-property-change (point-min) 'nethack-message)
                        (point-min))))
             (goto-char p)
             (when (or block
                       (and (> p (point-min))
                            (>= (+ p (length str) 1 (length " --more--")) nethack-map-width)))
               (nethack-overwrite-insert " --more--")
               (nethack-pause)
               ;; clear the line
               (delete-region (point-min) (line-end-position))
               (insert (make-string nethack-map-width 32))
               (goto-char (point-min)))
             (unless (bobp)
               (setq str (concat " " str)))
             (nethack-overwrite-insert (propertize str 'nethack-message t)))
        (unless dont-restore-point
          (goto-char old-pnt))))))

(defun nethack-message (_attr str &optional block dont-restore-point)
  (cl-case nethack-message-style
    (:map
     (nethack-display-message-in-map str block dont-restore-point))
    (t
     (with-current-buffer nethack-message-buffer
       (goto-char (point-max))
       (let ((inhibit-read-only t))
         (run-hooks 'nethack-before-print-message-hook)
         (insert str "\n"))
       ;; cover new text with highlight overlay
       (let ((start (overlay-start nethack-message-highlight-overlay)))
         (move-overlay nethack-message-highlight-overlay
                       start (point-max)))
       ;; scroll to show maximum output on all windows displaying buffer
       (let ((l (get-buffer-window-list (current-buffer))))
         (save-selected-window
           (mapc (lambda (w)
                   (select-window w)
                   (set-window-point w (- (point-max) 1))
                   (recenter -1))
                 l)))))))

(defun nethack-clear-map-message ()
  (with-current-buffer nethack-map-buffer
    (nethack-with-point
     (let ((inhibit-read-only t))
       (goto-char (point-min))
       ;; A cheap overwrite
       (delete-region (point) (line-end-position))
       (insert (make-string nethack-map-width 32))))))

(defun nethack-clear-message ()
  (cl-case nethack-message-style
    (:map
     (nethack-clear-map-message))
    (t
     (with-current-buffer nethack-message-buffer
       (move-overlay nethack-message-highlight-overlay
                     (point-max) (point-max))))))

;; XEmacs chars are not ints
(defalias 'nethack-char-to-int (if (fboundp 'char-to-int)
                              #'char-to-int
                            #'identity))


(defun nethack-read-key-sequence-vector (prompt)
  (let ((cursor-in-echo-area t))
    (read-key-sequence-vector prompt)))

(defun nethack-read-char-in-map (&optional prompt)
  (nethack-display-message-in-map prompt nil t)
  (let ((char (read-char-exclusive)))
    (nethack-clear-map-message)
    (nethack-char-to-int char)))

(defun nethack-read-char (&optional prompt)
  (let ((cursor-in-echo-area t)
        (x-stretch-cursor))
    (message prompt)
    (let ((char (read-char-exclusive)))
      (message "")
      (nethack-char-to-int char))))

(defun nethack-pause ()
  (while (not (memq (read-char-exclusive) '(32 13)))))

(defun nethack-overwrite-insert (str)
  ;; A cheap overwrite for in-map message printing
  (delete-region (point) (min (+ (point) (length str))
                              nethack-map-width))
  (insert (substring str 0 (min (length str)
                                (- nethack-map-width (point))))))

(defmacro nethack-with-point (&rest body)
  "Restore the point after running BODY."
  (let ((old-pnt (gensym)))
    `(let ((,old-pnt (point-marker)))
       (unwind-protect
            (progn
              ,@body)
         (goto-char ,old-pnt)))))

(defun nethack-gamegrid-set-cell (x y ch)
  "Like `gamegrid-set-cell', but skips `gamegrid-set-face'.
It doesn't work for certain characters in the IBMgraphics symset, and
to my untrained eye it doesn't actually seem to do anything."
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (gamegrid-cell-offset x y))
      (delete-char 1)
      (insert-char ch 1))))


(provide 'nethack-compat)
;;; nethack-compat.el ends here
