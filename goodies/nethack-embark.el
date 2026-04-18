;;; nethack-embark.el --- nethack-el integration for Embark -*- lexical-binding:t -*-

;; Copyright (C) 2026 George Huebner

;; Author: George Huebner <george@feyor.sh>
;; Maintainer: George Huebner <george@feyor.sh>
;; Created: Tue Apr 14 15:55:55 2026

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

;;; Code:

(require 'embark)
(require 'cl-lib)

(require 'nethack-api)


(defvar-local nethack-embark--item-counts nil)


(defun nethack-embark-target-inventory-item ()
  "Find a NetHack inventory item at point in a menu buffer."
  (when (and (derived-mode-p 'nethack-menu-mode)
             nethack-proc)
    (save-excursion
      (beginning-of-line)
      (when (looking-at nethack-menu-item-regexp)
        (let* ((letter (match-string-no-properties 1))
               (start (match-beginning 0))
               (end (match-end 0)))
          (cons 'nethack-item
                (cons letter (cons start end))))))))

(add-hook 'embark-target-finders #'nethack-embark-target-inventory-item)

(defun nethack-embark--command-name-item (item)
  "Name specific item"
  (unwind-protect
       (nethack--wait)
    (let ((select-menu-orig (symbol-function 'nethack-nhapi-select-menu))
          (yn-function-orig (symbol-function 'nethack-nhapi-yn-function)))
      (fset 'nethack-nhapi-select-menu
            (lambda (&rest _)
              (fset 'nethack-nhapi-select-menu select-menu-orig)
              (nethack-send-and-wait '((0 ?i -1)))))
      (fset 'nethack-nhapi-yn-function
            (lambda (&rest _)
              (fset 'nethack-nhapi-yn-function yn-function-orig)
              (nethack-send-and-wait (string-to-char item))))
      (nethack-send-and-wait "name 1"))))

(defun nethack-embark--command-drop-multiple (items)
  "Drop multiple items"
  (unwind-protect
     (nethack--wait)
    (let ((items-w-counts (mapcar (lambda (i) `(0 ,(string-to-char i) ,(or (cdr (assoc-string i nethack-embark--item-counts)) -1))) items))
          (select-menu-orig (symbol-function 'nethack-nhapi-select-menu))
          already-invoked)
      (setq nethack-embark--item-counts nil)
      (fset 'nethack-nhapi-select-menu
            (lambda (&rest _)
              (if already-invoked
                  (progn
                    (fset 'nethack-nhapi-select-menu select-menu-orig)
                    (nethack-send-and-wait items-w-counts))
                (setq already-invoked t)
                (nethack-send-and-wait '((0 ?a -1)))))))
    (nethack-send-and-wait "ddrop 1")))

(add-to-list 'embark-multitarget-actions nethack-embark--command-drop-multiple)

(define-advice nethack-completing-read-filter (:filter-return (func) "embark-count")
  (when func
    (lambda (str _pred flag)
      (let ((ret (funcall func str _pred flag)))
        (if (eq flag 'metadata)
            `(metadata
              ,@(cdr ret)
              ,(cons 'affixation-function
                     (lambda (cands)
                       (mapcar (lambda (c)
                                 (list c (if-let ((count (cdr (assoc-string c nethack-embark--item-counts))))
                                             (format #("[%d] " 0 4 (face font-lock-constant-face)) count)
                                           "")
                                       ""))
                               cands))))
          ret)))))

(defun nethack-embark--select (&optional arg)
  "Select the NetHack item at point with optional count ARG."
  (interactive "P")
  (message "Current buffer: %s" (current-buffer))
  (with-current-buffer (cond-let
                         ;; prioritize the minibuffer
                         ([win (active-minibuffer-window)] (window-buffer win))
                         ;; then check if we probably came from an embark-collect buffer
                         ([_ (with-current-buffer
                                 (window-buffer (previous-window))
                               (derived-mode-p 'embark-collect-mode))]
                          (window-buffer (previous-window)))
                         ([_ (eq (current-buffer) nethack-inventory-buffer)] (current-buffer))
                         ;; you're cooked
                         (t (current-buffer)))
    (let* ((target (car (embark--targets)))
           (acc (plist-get target :target))
           (bounds (plist-get target :bounds))
           (action-buffer (window-buffer (embark--target-window)))
           (existing (assoc-string acc nethack-embark--item-counts))
           (selected (assoc-string acc embark--selection))
           (count (and arg (prefix-numeric-value arg))))
      (when existing
        (when bounds
          (cl-loop
                for ov in (overlays-in (car bounds) (cdr bounds))
                when (overlay-get ov 'before-string)
                do (delete-overlay ov)))
        (setq nethack-embark--item-counts (assoc-delete-all acc nethack-embark--item-counts)))
      (when count
        (when bounds
          (let ((ov (make-overlay (car bounds) (cdr bounds))))
            (overlay-put ov 'before-string (format #("[%d] " 0 4 (face font-lock-constant-face)) count))))
        (add-to-list 'nethack-embark--item-counts (cons acc count)))
      ;; embark-act closes the minibuffer before we can do anything,
      ;; so we need to ensure a copy of nethack-embark--item-counts is saved
      (let ((item-counts nethack-embark--item-counts))
        (with-current-buffer action-buffer
          (setq nethack-embark--item-counts item-counts)))
      (unless (and (or existing selected) count)
        (apply #'embark--select target)))))

(add-to-list 'embark-quit-after-action '(nethack-embark--select))

(defun nethack-embark--collect-populate-item-counts ()
  (cl-loop
        for tar in tabulated-list-entries
        do (let ((str (car (elt (cadr tar) 0))))
  (remove-text-properties 0 (length str) '(line-prefix) str))))

(add-hook 'embark-collect-mode-hook #'nethack-embark--collect-populate-item-counts)


(defvar-keymap nethack-embark-item-map
  :doc "Embark keymap for NetHack items."
  :parent embark-meta-map)

(add-to-list 'embark-keymap-alist '(nethack-item . nethack-embark-item-map))

(pcase-dolist (`(,cmd . ,key) '(;;(embark-live . [?L]) ;; just use perm_invent with inventory buffer
                                ;;(embark-become . [?B]) ;; I don't really see the utility here
                                (embark-collect . [?S])
                                (embark-select . [? ])
                                (embark-act-all . [?A])))
  (keymap-set nethack-embark-item-map
              (if-let ((k (where-is-internal cmd embark-general-map t)))
                  (key-description k)
                key)
              (if (eq cmd #'embark-select)
                  #'nethack-embark--select
                cmd)))

(pcase-dolist (`(,cmd . ,key) '((nethack-embark--command-drop-multiple . "D")
                                (nethack-command-eat . "e")
                                (nethack-command-quaff . "q")
                                (nethack-command-read . "r")
                                (nethack-command-wear-armor . "W")
                                (nethack-command-wield . "w")
                                (nethack-command-put-on . "P")
                                (nethack-command-remove-single-armor . "T")
                                (nethack-command-remove-accessory . "R")
                                (nethack-command-throw . "t")
                                (nethack-command-apply . "a")
                                (nethack-command-zap-wand . "z")
                                (nethack-command-select-ammo-for-quiver . "Q")
                                (nethack-command-dip . "M-d")
                                (nethack-embark--command-name-item . "M-n")
                                (nethack-command-adjust . "M-a")))
  (when-let* ((k (where-is-internal cmd nethack-map-mode-map t)))
    (setq key (key-description k)))
  (let ((cmd-str (concat "nethack-"
                         (string-remove-prefix
                          "nethack-embark--command-"
                          (string-remove-prefix "nethack-command-" (symbol-name cmd))))))
    (keymap-set nethack-embark-item-map key `(menu-item ,cmd-str ,cmd))
    (which-key-add-keymap-based-replacements nethack-embark-item-map key (cons cmd-str cmd))
  (cl-pushnew (lambda (&rest _) (nethack--wait)) (alist-get cmd embark-pre-action-hooks))
  (cl-pushnew #'embark--restart (alist-get cmd embark-post-action-hooks))))



(provide 'nethack-embark)

;;; nethack-embark.el ends here
