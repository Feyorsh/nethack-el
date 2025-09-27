;;; nethack-gen-tiles.el --- Convert NetHack tiles to sexps -*- lexical-binding:t -*-

;; Copyright (C) 2005 Shawn Betts, 2025 George Huebner

;; Author: George Huebner <george@feyor.sh>
;;         Shawn Betts <sabetts@vcn.bc.ca>
;; Maintainer: George Huebner <george@feyor.sh>
;; Created: Fri Sep 26 13:21:38 2025

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
;; This is a standalone script to convert NetHack's bespoke tile
;; format into sexps that generate XPM images for display in Emacs.
;;
;; This is based on gen-tiles.lisp by Shawn Betts, translated into
;; elisp to remove a dependency on CL.

;;; Code:

(require 'dired-aux)


(defconst nethack-gen-tiles-files
  (mapcar (lambda (f) (concat "win/share/" f))
          '("monsters.txt" "objects.txt" "other.txt")))

(defconst nethack-gen-tiles-grayscale-alist
  (let ((palette-labels '(?. ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
        (mapping '(0 1 17 18 19 20 27 22 23 24 25 26 21 15 13 14 14 1 17 18 19 20 27 22 23 24 25 20)))
    (cl-mapcar (lambda (k m) (cons k (elt palette-labels m))) palette-labels mapping)))

(defun nethack-gen-tiles-read-palette ()
  (let (ret)
    (while (search-forward-regexp "\\(\\S+\\) = (\\([0-9]+, [0-9]+, [0-9]+\\))" nil t)
      (push `(,(match-string 1) . ,(mapcar #'string-to-number (split-string (match-string 2) ", "))) ret))
    (nreverse ret)))

(defun nethack-gen-tiles-read-glyph ()
  (when (search-forward-regexp "#.*\n{\n\\(\\(?:.*\n\\)+?\\)}" nil t)
    (mapcar #'string-trim (split-string (match-string 1)))))

(defun nethack-gen-tiles-glyph-to-string (palette glyph)
  "Return sexp for creating an XPM of GLYPH as a string."
  (mapconcat #'identity `("  (create-image \"/* XPM */
static char *XFACE[] = {
/* width height ncolors chars_per_pixel */"
                         ,(format "\\\"16 16 %d 1\\\"," (length palette))
                         "/* colors */"
                         ,@(mapcar (lambda (c)
                                     (apply #'format "\\\"%s c #%02X%02X%02X\\\","
                                            (car c) (cdr c)))
                                     palette)
                         "/* pixels */"
                         ,@(mapcar (lambda (g)
                                     (format "\\\"%s\\\"," g))
                                   glyph)
                         "};\" 'xpm t)")
             "\n"))

(defun nethack-gen-tiles-blank-tile ()
  (let ((palette '(("A" . (0 0 0))
                   ("B" . (0 182 255))
                   ("C" . (255 108 0))
                   ("D" . (255 0 0))
                   ("E" . (0 0 255))
                   ("F" . (0 145 0))
                   ("G" . (108 255 0))
                   ("H" . (255 255 0))
                   ("I" . (255 0 255))
                   ("J" . (145 71 0))
                   ("K" . (204 121 0))
                   ("L" . (255 182 145))
                   ("M" . (71 108 108))
                   ("N" . (255 255 255))
                   ("O" . (218 218 182))
                   ("P" . (108 145 182))))
        (glyph (make-list 16 (make-string 16 ?A))))
    (concat (nethack-gen-tiles-glyph-to-string palette glyph) ")\n")))

(defun nethack-gen-tiles-glyphs (src &optional grayscale)
  (with-temp-buffer
    (insert-file-contents src)
    (let ((palette (nethack-gen-tiles-read-palette)) g ret)
      (while (setq g (nethack-gen-tiles-read-glyph))
        (when grayscale
          (setq g (mapcar (lambda (l)
                            (mapconcat (lambda (c)
                                         (char-to-string (alist-get c nethack-gen-tiles-grayscale-alist)))
                                       l))
                          g)))
        (push (nethack-gen-tiles-glyph-to-string palette g) ret))
      (nreverse ret))))


(defun nethack-gen-tiles (dir &optional variant)
  (interactive (let* ((dir (expand-file-name (read-directory-name "NetHack source directory: " nil nil t)))
                      (variant-guess (car (split-string (file-name-base (directory-file-name dir)) "-")))
                      (variant (read-string "Variant name: " variant-guess)))
                 (list dir variant)))
  (setq variant (downcase (or variant (car (split-string (file-name-base dir) "-")))))
  (let ((filename (concat variant "-tiles.el")))
    (with-temp-file filename
      (insert (mapconcat #'identity
                         `(";;; This file in auto-generated with nethack-gen-tiles.el\n"
                           "(defconst nethack-empty-tile"
                           ,(nethack-gen-tiles-blank-tile)
                           "(defconst nethack-tile-vector (vector"
                           ,@(mapcan (lambda (f) (nethack-gen-tiles-glyphs (expand-file-name f dir))) nethack-gen-tiles-files)
                           ;; monsters.txt is included twice, once in full color and again in grayscale
                           ,@(nethack-gen-tiles-glyphs (expand-file-name (car nethack-gen-tiles-files) dir) t)
                           ,(format "))\n\n(provide '%s-tiles)\n" variant))
                         "\n")))
    (delete-file (concat filename ".gz"))
    (let ((dired-compress-file-default-suffix ".gz"))
      (dired-compress-file filename))))



(provide 'nethack-gen-tiles)

;;; nethack-gen-tiles.el ends here
