;;; nethack-api.el --- Emacs interface the lisp window-port -*- lexical-binding:t -*-

;; Copyright (C) 2002,2003,2005  Ryan Yeske and Shawn Betts

;; Author: Ryan Yeske
;; Maintainer: George Huebner <george@feyor.sh>
;; Created: Sat Mar 18 11:24:02 2000

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
;; This file is the Lisp side of the Nethack/C <--> Emacs/Lisp
;; interface.  This is where all the work gets done.
;;
;; Originally a translation of nethack-3.3.0/doc/window.doc
;; from the nethack src package.

;;; Code:

(require 'nethack-compat)
(require 'gamegrid)
(require 'nethack-keys)
(require 'nethack-options)


(defvar nethack-status-style)
(defvar nethack-status-header-line-format)
(defvar nethack-status-mode-line-format)
(defvar nethack-status-buffer-format)
(defvar nethack-use-tiles)
(defvar nethack-version)
(defvar nethack-tile-vector)
(defvar nethack-proc)
(defvar nethack-lisprec-record)
(defvar nethack-want-completing-read)
(defvar nethack-tiles-scale)
(declare-function nethack-map-mode "nethack")
(declare-function nethack-status-mode "nethack")
(declare-function nethack-message-mode "nethack")
(declare-function nethack-send "nethack")
(declare-function nethack-send-and-wait "nethack")


;;; Buffer handling
(defvar nethack-map-buffer nil)
(defvar nethack-status-buffer nil)
(defvar nethack-message-buffer nil)
(defvar nethack-inventory-buffer nil)
(defvar nethack-menu-buffer-table nil
  "An alist of (DIGIT-ID . BUFFER) pairs.")
(defun nethack-menu-buffer (menuid)
  "Return the buffer that corresponds to the MENUID."
  (let ((buffer (cdr (assq menuid nethack-menu-buffer-table))))
    (if (buffer-live-p buffer)
        buffer
      'nobuffer)))

(defvar nethack-message-highlight-overlay nil
  "Overlay used to highlight new text in the message window.")

(defvar nethack-raw-print-buffer-name "*nhw raw-print*"
  "Buffer name for Nethack raw-print messages.")

(defvar nethack--window-configuration-before nil)

(defun nethack-nhapi-raw-print (str)
  (save-current-buffer
    (let ((buffer (get-buffer-create nethack-raw-print-buffer-name)))
      (switch-to-buffer buffer)
      (insert str "\n"))))

(defun nethack-nhapi-raw-print-bold (str)
  (nethack-nhapi-raw-print
   (nethack-propertize str 'face 'nethack-atr-bold-face)))

(defun nethack-nhapi-curs (x y)
  "Set the cursor in `nethack-map-buffer' to X, Y."
  (with-current-buffer nethack-map-buffer
    (goto-char (gamegrid-cell-offset (- x 1) y))))


;;; Status/Attribute code:b
(defun nethack-propertize-attribute (attribute form)
  "Give an ATTRIBUTE the correct faces with format string FORM.

ATTRIBUTE is a list going attribute name, value, oldvalue, percent, and age.
An attribute name is a string representing either a stat or a condition."
  (unless nethack-options-hilites
    (nethack-options-get-hilites))
  (let* ((name (nth 0 attribute))
         (new-value (nth 1 attribute))
         ;; (old-value (nth 2 attribute))
         ;; (percent (nth 3 attribute))
         ;; (age (nth 4 attribute))
         (string (format form (or new-value "")))
         (face nil))
    (when (nethack-options-set-p "statushilites")
      (setq face
            ;; TODO: Make this so that things like "up" takes precedence over
            ;; "changed" work?
            (mapcan
             (lambda (func)
               ;; feeds the function new old percent age
               (apply (copy-tree func) (cdr attribute)))
             (nethack-options-status-hilite name))))
    ;; TODO Store polymorphs?
    ;; Do not display HD unless polymorphed (it has a value)
    (if (and (string-equal name "HD")
             (equal new-value "0"))
        ""
      (if face
          (nethack-propertize string 'face face)
        string))))

;; value oldvalue percent age
(defvar nethack-status-attributes nil
  "Alist of the attributes used.

Key is a symbol, the value is a list of current, old, percent, age.")

(defvar nethack-status-conditions nil
  "Alist of the NetHack conditions.

See `nethack-status-attributes' for details on the format.")

(defun nethack-reset-status-variables ()
  (setq nethack-status-attributes '(("title" nil nil 0 0)
                               ("strength" "0" "0" 0 0)
                               ("dexterity" "0" "0" 0 0)
                               ("constitution" "0" "0" 0 0)
                               ("intelligence" "0" "0" 0 0)
                               ("wisdom" "0" "0" 0 0)
                               ("charisma" "0" "0" 0 0)
                               ("alignment" nil nil 0 0)
                               ("score" "0" "0" 0 0)
                               ("carrying-capacity" nil nil 0 0)
                               ("gold" "0" "0" 0 0)
                               ("power" "0" "0" 0 0)
                               ("power-max" "0" "0" 0 0)
                               ("experience-level" "0" "0" 0 0)
                               ("armor-class" "0" "0" 0 0)
                               ("HD" "0" "0" 0 0)
                               ("time" "0" "0" 0 0)
                               ("hunger" nil nil 0 0)
                               ("hitpoints" "0" "0" 0 0)
                               ("hitpoints-max" "0" "0" 0 0)
                               ("dungeon-level" nil nil 0 0)
                               ("experience" "0" "0" 0 0))
        nethack-status-conditions (mapcar
                              (lambda (x)
                                (cons x
                                      '(nil nil 0 0)))
                              nethack-options-cond-all)))

(defun nethack-nhapi-status-condition-update (fields)
  (let (;(new-fields (split-string fields))
        (update-field
         (lambda (field)
           (let* ((field-name (car field))
                  (old-value (cadr field)) ; current
                  (new-value (car-safe (member field-name fields))))
             (unless (equal new-value old-value)
               ;; Update oldvalue and value
               (setf (caddr field) old-value)
               (setf (cadr field) new-value)
               (setcar (cddddr field) 0)))))) ; age
    (mapc
     update-field
     nethack-status-conditions)))

(defun nethack-nhapi-status-update (field new-value percent)
  (let* ((variable (assoc field nethack-status-attributes))
         (old-value (cadr variable)))
         ;; (age (cadddr variable)))
    (unless (equal new-value old-value)
      ;; TODO should this be in the let?
      (setf (alist-get field nethack-status-attributes nil nil #'equal)
            (list new-value
                  old-value
                  percent
                  0))
      (when (not (string-equal field "T"))
        (run-hook-with-args 'nethack-status-attribute-change-functions
                            field new-value old-value percent)))))

(defun nethack-status-string (format)
  (mapconcat
   (lambda (ch)
     (let ((stat (nethack-status-char-to-format ch)))
       (cond
         ((equal stat "condition")
          (mapconcat
           (lambda (x)
             (let ((c (nethack-propertize-attribute x "%s")))
               (when (not (string-equal c ""))
                 (concat c " "))))
           nethack-status-conditions
           ""))
         ((and (string-equal (car stat) "title") (nethack-options-set-p 'hitpointbar))
          (let ((title-str (nth 1 (assoc "title" nethack-status-attributes))))
            (when-let* ((hp-attr (assoc "hitpoints" nethack-status-attributes))
                        (hp-face (car (mapcan (lambda (f)
                                                (apply (copy-tree f) (cdr hp-attr)))
                                              (nethack-options-status-hilite "hitpoints"))))
                        (hp-percent (nth 3 hp-attr)))
              (put-text-property 0 (length title-str) 'face nil title-str)
              (put-text-property 0 (floor (* (length title-str) (/ hp-percent 100.0))) 'face `(:inherit ,hp-face :inverse-video t) title-str))
            (concat "[" title-str "] ")))
         (stat
          (nethack-propertize-attribute
           (assoc (car stat) nethack-status-attributes)
           (cdr stat)))                        ; String format
         (t (char-to-string ch)))))
   format nil))

;; TODO make this part of a unified defcustom array?
(defun nethack-status-char-to-format (ch)
  "Take character CH and return the format.

If CH is the character \"f\" for \"conditions\", then the string
  \"condition\" is returned instead."
  (pcase ch
    (?n '("title" . "%s"))
    (?s '("strength" . "St:%s"))
    (?d '("dexterity" . "Dx:%s"))
    (?c '("constitution" . "Cn:%s"))
    (?i '("intelligence" . "In:%s"))
    (?W '("wisdom" . "Wi:%s"))
    (?C '("charisma" . "Ch:%s"))
    (?A '("alignment" . "%s"))
    (?S '("score" . "S:%s"))
    (?r '("carrying-capacity" . "%s"))
    ;; TODO see note above on gold
    (?g '("gold" . "%s"))
    (?p '("power" . "Pw:%s"))
    (?P '("power-max" . "(%s)"))
    (?e '("experience-level" . "Xp:%s"))
    (?a '("armor-class" . "AC:%s"))
    (?D '("HD" . "HD:%s"))
    (?t '("time" . "T:%s"))
    (?G '("hunger" . "%s"))
    (?h '("hitpoints" . "HP:%s"))
    (?H '("hitpoints-max" . "(%s)"))
    (?l '("dungeon-level" . "%s"))
    (?E '("experience" . "/%s"))
    (?f "condition")
    (?v '("version" . "\n%s"))))

;; This is called upon from the C half, so it should be prefixed
;; "nethack-nhapi-" rather than "nethack-".
(defun nethack-nhapi-print-status ()
  ;; title value oldvalue percent age
  (setq nethack-status-attributes
        (mapcar
         (lambda (attr)
           (append (butlast attr 1) (list (1+ (nth 4 attr)))))
         nethack-status-attributes))
  (setq nethack-status-conditions
        (mapcar
         (lambda (attr)
           (append (butlast attr 1) (list (1+ (nth 4 attr)))))
         nethack-status-conditions))
  (cl-case nethack-status-style
    (:header-line
     (with-current-buffer nethack-map-buffer
       (setq header-line-format
             (nethack-status-string nethack-status-header-line-format))))
    (:mode-line
     (setq mode-line-format
           (nethack-status-string nethack-status-mode-line-format)))
    (:map
     (with-current-buffer nethack-map-buffer
       (let ((p (next-single-property-change (point-min) 'nethack-status))
             (inhibit-read-only t))
         (when p
           (delete-region p (point-max)))
         (nethack-with-point
          (goto-char (point-max))
          (insert (propertize (nethack-status-string nethack-status-buffer-format) 'nethack-status t))))))
    (:buffer
     (with-current-buffer nethack-status-buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (nethack-status-string nethack-status-buffer-format)))))
    (t (user-error "unsupported nethack status style"))))



;;; Menu code:
(defun nethack-nhapi-menu-putstr (menuid attr str)
  "On buffer associated with MENUID, insert with ATTR the STR."
  (with-current-buffer (nethack-menu-buffer menuid)
    (let ((inhibit-read-only t))
      (cond (t (goto-char (point-max))
               (insert (nethack-propertize str
                                      'face
                                      (nethack-attr-face attr))
                       "\n"))))))

(defun nethack-nhapi-message (attr str)
  "Insert STR to `nethack-message-buffer' using ATTR face.
FIXME: doesnt actually use ATTR!"
  (nethack-message attr str))

(defun nethack-nhapi-message-nohistory (_attr str)
  "Display STR in echo area.

This is used when the ATR_NOHISTORY bit in a message is set."
  (message "%s" str))

(defconst nethack-colors
  [nethack-black-face nethack-red-face nethack-green-face nethack-brown-face
                      nethack-blue-face nethack-magenta-face nethack-cyan-face
                      nethack-gray-face nethack-dark-gray-face
                      nethack-orange-face nethack-bright-green-face
                      nethack-yellow-face nethack-bright-blue-face
                      nethack-bright-magenta-face nethack-bright-cyan-face
                      nethack-white-face]
  "Vector indexed by Nethack's color number.")

(defconst nethack-dec-graphics-char
  '((#x5f . #x00A0)
    (#x60 . #x25C6)
    (#x61 . #x2592)
    (#x62 . #x2409)
    (#x63 . #x240C)
    (#x64 . #x240D)
    (#x65 . #x240A)
    (#x66 . #x00B0)
    (#x67 . #x00B1)
    (#x68 . #x2424)
    (#x69 . #x240B)
    (#x6a . #x2518)
    (#x6b . #x2510)
    (#x6c . #x250C)
    (#x6d . #x2514)
    (#x6e . #x253C)
    (#x6f . #x23BA)
    (#x70 . #x23BB)
    (#x71 . #x2500)
    (#x72 . #x23BC)
    (#x73 . #x23BD)
    (#x74 . #x251C)
    (#x75 . #x2524)
    (#x76 . #x2534)
    (#x77 . #x252C)
    (#x78 . #x2502)
    (#x79 . #x2264)
    (#x7a . #x2265)
    (#x7b . #x03C0)
    (#x7c . #x2260)
    (#x7d . #x00A3)
    (#x7e . #x00B7))
  "Alist of DEC to unicode as if DEC graphics mode was on.

Values taken from
http://fileformats.archiveteam.org/wiki/DEC_Special_Graphics_Character_Set,
accessed 2021-04-23.")

;; set to nil to disable the check
(defvar nethack-nhapi-print-glyph--previous-ch t)
(defun nethack-nhapi-print-glyph (x y color glyph tile ch &optional attr)
  "Insert glyph into `nethack-map-buffer'."
  (set-buffer nethack-map-buffer)
  (setq x (- x 1))                      ; FIXME: put this hack in C
  (let ((inhibit-read-only t))
    (if (and nethack-use-tiles (display-images-p))
        (save-excursion
          (let ((buffer-read-only nil)
                (image (elt nethack-tile-vector tile)))
            (goto-char (gamegrid-cell-offset x y))
            (delete-char 1)
            (image--set-property image :scale nethack-tiles-scale)
            (insert-image image)))
      (cond
        ((or (nethack-options-set-p 'DECgraphics) (string-match-p "^DECgraphics$" (or (nethack-options-set-p 'symset) "")))
         (nethack-gamegrid-set-cell
          x y
          ;; For DECgraphics, lower-case letters with high bit set mean switch
          ;; character set and render with high bit clear; user might want 8-bits
          ;; for other characters
          (if (< (logand ch #x7f) #x60)
              ch
            (or (cdr (assq (logxor ch #x80)
                           nethack-dec-graphics-char))
                ch))))
        ((or (nethack-options-set-p 'IBMgraphics) (string-match-p "^IBMgraphics\\(?:_1\\|_2\\)?$" (or (nethack-options-set-p 'symset) "")))
         (nethack-gamegrid-set-cell x y (decode-char 'cp437 ch)))
        (t
         ;; try to infer symset by checking the character used for the horizontal wall.
         (when nethack-nhapi-print-glyph--previous-ch
           (if (eq nethack-nhapi-print-glyph--previous-ch ch)
               (when-let ((symset (pcase ch
                                    (241 "DECgraphics")
                                    (196 "IBMgraphics")
                                    (?- (setq nethack-nhapi-print-glyph--previous-ch nil))
                                    (_ nil))))
                 (setq nethack-nhapi-print-glyph--previous-ch nil)
                 (push `("symset" ,symset) nethack-options)
                 (warn "Inferred symset to be %s, please put OPTIONS=symset:%1$s in your .nethackrc" symset))
             (setq nethack-nhapi-print-glyph--previous-ch ch)))

         (nethack-gamegrid-set-cell x y ch)))
      (set-text-properties (gamegrid-cell-offset x y)
                           (1+ (gamegrid-cell-offset x y))
                           `(face
                             ,(list (aref nethack-colors color) (nethack-attr-face attr))
                             glyph
                             ,glyph)))))


(defun nethack-completing-read-filter (action)
  (let* ((polymorph nil) ;; TODO
         (all-categories '("Coins" "Weapons" "Armor" "Amulets" "Potions" "Scrolls" "Comestibles" "Spellbooks" "Rings" "Wands" "Tools" "Gems/Stones"))
         (action-to-category-preds `(("drink" . (("Potions")))
                                     ("adjust" . (,all-categories))
                                     ("use or apply" . (,all-categories))
                                     ("wear" . (("Armor") . ("being worn")))
                                     ("take off" . (("Armor") . (t "being worn")))
                                     ("drop" . (,all-categories . ("being worn")))
                                     ("dip" . (,all-categories))
                                     ("dip into" . (("Potions")))
                                     ("eat" . (,@(pcase polymorph
                                                  ('gelatinous-cube `(,all-categories))
                                                  ('metalivore `(,all-categories))
                                                  (_ '(("Comestibles"))))))
                                     ("write with" . (("Coins" "Weapons" "Miscellaneous" "Wands" "Tools" "Gems/Stones")))
                                     ("wield" . ((,@all-categories "Miscellaneous") . ("being worn" "wielded")))
                                     ("read" . (("Scrolls" "Spellbooks")))
                                     ("ready" . ((,all-categories) . ("being worn")))
                                     ("throw" . (,all-categories . ("being worn")))
                                     ("fire" . (,all-categories . ("being worn")))
                                     ("put on" . (("Amulets" "Rings" "Armor") . ("being worn")))
                                     ("remove" . (("Amulets" "Rings" "Armor") . (t "being worn")))))
         (category-predicates (cdr (assoc-string action action-to-category-preds)))
         (inventory (append (when (string= action "write with") '(("-" "your fingertip" . "Miscellaneous")))
                            (when (string= action "wield") '(("-" "your bare hands" . "Miscellaneous")))
                            nethack--inventory)))
    (lambda (str _pred flag)
      (pcase flag
        ('metadata
         (list 'metadata
               (cons 'display-sort-function #'identity)
               (cons 'annotation-function
                     (lambda (cand)
                       (format #("    %s" 4 6 (face font-lock-doc-face))
                               (substring-no-properties (cadr (assoc-string cand inventory))))))
               (cons 'group-function
                     (lambda (cand transform)
                       (if transform cand
                         (cddr (assoc-string (substring-no-properties cand 0 1) inventory)))))))
        ('t
         (if (string-blank-p str)
             (all-completions str (cl-remove-if-not
                (lambda (i)
                  (let* ((name (cadr i))
                         (category (cddr i))
                         ;; yes, if you name a weapon "(wielded)" or some dumb shit like that, this will break.
                         (name-matcher (lambda (s) (string-match-p (format "(%s)$" (regexp-quote s)) name))))
                    (and (member category (car category-predicates))
                         (if (eq (cadr category-predicates) t) ;; invert match
                             (cl-some name-matcher (cddr category-predicates))
                           (not (cl-some name-matcher (cdr category-predicates)))))))
                inventory))
           (all-completions
            str
            (lambda (s _ _)
              (mapcar
               #'car
               (cl-remove-if-not
                (lambda (i)
                  (let* ((name (cadr i))
                         (category (cddr i))
                         (name-matcher (lambda (s) (string-match-p (format "(%s)$" (regexp-quote s)) name))))
                    (when (and (member category (car category-predicates))
                         (if (eq (cadr category-predicates) t) ;; invert match
                             (cl-some name-matcher (cddr category-predicates))
                           (not (cl-some name-matcher (cdr category-predicates)))))
                    (or
                     (string-match-p (regexp-quote s) (car i))
                     (string-match-p (regexp-quote s) (cadr i))))))
                inventory))))))))))

(defun nethack-nhapi-yn-function (ques choices default)
  (if nethack-proc
      (let (key)
        ;; convert string of choices to a list of ints
        (setq choices (mapcar #'nethack-char-to-int
                              (string-to-list choices)))

        (when (/= default 0)
          (setq choices (cons default choices)))

        (if (and nethack-want-completing-read
                 (string-match "\\(What do you want to \\(dip \\(.*\\) into\\|.*\\)\\? \\)\\[" ques))
            (let* ((prompt (match-string 1 ques))
                   (action (if (match-string 3 ques) "dip into" (match-string 2 ques)))
                   (ret (condition-case _ (completing-read prompt (nethack-completing-read-filter action)) (quit "\0"))))
              (while (not (or (assoc-string ret nethack--inventory) (string= ret "\0")))
                (run-with-timer 0.01 nil (lambda () (minibuffer-message "match required")))
                (setq ret (condition-case _ (completing-read prompt (nethack-completing-read-filter action)) (quit "\0"))))
              (if (and (not (string= ret "\0")) (numberp current-prefix-arg) (> current-prefix-arg 0))
                  (let* ((inhibit-quit nil)
                         (nstr (number-to-string current-prefix-arg))
                         (msd (substring nstr 0 1))
                         (remaining-count (string-to-number (substring nstr 1))))
                    (nethack-send-and-wait (string-to-char msd))
                    (funcall (keymap-lookup nethack-map-mode-map ret) remaining-count))
                (nethack-send (string-to-char ret))))

          ;; Add some special keys of our own to the choices
          (setq choices (cons 13 choices))

          (setq key (nethack-read-char (concat ques " ")))
          (when (> (length choices) 1)
            (while (not (member key choices))
              (setq key (nethack-read-char (concat
                                            (format "(bad %d) " key)
                                            ques " ")))))
          ;; 13, 27, and 7 are abort keys
          (nethack-send (if (or (= 13 key)
                                (= 27 key)
                                (= 7 key))
                            default
                          key))))
    (message ques)))

(defun nethack-nhapi-ask-direction (prompt)
  "Prompt the user for a direction."
  (if nethack-proc
      (let ((cmd (lookup-key nethack-map-mode-map
                             (nethack-read-key-sequence-vector prompt))))
        (nethack-send
         (cond ((eq cmd #'nethack-command-north) "n")
               ((eq cmd #'nethack-command-south) "s")
               ((eq cmd #'nethack-command-west) "w")
               ((eq cmd #'nethack-command-east) "e")
               ((eq cmd #'nethack-command-northwest) "nw")
               ((eq cmd #'nethack-command-northeast) "ne")
               ((eq cmd #'nethack-command-southwest) "sw")
               ((eq cmd #'nethack-command-southeast) "se")
               ((eq cmd #'nethack-command-up) "up")
               ((eq cmd #'nethack-command-down) "down")
               ((eq cmd #'nethack-command-rest-one-move) "self")
               ((eq cmd #'nethack-command-search) "self")
               (t "nowhere"))))
    (message prompt)))

(defun nethack-nhapi-askname ()
  "Prompt the user for their name."
  (if nethack-proc
      (nethack-send (nethack-read-line "Who are you? "))
    (message "Who are you? ")))

(defun nethack-nhapi-getlin (ques)
  (if nethack-proc
      (nethack-send (condition-case nil
                        (nethack-read-line (concat ques " "))
                      (quit "")))
    (message ques)))

(defun nethack-nhapi-get-ext-cmd (cmd-alist)
  "Get an extended command from the user."
  (nethack-nhapi-choose-attribute "# " cmd-alist ""))

(defun nethack-nhapi-player-selection ()
  "Does nothing right now, perhaps simply indicates that the
nethack-nhapi-choose-X calls are to follow for actual
role/race/gender/align selection.")

(defun nethack-nhapi-choose-attribute (prompt alist abort)
  "Prompts user for an element from the cars of ALIST and returns the
corresponding cdr."
  (if nethack-proc
      (nethack-send
       (if (> (length alist) 1)
           (let ((completion-ignore-case t))
             (condition-case nil
                 (cdr (assoc (completing-read prompt alist nil t) alist))
               (quit abort)))
         (cdar alist)))
    (message prompt)))

(defvar nethack-directory nil
  "Location of the nethack directory.

This is set when the process starts by `nethack-nhapi-init-nhwindows'.
Do not edit the value of this variable.  Instead, change the value of
`nethack-program'.")

(defvar nethack-file-receive-buffer nil
  "Temporary buffer to receive a file from the Nethack process.")

(defun nethack-nhapi-display-file (str _complain)
  (if-let ((file (concat nethack-directory str))
           ((file-exists-p file)))
      (view-file file)
    (let* ((default-directory (make-temp-file "nethack-dlb" t))
           (nhdat-file (concat nethack-directory "nhdat"))
           (dlb-exe (cond ((locate-file "dlb" (list nethack-directory)))
                          ((car-safe (when (file-exists-p nethack-directory) (directory-files-recursively
                                                                              (locate-dominating-file nethack-directory (lambda (dir) (directory-files dir t "games"))) "dlb")))))))
      (unless (ignore-errors
                (call-process dlb-exe nil nil nil "xf" nhdat-file str)
                (view-file str)
                (nethack-send t))
        ;; if fetching file locally was unsuccessful, tell nethack
        ;; process to send the file
        (setq nethack-file-receive-buffer (generate-new-buffer str))
        (nethack-send nil)))))

(defun nethack-nhapi-receive-file (bytes &optional eof)
  ;; PERF: if this function ends up being a bottleneck, we could use
  ;; zlib to reduce the size of lisp messages
  (with-current-buffer nethack-file-receive-buffer
    (insert bytes))
  (when eof
    (if (string= eof "error")
        (message bytes)
      (with-current-buffer nethack-file-receive-buffer
        (if-let ((filename (buffer-file-name)))
            (progn (save-buffer) (kill-buffer)
                   (when (string= eof "error") (delete-file filename))
                   (setq nethack-options (nethack-options-parse filename)))
            (view-buffer (current-buffer))))
      (setq nethack-file-receive-buffer nil))))

(defvar nethack-inventory-need-update nil
  "If non-nil, at the next command prompt, update the menu.")
(defvar nethack--inventory nil)

(defun nethack-nhapi-update-inventory ()
  (setq nethack--inventory nil)
  (setq nethack-inventory-need-update t))

(defun nethack-nhapi-doprev-message ()
  (save-selected-window
    (save-current-buffer
      (walk-windows (lambda (w)
                      (select-window w)
                      (set-buffer (window-buffer))
                      (when (eq (current-buffer) nethack-message-buffer)
                        (scroll-down)))))))

(defun nethack-nhapi-update-positionbar (_features))

(defun nethack-nhapi-init-nhwindows (version executable &rest _args)
  "Function called by the nethack process for windowing setup."
  (if (string-match-p "lisp patch \\([0-9]*\\.[0-9]*\\.[0-9]*\\)" version)
      (setq nethack-version (match-string 1))
    (warn "Could not detect version from NetHack process"))
  (setq nethack-directory (file-name-directory executable))
  (when (and (nethack-options-set-p 'tiled_map) (null nethack-use-tiles))
    (message "You have OPTIONS=tiled_map set in your nethackrc; consider setting nethack-use-tiles"))
  (setq nethack-nhapi-print-glyph--previous-ch t)
  (setq nethack--inventory nil)
  ;; clean up old buffers
  (mapc (lambda (b) (kill-buffer (cdr b))) nethack-menu-buffer-table)
  (setq nethack-menu-buffer-table nil)
  (when (get-buffer nethack-raw-print-buffer-name)
    (kill-buffer nethack-raw-print-buffer-name)))

(defun nethack-nhapi-exit-nhwindows (str)
  "Print the message in STR to the raw print buffer."
  (nethack-nhapi-raw-print str))

(defun nethack-nhapi-create-message-window ()
  "Create the message buffer."
  (with-current-buffer (get-buffer-create "*nethack message*")
    (nethack-message-mode)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setq nethack-message-highlight-overlay
          (make-overlay (point-max) (point-max)))
    (overlay-put nethack-message-highlight-overlay
                 'face 'nethack-message-highlight-face)
    (setq nethack-message-buffer (current-buffer))))

(defun nethack-nhapi-create-status-window ()
  "Create the status buffer."
  (with-current-buffer (get-buffer-create "*nethack status*")
    (nethack-status-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq nethack-status-buffer (current-buffer)))))

(defun nethack-nhapi-create-map-window ()
  "Created the map buffer."
  (when (not (buffer-live-p nethack-map-buffer))
    (with-current-buffer (get-buffer-create "*nethack map*")
      (nethack-map-mode)
      (setq nethack-map-buffer (current-buffer)))))

(defun nethack-nhapi-create-inventory-window (menuid)
  "Create the inventory window."
  (when (not (buffer-live-p nethack-inventory-buffer))
  (with-current-buffer (nethack-nhapi-create-menu 'menu menuid)
    (rename-buffer "*nethack inventory*")
    (nethack-menu-mode nil)
    (setq buffer-read-only t)
    (setq nethack-inventory-buffer (current-buffer)))))

(defun nethack-nhapi-create-menu-window (menuid)
  "Create a menu window."
  (with-current-buffer (nethack-nhapi-create-menu 'menu menuid)
    (setq buffer-read-only t)))

(defun nethack-nhapi-create-text-window (menuid)
  "Create a text window."
  ;; text windows are treated as "pick-none" menu windows
  (nethack-nhapi-create-menu 'text menuid))

(defun nethack-nhapi-create-menu (type menuid)
  "Return a newly created buffer and add it to the menu table.

The TYPE argument is legacy and serves no real purpose."
  (let* ((name (format "*%s* %d" (symbol-name type) menuid))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables))
    (push (cons menuid buf) nethack-menu-buffer-table)
    buf))

(defun nethack-nhapi-clear-message ()
  "Move overlay off the last message in `nethack-message-buffer'."
  (nethack-clear-message))

(defconst nethack-map-width 79 "Max width of the map.")
(defconst nethack-map-height 21 "Max height of the map.")
(defun nethack-nhapi-clear-map ()
  "Clear the map."
  (with-current-buffer nethack-map-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when (and nethack-use-tiles (display-images-p))
        (require (intern (concat nethack-use-tiles "-tiles"))))
      (setq gamegrid-use-glyphs nil)  ; dont try to use gamegrid glyphs
      (let (cursor-type)              ; protect from gamegrid-init clobbering
        (gamegrid-init (make-vector 256 nil)))
      (gamegrid-init-buffer nethack-map-width
                            nethack-map-height
                            ?\ ))))

(defun nethack-nhapi-block ()
  (read-from-minibuffer "--more--")
  (nethack-send 'block-dummy))

(defvar nethack-active-menu-buffer nil)
(defvar nethack-menu-how nil
  "One of pick-one, pick-none, pick-any.")

(defun nethack-nhapi-display-menu (menuid)
  (with-current-buffer (nethack-menu-buffer menuid)
    (let ((window (and nethack-message-buffer
                       (get-buffer-window nethack-message-buffer)))
          (size (count-lines (point-min) (point-max))))
      (if (or (not window)
              (>= size (window-height window)))
          (nethack-nhapi-select-menu menuid 'pick-none)
        (nethack-nhapi-message 'atr-none
                       (buffer-substring (point-min)
                                         (- (point-max) 1)))
        (nethack-send 'dummy)))))

(defun nethack-nhapi-destroy-menu (menuid)
  (save-current-buffer
    (let ((buffer (nethack-menu-buffer menuid)))
      (if (and (= menuid 3) (nethack-options-set-p "perm_invent"))
          (delete-windows-on buffer nil) ;; end of game, delete inventory window
        (when-let ((map-window (and nethack-map-buffer
                                    (get-buffer-window nethack-map-buffer))))
          (select-window map-window)))
      (kill-buffer buffer)
      (setq nethack-menu-buffer-table
            (nethack-assq-delete-all menuid nethack-menu-buffer-table)))))

(defun nethack-menu-mode (how)
  "Major mode for Nethack menus.

\\{nethack-menu-mode-map}"
  (kill-all-local-variables)
  (setq mode-name (string-join `("NetHack Menu" ,@(when how (list (symbol-name how)))) " "))
  (setq major-mode #'nethack-menu-mode)
  (setq nethack-menu-how how)
  (use-local-map nethack-menu-mode-map)
  (run-mode-hooks 'nethack-menu-mode-hook))

(defun nethack-menu-toggle-item (&optional count)
  "Toggle the menu item that is associated with the key event that
triggered this function call, if it is a valid option.

Does nothing if this is a pick-none menu.

Automatically submits menu if this is a pick-one menu and an option
was actually toggled."
  (interactive "P")
  (unless (eq nethack-menu-how 'pick-none)
    (let ((case-fold-search nil)
          (menu-item-rx (format "^[%c] \\([-+]\\|[0-9]+\\) .+$"
                                last-command-event))
          (old-point (point)))
      (beginning-of-line)
      (if (or (re-search-forward menu-item-rx nil t)
              (re-search-backward menu-item-rx nil t))
          (let ((value (match-string 1))
                (start (match-beginning 1))
                (end (match-end 1))
                (inhibit-read-only t))
            (delete-region start end)
            (goto-char start)
            (if (and count)
                (insert (number-to-string (if (consp count)
                                              (car count)
                                            count)))
              (if (string-equal value "-")
                  (insert "+")
                (insert "-")))
            (beginning-of-line)
            (when (eq nethack-menu-how 'pick-one)
              (nethack-menu-submit)))
        (message "No such menu option: %c" last-command-event)
        (goto-char old-point)))))

(defun nethack-menu-toggle-items (&optional behavior page-only)
  "Toggle items in a pick-any menu buffer.

Items are inverted by default, and selected/deselected if behavior is
`select'/`deselect'.

Set page-only to t to only toggle visible menu items."
  (interactive)
  (when (eq nethack-menu-how 'pick-any)
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (if page-only (window-start) (point-min)))
        (while (re-search-forward nethack-menu-item-regexp (when page-only (window-end)) t)
          (let ((value (match-string 2)))
            (if (and (or (string-equal value "-")
                         (eq behavior 'select))
                     (not (eq behavior 'deselect)))
                (replace-match "+" nil nil nil 2)
              (replace-match "-" nil nil nil 2))))))))

(defun nethack-menu-select-page ()
  "Select all visible menu items."
  (interactive)
  (nethack-menu-toggle-items 'select t))

(defun nethack-menu-select-all ()
  "Select all menu items."
  (interactive)
  (nethack-menu-toggle-items 'select))

(defun nethack-menu-deselect-page ()
  "Deselect all visible menu items."
  (interactive)
  (nethack-menu-toggle-items 'deselect t))

(defun nethack-menu-deselect-all ()
  "Deselect all menu items."
  (interactive)
  (nethack-menu-toggle-items 'deselect))

(defun nethack-menu-invert-page ()
  "Invert selection of all visible menu items."
  (interactive)
  (nethack-menu-toggle-items nil t))

(defun nethack-menu-invert-all ()
  "Invert selection of all menu items."
  (interactive)
  (nethack-menu-toggle-items))

(defun nethack-menu-toggle-category (category)
  "Toggle all items under CATEGORY in a pick-any menu buffer.

If CATEGORY cannot be found, fallback to `nethack-menu-toggle-item'."
  (when (catch 'fallback
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (unless (and (eq nethack-menu-how 'pick-any)
                           (re-search-forward (format "^%s$" category) nil t))
                (throw 'fallback t))
              (beginning-of-line 2)
              (while (looking-at nethack-menu-item-regexp)
                (let ((value (match-string 2)))
                  (if (string-equal value "-")
                      (replace-match "+" nil nil nil 2)
                    (replace-match "-" nil nil nil 2)))
                (beginning-of-line 2)))))
    (nethack-menu-toggle-item)))

;; kinda useless as only menu item in coins already has accelerator $
(defun nethack-menu-toggle-coins ()
  (interactive)
  (nethack-menu-toggle-category "Coins"))

(defun nethack-menu-toggle-weapons ()
  (interactive)
  (nethack-menu-toggle-category "Weapons"))

(defun nethack-menu-toggle-armor ()
  (interactive)
  (nethack-menu-toggle-category "Armor"))

(defun nethack-menu-toggle-amulets ()
  (interactive)
  (nethack-menu-toggle-category "Amulets"))

(defun nethack-menu-toggle-scrolls ()
  (interactive)
  (nethack-menu-toggle-category "Scrolls"))

(defun nethack-menu-toggle-potions ()
  (interactive)
  (nethack-menu-toggle-category "Potions"))

(defun nethack-menu-toggle-comestibles ()
  (interactive)
  (nethack-menu-toggle-category "Comestibles"))

(defun nethack-menu-toggle-spellbooks ()
  (interactive)
  (nethack-menu-toggle-category "Spellbooks"))

(defun nethack-menu-toggle-rings ()
  (interactive)
  (nethack-menu-toggle-category "Rings"))

(defun nethack-menu-toggle-wands ()
  (interactive)
  (nethack-menu-toggle-category "Wands"))

(defun nethack-menu-toggle-tools ()
  (interactive)
  (nethack-menu-toggle-category "Tools"))

(defun nethack-menu-toggle-gems ()
  (interactive)
  (nethack-menu-toggle-category "Gems/Stones"))

(defun nethack-menu-search-toggle (regexp)
  "Toggle all menu items matching REGEXP."
  (interactive "MSearch for: ")
  (when (eq nethack-menu-how 'pick-any)
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (re-search-forward nethack-menu-item-regexp nil t)
          (let ((item (match-string 3))
                (value (match-string 2)))
            (when (string-match-p regexp item)
              (if (string-equal value "-")
                  (replace-match "+" nil nil nil 2)
                (replace-match "-" nil nil nil 2)))))))))

(defun nethack-menu-goto-next ()
  "Move to the next selectable menu item."
  (interactive)
  (let ((old-point (point)))
    (end-of-line)
    (goto-char (if (re-search-forward nethack-menu-item-regexp nil t)
                   (line-beginning-position)
                 old-point))))

(defun nethack-menu-goto-prev ()
  "Move to the previous selectable menu item."
  (interactive)
  (let ((old-point (point)))
    (beginning-of-line)
    (goto-char (if (re-search-backward nethack-menu-item-regexp nil t)
                   (line-beginning-position)
                 old-point))))

(defvar nethack-window-configuration nil)
(defun nethack-menu-submit ()
  "Submit the selected menu options to the nethack process.

Restores the window configuration what it was before the menu was
displayed."
  (interactive)
  (goto-char (point-min))
  (let ((menu-data nil) (page -1))
    (while (re-search-forward nethack-menu-item-regexp nil t)
      (let ((accelerator (string-to-char (match-string 1)))
            (value (match-string 2)))
        (when (equal accelerator ?a) (setq page (1+ page)))
        (cond ((string-equal value "+")
               (setq value -1))
              ((string-equal value "-")
               (setq value 0))
              (t (setq value (string-to-number value))))
        (when (/= value 0)
          (setq menu-data (cons (list (max page 0)
                                      (nethack-char-to-int accelerator)
                                      value)
                                menu-data)))))
    (nethack-send menu-data)
    (and (window-configuration-p nethack-window-configuration)
         (set-window-configuration nethack-window-configuration))
    (setq nethack-active-menu-buffer nil)))

(defun nethack-menu-cancel ()
  "Dismiss a menu with out making any choices."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    ;; turn off all the options
    (while (re-search-forward nethack-menu-item-regexp nil t)
      (replace-match "-" nil nil nil 1)))
  (nethack-menu-submit))

(defvar nethack-unassigned-accelerator-index 0
  "Index into `nethack-accelerator-chars' indicating the next
accelerator that will be used in unassigned menus.")

(defun nethack-nhapi-start-menu (menuid)
  (with-current-buffer (nethack-menu-buffer menuid)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; we don't turn on `nethack-menu-mode' yet, since we do not yet know
      ;; "how" this menu is going to work.
      (setq nethack-unassigned-accelerator-index 0))))

(defun nethack-specify-accelerator ()
  "Return the next accelerator from `nethack-accelerator-chars' specified
by `nethack-unassigned-accelerator-index'."
  (prog1
      (aref nethack-accelerator-chars
            nethack-unassigned-accelerator-index)
    (setq nethack-unassigned-accelerator-index
          (+ 1 nethack-unassigned-accelerator-index))))

(defun nethack-nhapi-add-menu (menuid _glyph tile accelerator _groupacc attr str preselected)
  "Create a menu item out of arguments and draw it in the menu
buffer."
  (with-current-buffer (nethack-menu-buffer menuid)
    (goto-char (point-max))
    (let ((inhibit-read-only t)
          (start (point)))
      (if (= accelerator -1)
          (insert str)
        (insert (format "%c %c " (if (eq accelerator 0)
                                     (nethack-specify-accelerator)
                                   accelerator)
                        (if preselected ?+ ?-)))
        (when (and (not (= -1 tile)) nethack-use-tiles (display-graphic-p))
          (insert-image (elt nethack-tile-vector tile))
          (insert " "))
        (insert str))
      (put-text-property start (point) 'face (nethack-attr-face attr))
      (insert-char ?\n 1 nil)
      (when (nethack-options-set-p 'menucolors)
        (nethack-options-highlight-menu))
      (when nethack-inventory-need-update
        (if (= accelerator -1)
            (push `(,str) nethack--inventory)
          (let ((start (save-excursion (search-backward str))))
            (push (cons (char-to-string accelerator) (buffer-substring start (+ start (length str)))) (cdar nethack--inventory)))))
      (run-hooks 'nethack-add-menu-hook))))

;; FIXME: xemacs propertize bug here
(defun nethack-nhapi-end-menu (window prompt)
  (with-current-buffer (nethack-menu-buffer window)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (unless (string-empty-p prompt)
        (insert prompt)
        (newline)))))

(defun nethack-nhapi-select-menu (menuid how)
  "Display the menu given by MENUID and put the buffer in
`nethack-menu-mode'.

Saves the current window configuration so that it can be restored when
the menu is dismissed."
  (let ((buffer (nethack-menu-buffer menuid)))
    (unless buffer (error "No such menuid: %d" menuid))
    (if nethack-inventory-need-update
        (progn
          (setq nethack-inventory-need-update nil)
          ;; transpose from '((CATEGORY (ACCEL . NAME))) to '((ACCEL NAME . CATEGORY))
          (setq nethack--inventory
                (reverse (mapcan
                          (lambda (category)
                            (mapcar (lambda (item)
                                      `(,(car item) ,(cdr item) . ,(car category)))
                                    (cdr category)))
                          nethack--inventory)))
          (nethack-send nil))
      (progn
        (unless nethack-active-menu-buffer
          (setq nethack-window-configuration (current-window-configuration)))
        (if (one-window-p)
            (switch-to-buffer buffer)
          ;; Use the window displaying the message buffer for the menu
          (if-let ((window (or (and nethack-inventory-buffer
                                    (get-buffer-window nethack-inventory-buffer))
                               (and nethack-message-buffer
                                    (get-buffer-window nethack-message-buffer)))))
              (progn
                (select-window window t)
                (switch-to-buffer buffer t t))
            (switch-to-buffer-other-window buffer t))
          ;; make window larger, if necessary
          (let ((bh (nethack-window-buffer-height (selected-window)))
                (wh (- (window-height) 1)))
            (when (> bh wh)
              (enlarge-window (- bh wh)))))
        (nethack-menu-mode how)
        (goto-char (point-min))
        (message "Displaying menu")
        (setq nethack-active-menu-buffer buffer)))))

(defmacro nethack-nhapi--let*-if (condition then-bindings else-bindings &rest body)
  `(if ,condition
       (let* ,then-bindings
         ,@body)
     (let* ,else-bindings
       ,@body)))

(defun nethack-nhapi-restore-window-configuration ()
  "Layout the nethack windows according to the values
`nethack-status-window-height' and `nethack-message-window-height'."
  (unless nethack--window-configuration-before
    (setq nethack--window-configuration-before (window-state-get)))

  (set-window-dedicated-p (selected-window) nil)
  (delete-other-windows)

  (nethack-nhapi--let*-if (and nethack-use-tiles (display-graphic-p))
                          ((_ (selected-window)) ; layout similar to x11 port
                           (w-map (split-window-vertically (floor (* 0.55 (window-body-height)))))
                           (w-message (selected-window))
                           (w-inventory (when (nethack-options-set-p "perm_invent") (progn (split-window-horizontally (floor (* 0.6 (window-body-width))))))))
                          ((w-left (selected-window)) ; layout similar to curses port
                           (w-right (split-window-horizontally (floor (* 0.6 (window-width)))))
                           
                           (w-inventory (when (nethack-options-set-p "perm_invent") (progn (select-window w-right) (split-window-vertically (floor (* 0.4 (window-body-height)))))))
                           (w-map w-left)
                           (w-message w-right))

                          ;; By nethack 3.6.6, the nethack-nhapi-create-map-window and
                          ;; nethack-nhapi-create-inventory-window are called after
                          ;; nethack-nhapi-restore-window-configuration. This may be an issue within the source
                          ;; and it may be possible to patch it there, but patching it here is easier.
                          (nethack-nhapi-create-map-window)             ; We don't need an if, since these already have a check for duplicates.

                          (when (nethack-options-set-p "perm_invent")
                            (nethack-nhapi-create-inventory-window 3)
                            (set-window-buffer w-inventory nethack-inventory-buffer)
                            (set-window-dedicated-p w-inventory nil))

                          (set-window-buffer w-map nethack-map-buffer)
                          (set-window-dedicated-p w-map nil)
                          (set-window-buffer w-message nethack-message-buffer)
                          (set-window-dedicated-p w-message nil)

                          (when (eq nethack-status-style :buffer)
                            (let ((w-status (split-window-vertically (- nethack-status-window-height) w-map)))
                              (set-window-buffer w-status nethack-status-buffer)
                              (set-window-dedicated-p w-status t)
                              (window-preserve-size w-status nil t)
                              (with-current-buffer nethack-status-buffer
                                (setq mode-line-format nil))))
                          
                          (select-window w-map)))


(defun nethack-nhapi-bell ()
  "Beep at user."
  (ding))

(defun nethack-nhapi-wait-synch ()
  "Does nothing.")

(defun nethack-nhapi-delay-output ()
  "Sleep for 50ms."
  ;; This is the only way I can get the desired effect of a redisplay
  ;; with a short pause.  Unfortunatly, if a keypress occurs during an
  ;; "animation" we stop getting redisplays.
  (sit-for 0.05)
  ;; Tell process to continue
  (nethack-send 'dummy))


(defun nethack-nhapi-end ()
  (message "Goodbye.")
  (when nethack--window-configuration-before
    (window-state-put nethack--window-configuration-before)
    (setq nethack--window-configuration-before nil))
  ;; Prevent a memory leak
  (clrhash nethack-options-status-hilite-results)
  (run-hooks 'nethack-end-hook))

;; Options
(defvar nethack-options-cbreak nil)
(defvar nethack-options-dec-graphics nil)
(defvar nethack-options-echo nil)
(defvar nethack-options-ibm-graphics nil)
(defvar nethack-options-msg-history nil)
(defvar nethack-options-num-pad nil)
(defvar nethack-options-news nil)
(defvar nethack-options-window-inited nil)
(defvar nethack-options-vision-inited nil)
(defvar nethack-options-menu-tab-sep nil)
(defvar nethack-options-menu-requested nil)
(defvar nethack-options-num-pad-mode nil)
(defvar nethack-options-purge-monsters nil)
(defvar nethack-options-bouldersym nil)
(defvar nethack-options-travelcc nil)
(defvar nethack-options-sanity-check nil)
(defvar nethack-options-mon-polycontrol nil)

(defun nethack-nhapi-options (cbreak dec-graphics echo ibm-graphics msg-history
                      num-pad news window-inited vision-inited
                      menu-tab-sep menu-requested num-pad-mode
                      purge-monsters bouldersym travelcc
                      sanity-check mon-polycontrol &rest _)
  (setq nethack-options-cbreak cbreak)
  (setq nethack-options-dec-graphics dec-graphics)
  (setq nethack-options-echo echo)
  (setq nethack-options-ibm-graphics ibm-graphics)
  (setq nethack-options-msg-history msg-history)
  (setq nethack-options-num-pad num-pad)
  (setq nethack-options-news news)
  (setq nethack-options-window-inited window-inited)
  (setq nethack-options-vision-inited vision-inited)
  (setq nethack-options-menu-tab-sep menu-tab-sep)
  (setq nethack-options-menu-requested menu-requested)
  (setq nethack-options-num-pad-mode num-pad-mode)
  (setq nethack-options-purge-monsters purge-monsters)
  (setq nethack-options-bouldersym bouldersym)
  (setq nethack-options-travelcc travelcc)
  (setq nethack-options-sanity-check sanity-check)
  (setq nethack-options-mon-polycontrol mon-polycontrol))

(defun nethack-nhapi-need-options-file ()
  (setq nethack-options nil)
  (if (and (file-exists-p nethack-options-file)
           (not nethack-lisprec-record)
           ;; remote nethack sessions use the shell, so the command is longer
           nethack-proc
           (= (length (process-command nethack-proc)) 1))
      (progn
        (nethack-send nil)
        (setq nethack-options (nethack-options-parse nethack-options-file)))
    (setq nethack-file-receive-buffer (find-file-noselect (make-temp-file "nethackrc")))
    ;; nethack-options set in `nethack-nhapi-receive-file'
    (nethack-send t)))

(provide 'nethack-api)

;;; nethack-api.el ends here
