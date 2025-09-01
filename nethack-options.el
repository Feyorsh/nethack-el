;;; nethack-options.el --- Options parser for nethack-el -*- lexical-binding:t -*-

;; Copyright (C) 2020  Benjamin Yang

;; Author: Benjamin Yang <be11ng@protonmail.com>
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

;; This file helps nethack-el read a user's options file and sets the right
;; variables.  This is done in an attempt to make things a little less reliant
;; on the C half of nethack-el.

;; I feel like it would be a bad idea to replace the options file altogether the
;; way it is currently written --- a bunch of the map-drawing stuff isn't
;; abstracted away but is actually writing a character to a place.  This would
;; be bad, say, if someone wanted to make a boulder a `0` or something like
;; that.  Far easier to just look for options that might screw things up (like
;; IBM graphics) and adjust for them as necessary.  At the very least, writing
;; this lays the groundwork in case the options file does need to be read in
;; entirety.

;;; Code:

(defcustom nethack-options-file
  ;; On windows this would be ‘\%USERPROFILE%\NetHack\3.6’, but I'm too lazy to
  ;; figure out how to get this to work between OSs.
  (expand-file-name "~/.nethackrc")
  "The nethack configuration file.

The NetHack executable will be called with this in mind.  Some minimal options
parsing is also done on the Lisp-side of nethack-el."
  :type '(file)
  :group 'nethack)



(defconst nethack-options-fields-percents
  '("hitpoints" "power" "experience" "experience-level")
  "List of fields highlightable by percentages.

Note that “experience” and “experience-level” are compared from the start of the
current level to the start of the next experience level.")

(defconst nethack-options-fields-text-matches
  '("alignment" "carrying-capacity" "hunger" "dungeon-level" "title")
  "List of fields highlightable by text match.")

(defconst nethack-options-fields-characteristics
  '("strength" "dexterity" "constitution" "intelligence" "wisdom" "charisma")
  "List of status flags that match “characteristics”.
This is used by the function ‘nethack-options-equal’.")

(defconst nethack-options-fields
  (append (list "gold" "HD" "hitpoints-max" "time" "power-max" "armor-class"
                "condition" "score" "characteristics")
          nethack-options-fields-percents
          nethack-options-fields-text-matches
          nethack-options-fields-characteristics)
  "List of fields “hilite_status” can act on.")

(defconst nethack-options-cond-major-troubles
  '("Stone" "Slim" "Strngl" "FoodPois" "TermIll")
  "List of major_troubles condition flags.")

(defconst nethack-options-cond-minor-troubles
  '("Blind" "Deaf" "Stun" "Conf" "Hallu")
  "List of minor_troubles condition flags.")

(defconst nethack-options-cond-movement
  '("Lev" "Fly" "Ride")
  "List of movement-related condition flags.")

(defconst nethack-options-cond-all
  (append nethack-options-cond-major-troubles
          nethack-options-cond-minor-troubles
          nethack-options-cond-movement)
  "List of all condition flags.")

(defun nethack-options-status-field-p (field)
  (member
   (if (symbolp field)
       (symbol-name field)
     field)
   nethack-options-fields))

(defconst nethack-options-attributes
  '("none" "bold" "dim" "underline" "blink" "inverse" "normal")
  "List of allowed settable text attributes.")

;; This might be called by the C half of nethack-el, hence the name is prefixed
;; with “nh” rather than “nethack-options”
(defun nh-attr-face (attr)
  "Return the face corresponding with ATTR.
ATTR can be either a string or a symbol.  It does not matter if it is prefixed
  with “atr-” or not.  “normal” is aliased to “none” as is “underline” to
  “uline”.
However, ATTR should be a symbol.  As of 0.13.0, the C half should give a symbol
  which correctly resolves to an attribute, but this function is left for
  compatibility."
  (if (facep attr)
      attr
    (when (symbolp attr)
      (setq attr (symbol-name attr)))
    (when (string-prefix-p "atr-" attr)
      (setq attr (substring attr 4)))
    (intern-soft (concat "nethack-atr-"
                         ;; Aliases, since this function handles both the C codes
                         ;; attributes as well as those set within .nethackrc
                         (pcase attr
                           ("normal" "none")
                           ("underline" "uline")
                           (_ attr))
                         "-face"))))

(defun nethack-options-color-face (color)
  "Return the nethack face corresponding to COLOR.

COLOR can be either a string or a symbol.  Translation is automatically done
  between things like “lightgreen” and “bright-green”.  Also handles
  “no-color” as “nethack-atr-none-face”."
  (when (symbolp color)
    (setq color (symbol-name color)))
  (intern-soft (concat "nethack-"
                       ;; Aliases, since this function handles both the C codes
                       ;; coloributes as well as those set within .nethackrc
                       (pcase color
                         ((rx (seq "light" (not ?-)))
                          (concat "bright-" (substring color 5)))
                         ((rx "light-")
                          (concat "bright-" (substring color 6)))
                         ((rx (seq "bright" (not ?-)))
                          (concat "bright-" (substring color 6)))
                         ("no-color" "atr-none")
                         (_ color))
                       "-face")))

(defvar nethack-options-menucolors nil
  "The menucolors set.

Set by ‘nethack-options-get-menucolors’, which trawls through ‘nethack-options’
and looks for the prefix “\"menucolor\"”.  This means that the data structure
itself is buried in ‘nethack-options-parse-menucolor’.")

(defun nethack-options-get-menucolors ()
  "Set ‘nethack-options-menucolors’.

‘nethack-options-menucolors’ is set to a list of lists.  The “\"menucolor\"”
token is stripped away, so the first element of each element is a regexp (the
second being the attributes).

Matches if the ‘car’ of an element in ‘nethack-options’ is “\"menucolor\"”."
  (setq nethack-options-menucolors
        (seq-map #'cdr
                 (seq-filter
                  (lambda (elt)
                    (string-equal (car-safe elt) "menucolor"))
                  nethack-options))))

(defvar nethack-options-hilites nil
  "The status-hilites set.

Set by ‘nethack-options-get-hilites’, which trawls through ‘nethack-options’ and
looks for the prefix string “hilite_status”.  This means that the data structure
itself is buried in ‘nethack-options-parse-hilite-status’.")

(defun nethack-options-get-hilites ()
  "Set ‘nethack-options-hilites’.

‘nethack-options-hilites’ is set to a list of lists.  The “hilite_status” string
is stripped away, so the first element of each element is a field.  The logic
from field to field varies a little, so the second element is a list usually
containing a behavior and attributes.  Sometimes there's a third list starting
with “'else”, which contains the attributes for when the first “clause” doesn't
match.

Matches if the ‘car’ of an element in ‘nethack-options’ is “hilite_status”."
  (setq nethack-options-hilites
        (seq-map #'cdr
                 (seq-filter
                  (lambda (elt)
                    (equal (car-safe elt) "hilite_status"))
                  nethack-options))))



(defun nethack-options-parse-status-behav (behav)
  (list 'behavior
        behav))

(defun nethack-options-parse-attr (attributes)
  (cons 'attributes
        (mapcar
         (lambda (attr)
           (cons (if (member attr nethack-options-attributes)
                     'attribute
                   'color)
                 attr))
         (if (string-match-p "[&+]" attributes)
             (split-string attributes "[&+]" t "[ \t\n\r]+")
           (list attributes)))))

(defun nethack-options-attr-propertize (attributes)
  "Return a list of NetHack faces.

These faces correspond to the input of ATTRIBUTES.  ATTRIBUTES should be an
  alist, indicating whether a property is a “attribute” or a “color”.  This
  function calls on ‘nethack-options-color-face’ and ‘nh-attr-face’ to turn
  strings into actual faces."
  (when (equal (car attributes) 'attributes)
    (pop attributes))
  (mapcar
   (lambda (attr)
     (if (eq (car attr) 'attribute)
         (nh-attr-face (cdr attr))
       (nethack-options-color-face (cdr attr))))
   attributes))

(defun nethack-options-substitute-conditions (behav)
  "Turn a condition string into a list of matches.

For example, given a BEHAV of “movement”, returns a list of “lev”, “fly”, and
“ride”."
  (pcase behav
    ((or "major_troubles"
         "major")
     nethack-options-cond-major-troubles)
    ((or "minor_troubles"
         "minor")
     nethack-options-cond-minor-troubles)
    ("movement" nethack-options-cond-movement)
    ("all" nethack-options-cond-all)
    (_ (list behav))))

(defun nethack-options-parse-hilite-status (params)
  (let* ((ops (split-string params "/" t "[ \t\n\r]+"))
         (field-name (pop ops))
         (result (list field-name)))
    (while ops
      (setq result
            (cons
             (cond
              ;; Condition needs to be handled a little differently
              ((equal "condition" field-name)
               (let ((behav (pop ops))) ; (cadr ops)
                 (list (cons 'condition
                             (mapcar
                              #'downcase
                              (mapcan
                               #'nethack-options-substitute-conditions
                               (if (string-match-p "+" behav)
                                   (split-string behav "+")
                                 (list behav)))))
                       (nethack-options-parse-attr (pop ops)))))
              ((and (nethack-options-status-field-p field-name)
                    (cdr ops))          ; (cddr ops)
               (list (nethack-options-parse-status-behav (pop ops))
                     (nethack-options-parse-attr (pop ops))))
              ;; For something like: hilite_status:hitpoints/<=30%/red/normal
              ;; This also applies for hilite_status:hitpoints-max/green&normal
              (t
               (list (nethack-options-parse-status-behav 'else)
                     (nethack-options-parse-attr (pop ops)))))
             result)))
    (reverse result)))

(defun nethack-options-parse-menucolor (elem)
  "Parse a nethackrc MENUCOLOR= line.

Returns an alist entry of the options set."
  (when (string-prefix-p "MENUCOLOR=" elem)
    (setq elem (string-trim-left elem "[a-zA-Z]+=")))
  ;; In case the regexp contains “=”
  ;; TODO: Do this without using ‘reverse’
  (setq elem (reverse (split-string elem "=" t)))
  (setq attr (pop elem))
  (when (stringp elem)
    (setq elem (apply #'concat (reverse elem))))
  (setq elem (car elem))                ; There should only be 1 item
  ;; Remove quotes from either side of ‘elem’
  (setq elem (substring elem 1 -1))
  (list "menucolor"
        elem
        (nethack-options-parse-attr attr)))

(defun nethack-options-parse-option-1 (elem)
  ;; Cut out whitespace
  (setq elem (string-trim-left elem))
  ;; TODO: Make this more robust so if we also parse MENUCOLOR settings, then
  ;; the ":" in the regexp won't confuse this
  (if (string-match-p ":" elem)
      ;; TODO: Set this up so it auto parses things like "hilite_status"
      ;; The string trim regexp is copied from ‘string-trim-left’.
      (let* ((option
              (split-string elem ":" t "[ \t\n\r]+"))
             (op (car option))
             (params (cadr option)))
        (cons
         op
         (cond
          ((equal "hilite_status" op)
           (nethack-options-parse-hilite-status params))
          (t
           (list params)))))
    elem))

(defun nethack-options-parse-option (elem)
  "Parse a nethackrc OPTIONS= line.

Returns a list of the options set."
  ;; (when (string-prefix-p "OPTIONS=" elem)
  ;;   (setq elem (string-trim-left elem "[a-zA-Z]+=")))
  (mapcar
   #'nethack-options-parse-option-1
   (split-string (string-trim-left elem "[a-zA-Z]+=") "," t)))

(defun nethack-options-parse ()
  "Return a parsed list of ‘nethack-options-file’.

Maybe I should have used eieio."
  (when (file-exists-p nethack-options-file)
    (let (result elem)
      (with-temp-buffer
        (insert-file-contents nethack-options-file)
        (while (not (eobp))
          (narrow-to-region (point) (point-at-eol))
          ;; Skip blank lines and comments
          (unless (or (eobp)
                      (eq (char-after) ?#))
            (setq elem (buffer-string))
            (cond
             ;; Matches both OPTION= and OPTIONS=
             ((string-prefix-p "OPTION" elem)
              (setq result
                    (append result (nethack-options-parse-option elem))))
             ;; ((string-prefix-p "AUTOPICKUP_EXCEPTION=" elem))
             ((string-prefix-p "MENUCOLOR=" elem)
              (setq result
                    (append result (list
                                    (nethack-options-parse-menucolor elem)))))
             ;; ((string-prefix-p "BOLDER=" elem))
             ;; ((string-prefix-p "MSGTYPE=" elem))
             ;; ((string-prefix-p "BINDINGS=" elem))
             ;; ((string-prefix-p "SOUND=" elem))
             ;; I'm probably not going to process “CHOOSE” lines
             (t nil)))
          (widen)
          (forward-line 1)))
      result)))



(defun nethack-options-set-p (op)
  "Check if OP is set as a NetHack option.

OP can be either a symbol or a string.

This only really makes sense to use it on options that don't take arguments (so
don't use it to theck “pickup types”, but do check things like “showexp”).
It can check these options, though it doesn't make sense to."
  ;; There's probably a more efficient way to do this through a ‘filter’ or
  ;; something of that like but this works.
  (when (symbolp op)
    (setq op (symbol-name op)))
  (cl-assert (stringp op))
  (if-let ((val (member op nethack-options)))
      (car val)
    (cadr (assoc op nethack-options))))

(defun nethack-options-equal (a b)
  "Check if two hilite names are quivalent.

Specifically, this checks for the field “characteristics”."
  (or (and (equal a "characteristics")
           (member b nethack-options-fields-characteristics))
      (and (equal b "characteristics")
           (member a nethack-options-fields-characteristics))
      (equal a b)))

(defvar nethack-options-status-hilite-results
  (make-hash-table :test #'equal))

(defun nethack-options-status-hilite (stat)
  "Return a list of functions for a string STAT.

Each function takes a new-value, old-value, and percent, and returns a list of
faces using ‘nethack-options-attr-propertize’.

If STAT is a condition, then the figuring out which status hilites to give it
is done automatically, so “Stone” will match to “major”."
  ;; TODO actually match against characteristics
  (or (gethash stat nethack-options-status-hilite-results)
      (puthash
       stat
       (seq-remove
        #'null
        (mapcar
         (lambda (hilite)
           (let* ((hilite-name (car hilite))
                  (hilite-case1 (cadr hilite))
                  (hilite-case2 (nth 2 hilite))
                  (hilite-behavior1 (cdar hilite-case1)))
             (cond
              ;; always
              ((or (and (nethack-options-equal hilite-name "condition")
                        (member stat nethack-options-cond-all)
                        (equal 'else (car hilite-behavior1)))
                   (and (nethack-options-equal stat hilite-name)
                        (equal 'else (car hilite-behavior1))))
               `(lambda (_new _old _percent _age)
                  (nethack-options-attr-propertize
                   (quote ,(cdadr hilite-case1)))))
              ;; not condition, with second else clouse
              ((and (nethack-options-equal stat hilite-name)
                    (equal 'else (car-safe hilite-case2)))
               (nethack-options-status-function
                hilite-name
                (car hilite-behavior1)
                (nethack-options-attr-propertize
                 (cdadr hilite-case1))
                (nethack-options-attr-propertize
                 (cdadr hilite-case2))))
              ;; not condition, with second clause
              ((and (nethack-options-equal stat hilite-name)
                    (cdr-safe hilite-case2))
               `(lambda (new old percent age)
                  (funcall
                   (nethack-options-status-function
                    ,hilite-name
                    (quote ,(car hilite-behavior1))
                    (quote ,(nethack-options-attr-propertize
                             (cdadr hilite-case1)))
                    (funcall
                     (nethack-options-status-function
                      ,hilite-name
                      (quote ,(cadar hilite-case2))
                      (quote ,(nethack-options-attr-propertize
                               (cdadr hilite-case2))))
                     new old percent age))
                   new old percent age)))
              ;; not condition, with no second clause
              ((nethack-options-equal stat hilite-name)
               (nethack-options-status-function
                hilite-name
                (car hilite-behavior1)
                (nethack-options-attr-propertize
                 (cdadr hilite-case1))))
              ;; condition, with second else clause
              ((and (equal hilite-name "condition")
                    (member stat hilite-behavior1)
                    (equal 'else (car-safe hilite-case2)))
               (nethack-options-status-function
                hilite-name
                (car (member stat hilite-behavior1))
                (nethack-options-attr-propertize
                 (cdadr hilite-case1))
                (nethack-options-attr-propertize
                 (cdadr hilite-case2))))
              ;; condition, with second clause
              ((and (equal hilite-name "condition")
                    (member stat hilite-behavior1)
                    (cdr-safe hilite-case2))
               `(lambda (new old percent age)
                  (funcall
                   (nethack-options-status-function
                    ,hilite-name
                    ,(car (member stat hilite-behavior1))
                    (quote ,(nethack-options-attr-propertize
                             (cdadr hilite-case1)))
                    (funcall
                     (nethack-options-status-function
                      ,hilite-name
                      ,(car (member stat (cadar hilite-case2)))
                      (quote ,(nethack-options-attr-propertize
                               (cdadr hilite-case2)))
                      new old percent age))
                    new old percent age))))
              ;; condition, with no second clause
              ((and (equal hilite-name "condition")
                    (member stat hilite-behavior1))
               (nethack-options-status-function
                hilite-name
                (car (member stat hilite-behavior1))
                (nethack-options-attr-propertize
                 (cdadr hilite-case1))))
              (t nil))))
         nethack-options-hilites))
       nethack-options-status-hilite-results)))

(defun nethack-options-status-function (name behav attr &optional else)
  "Return a function checking for a BEHAV.

Returns a function which takes a new, old, percent, and age, and computes, based
on BEHAV, whether to return ATTR or not.

NAME should be a string of the name of the attribute.  It is used to check
  against things like ‘nethack-options-fields-percents’ if the BEHAV is parsable
  as a percent.  If it isn't, it fails quietly and treats it like a textmatch.
BEHAV should be a string representing the field to match.
ATTR and ELSE should be lists of faces.  ATTR is returned from the function if
  the condition matches.  As the name suggests, ELSE is returned from the
  funciton if the condition does not match"
  (setq percentp (and (string-suffix-p "%" behav)
                      (member name nethack-options-fields-percents)))
  (when percentp (setq behav (substring behav 0 -1)))
  (if (not (string-equal name "condition"))
      `(lambda (new old percent age)
         (setq val (or (and (quote ,percentp) (number-to-string percent)) new))
         (if (cond
              (,(string-prefix-p ">=" behav)
               (>= (string-to-number val)
                   (string-to-number ,(substring behav 2))))
              (,(string-prefix-p "<=" behav)
               (<= (string-to-number val)
                   (string-to-number ,(substring behav 2))))
              (,(string-prefix-p "<" behav)
               (< (string-to-number val)
                  (string-to-number ,(substring behav 1))))
              (,(string-prefix-p ">" behav)
               (> (string-to-number val)
                  (string-to-number ,(substring behav 1))))
              (,(string-equal "always" behav)
               t)
              ((and ,(string-equal "up" behav)
                    (<= age nethack-status-highlight-delay))
               (> (string-to-number new)
                  (string-to-number old)))
              ((and ,(string-equal "down" behav)
                    (<= age nethack-status-highlight-delay))
               (< (string-to-number new)
                  (string-to-number old)))
              ((and ,(string-equal "changed" behav)
                    (<= age nethack-status-highlight-delay))
               (not (= (string-to-number new)
                       (string-to-number old))))
              (t
               ;; works for both text match and absolute value
               (string-equal val ,behav)))
             (quote ,attr)
           (quote ,else)))
    ;; Is a condition
    `(lambda (new _old _percent _age)
       (if (string-equal ,behav new)
           (quote ,attr)
         (quote ,else)))))



(defun nethack-options-highlight-menu ()
  "Highlight a NetHack menu buffer.

Uses ‘nethack-options-menucolors’ as a source of regexps and attributes.  The
regexps are searched through first to last, meaning that the last highlight will
override all highlights before it.  A match will highlight an entire menu line
at a time."
  ;; Adapted from “nethack-example.el” by Shawn Betts <sabetts@vcn.bc.ca>.
  (unless nethack-options-menucolors
    (nethack-options-get-menucolors))
  (save-excursion
    (let (start
          (end (point)))
      (forward-line -1)
      (forward-line 0)
      ;; A mini-HACK so the option accelerator doesn't get highlighted
      (setq start (+ (point) 4))
      (mapc (lambda (x)
              (when (re-search-forward (car x) nil t)
                (put-text-property start end 'face
                                   (nethack-options-attr-propertize
                                    (cadr x)))))
            nethack-options-menucolors))))



(defvar nethack-options nil)



(provide 'nethack-options)

;;; nethack-options.el ends here
