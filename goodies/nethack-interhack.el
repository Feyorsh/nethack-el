;;; nethack-interhack.el --- Plugins to enhance the nethack-el interface -*- lexical-binding:t -*-

;; Copyright (C) 2025 George Huebner

;; Author: George Huebner <george@feyor.sh>
;; Maintainer: George Huebner <george@feyor.sh>

;; Package-Requires: ((emacs "27.1") (compat "30.1"))

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

;; Interhack is MIT licensed and is copyright (c) 2007 by Shawn M
;; Moore, Jesse Luehrs, and Jordan A Lewis.
;; These plugins may be considered cheating by some, which is why they
;; are not part of the main nethack-el distribution. This library
;; intentionally does not automatically make decisions for the player
;; to avoid meeting TNNT's definition of a bot. Have fun and please
;; do not use these to break rules set by public servers.

;;; Code:

(require 'cl-lib)
(require 'compat)


(defface nethack-interhack-hint-face '((t (:italic t :inherit font-lock-doc-face)))
  "Face for displaying prompt hints.")


(defmacro nethack-interhack--toggle-advice (symbol name how lambda-list &rest body)
  `(if (advice-member-p ,name #',symbol)
       (advice-remove #',symbol ,name)
     (define-advice ,symbol (,how ,lambda-list ,name)
       ,@body)))

(defun nethack-interhack--search-messages (regexp &optional bound from-top)
  (with-current-buffer nethack-message-buffer
    (save-excursion
      (goto-char (if from-top (point-min) (point-max)))
      (when (funcall (if from-top #'search-forward-regexp #'search-backward-regexp) regexp bound t)
        (let (ret)
          (cl-loop
                for (start end)
                on (match-data)
                by #'cddr
                do (when (and start end) (push (buffer-substring-no-properties start end) ret)))
          (reverse ret))))))

(defun nethack-interhack--get-role-race ()
  (cdr (or (nethack-interhack--search-messages "the \\([^ ]+\\) \\([^ ]+\\), welcome back to NetHack!$" 100 t)
           (nethack-interhack--search-messages "^Hello .*, welcome to NetHack!  You are a [^ ]+ [^ ]+ \\([^ ]+\\) \\([^ ]+\\)\\.$" 1000 t))))

(defun nethack-interhack--hint-message (format-string &rest args)
  (message "%s" (propertize (apply #'format format-string args) 'face 'nethack-interhack-hint-face)))



(defun nethack-interhack-floating-eye ()
  "Toggles printing floating eyes in `nethack-interhack-floating-eye-face'."
  (interactive)
  (nethack-interhack--toggle-advice
   nethack-nhapi-print-glyph "floating-eye"
   :filter-args (x y color glyph tile ch &optional attr)
   (when (and (= ch ?e) (eq attr 'nethack-blue-face))
     (setq attr 'nethack-interhack-floating-eye-face))
   (list x y color glyph tile ch attr)))


(defun nethack-interhack--mastermind-hash (tune)
  (cl-loop for note across tune
           for i downfrom 4
           sum (* (- (upcase note) ?A) (expt 7 i))))

(defsubst nethack-interhack--mastermind-unhash (hash)
  (aref mastermind-lookup-table hash))

(defun nethack-interhack--mastermind-judge (real guess)
  (cl-loop
        for rnote across real
        for gnote across guess
        if (= rnote gnote) sum 1 into blacks
        else count (cl-find gnote real) into whites
        finally return (+ (ash (or blacks 0) 3) (or whites 0))))

(defvar nethack-interhack--mastermind-guess nil) ;; what the user actually guessed
(defvar nethack-interhack--mastermind-suggested-guess nil) ;; memoized storage for nethack-interhack--mastermind-guess
(defvar nethack-interhack--mastermind-response nil) ;; # of blacks and whites from the codemaster
(defvar nethack-interhack--mastermind-possible-codes #&0"")
(defvar nethack-interhack--mastermind-lookup-table #&0"")

(defun nethack-interhack--mastermind-guess ()
  (if (and (not nethack-interhack--mastermind-guess) nethack-interhack--mastermind-possible-codes (= (bool-vector-count-population nethack-interhack--mastermind-possible-codes) (length nethack-interhack--mastermind-possible-codes)))
      "AABBC"
    (let ((blacks (car nethack-interhack--mastermind-response))
          (whites (cdr nethack-interhack--mastermind-response)))
      (when (and nethack-interhack--mastermind-possible-codes (or whites blacks))
        ;; eliminate any codes that cannot possibly be the correct answer
        (cl-loop
              with gotta-go-fast = (+ (ash (or blacks 0) 3) (or whites 0))
              for code-possible across-ref nethack-interhack--mastermind-possible-codes
              for code from 0
              if code-possible do (unless (= (nethack-interhack--mastermind-judge (nethack-interhack--mastermind-unhash code) nethack-interhack--mastermind-guess)
                                             gotta-go-fast)
                                    (setf code-possible nil)))
        ;; calculate the next guess
        (cl-loop
              for code-possible across nethack-interhack--mastermind-possible-codes
              for code from 0
              with best = (cons -1 most-positive-fixnum)
              if code-possible do (let ((ncodes-remaining (cl-loop
                                                             with ansatz = (nethack-interhack--mastermind-unhash code)
                                                             with histogram = (make-vector 41 0)
                                                             for possible across nethack-interhack--mastermind-possible-codes
                                                             for i from 0
                                                             if possible do (cl-incf (aref histogram (nethack-interhack--mastermind-judge ansatz (nethack-interhack--mastermind-unhash i))))
                                                             finally return (cl-loop for i across histogram maximize i))))
                                 (when (< ncodes-remaining (cdr best))
                                   (setf best (cons code ncodes-remaining))))
              finally return (nethack-interhack--mastermind-unhash (car best)))))))

(defun nethack-interhack-mastermind ()
  "Provides a GTO default input for the drawbridge tune."
  (interactive)
  (setq nethack-interhack--mastermind-guess nil)
  (setq nethack-interhack--mastermind-suggested-guess nil)
  (setq nethack-interhack--mastermind-response (cons nil nil))
  (setq nethack-interhack--mastermind-possible-codes (make-bool-vector (expt 7 5) t))
  (with-memoization nethack-interhack--mastermind-lookup-table
    (let ((v (make-vector (expt 7 5) "")))
      (cl-loop
            for code across-ref v
            for i from 0
            do (setf code (nethack-interhack--mastermind-unhash i)))
      v))

  (nethack-interhack--toggle-advice
   nethack-nhapi-message "mastermind-response"
   :after (_attr str)
   (when (string-match "You hear .*\\([[:digit:]]\\) gears? turn\\." str)
     (setf (car nethack-interhack--mastermind-response) (string-to-number (match-string 1 str))))
   (when (string-match "You hear .*\\([[:digit:]]\\) tumblers? click.*\\." str)
     (setf (cdr nethack-interhack--mastermind-response) (string-to-number (match-string 1 str))))
   (when (string-match "You \\(?:dream that you see\\|sense\\|see\\) a drawbridge \\(?:going\\|coming\\) down!" str)
     (setq nethack-interhack--mastermind-possible-codes nil)))

  (nethack-interhack--toggle-advice
   nethack-nhapi-getlin "mastermind"
   :around (fn ques &optional initial)
   (if-let (((string-match-p "What tune are you playing\\? \\[5 notes, A-G\\]" ques))
            (suggested-guess (with-memoization nethack-interhack--mastermind-suggested-guess (nethack-interhack--mastermind-guess))))
       (let ((response (condition-case nil (nethack-read-line (concat ques " " (propertize (format "\nHint: %s" suggested-guess) 'face 'nethack-interhack--hint-face)) initial) (quit "\033"))))
         (when (string-match-p "[a-h]\\{5\\}" response)
           (setq nethack-interhack--mastermind-guess (replace-regexp-in-string "h" "b" response))
           (setq nethack-interhack--mastermind-suggested-guess nil)
           (setq nethack-interhack--mastermind-response (cons nil nil)))
         (funcall (if nethack-proc #'nethack-send #'message) response))
     (funcall fn ques initial))))


(defun nethack-interhack-donate ()
  "Automatically calculate the optimal donation for priests."
  (nethack-interhack--toggle-advice
   nethack-nhapi-getlin "common-wishes"
   :before (prompt &optional _initial)
   (when (string-match-p "How much will you offer\\?" prompt)
     (let ((lvl (string-to-number (cadr (assoc-string "experience-level" nethack-status-attributes)))))
       (run-at-time 0.1 nil (lambda () (nethack-interhack--hint-message "Hint: %d gold" (* 400 lvl))))))))


(defun nethack-interhack-vault-guard ()
  "Provide a sensible default name to give to the vault guard."
  (interactive)
  (nethack-interhack--toggle-advice
   nethack-nhapi-getlin "vault-guard"
   :filter-args (prompt &optional initial)
   (when (string-match-p "\\(?:You are required to supply your name\\. -\\|\"Hello stranger, who are you\\?\"\\)" prompt)
     ;; unfortunately there's no way to check if croesus is dead if loading a savefile; caveat emptor.
     (setq initial (if (nethack-interhack--search-messages "Killed croesus")
                       (car (split-string (cadr (assoc-string "title" nethack-status-attributes))))
                     "Croesus")))
   (list prompt initial)))


(defun nethack-interhack-wand-identify ()
  "Print a message explaining the effect of zapping an engraving."
  (nethack-interhack--toggle-advice
   nethack-nhapi-message "wand-identify"
   :after (_attr str)
   (pcase str
     ((rx "The engraving now reads: \".*?\"") (nethack-interhack--hint-message "Hint: This is a wand of polymorph"))
     ((rx "The wand unsuccessfully fights your attempt to write!") (nethack-interhack--hint-message "Hint: This is a wand of striking"))
     ((rx "A few ice cubes drop from the wand\\.") (nethack-interhack--hint-message "Hint: This is a wand of cold"))
     ((rx "The \\w+ is riddled by bullet holes!") (nethack-interhack--hint-message "Hint: This is a wand of magic missile"))
     ((rx "The bugs on the \\w+ slow down!") (nethack-interhack--hint-message "Hint: This is a wand of slow monster"))
     ((rx "The bugs on the \\w+ speed up!") (nethack-interhack--hint-message "Hint: This is a wand of speed monster"))
     ((and (rx "The engraving on the \\w+ vanishes!") (guard (nethack-interhack--search-messages "A few ice cubes drop from the wand\\." 100))) (nethack-interhack--hint-message "Hint: This is either a wand of cancellation, teleportation, or make invisible."))
     ((rx "The bugs on the \\w+ stop moving!") (nethack-interhack--hint-message "Hint: This is either a wand of sleep or death")))))

(defvar nethack-interhack--ring-identify-debounce nil)

(defun nethack-interhack-ring-identify ()
  "Print a message explaining what ring was dropped into a sink."
  (nethack-interhack--toggle-advice
   nethack-nhapi-message "wand-identify"
   :after (_attr str)
   (pcase str
     ;; only print the hint for the first item to vanish from the sink
     ((and (guard (not nethack-interhack--ring-identify-debounce)) (rx "Suddenly, \\w+ vanishes from the sink!")) (nethack-interhack--hint-message "Hint: That was a ring of hunger") (setq nethack-interhack--ring-identify-debounce t) (run-at-time 0.1 nil (lambda () (setq nethack-interhack--ring-identify-debounce nil))))
     ((rx "The faucets flash brightly for a moment\\.") (nethack-interhack--hint-message "Hint: That was a ring of adornment"))
     ((rx "The sink glows \\(?:silver|black\\) for a moment\\.") (nethack-interhack--hint-message "Hint: That was a ring of protection"))
     ((rx "The sink looks nothing like a fountain\\.") (nethack-interhack--hint-message "Hint: That was a ring of protection from shape changers"))
     ((rx "The sink seems to blend into the floor for a moment\\.") (nethack-interhack--hint-message "Hint: That was a ring of stealth"))
     ((rx "The water flow seems fixed\\.") (nethack-interhack--hint-message "Hint: That was a ring of sustain ability"))
     ((rx "The sink glows white for a moment\\.") (nethack-interhack--hint-message "Hint: That was a ring of warning"))
     ((rx "Several flies buzz around the sink\\.") (nethack-interhack--hint-message "Hint: That was a meat ring"))
     ((rx "Several flies buzz angrily around the sink\\.") (nethack-interhack--hint-message "Hint: That was a ring of aggravate monster"))
     ((rx "You hear loud noises coming from the drain") (nethack-interhack--hint-message "Hint: That was a ring of conflict"))
     ((rx "The cold water faucet flashes brightly for a moment\\.") (nethack-interhack--hint-message "Hint: That was a ring of cold resistance"))
     ((rx "The water flow seems \\(?:greater\\|lesser\\) now\\.") (nethack-interhack--hint-message "Hint: That was a ring of gain constitution"))
     ((rx "The water flow seems \\(?:stronger\\|weaker\\) now\\.") (nethack-interhack--hint-message "Hint: That was a ring of gain strength"))
     ((rx "The water flow \\(?:hits\\|misses\\) the drain\\.") (nethack-interhack--hint-message "Hint: That was a ring of increase accuracy"))
     ((rx "The water's force seems \\(?:greater\\|smaller\\) now\\.") (nethack-interhack--hint-message "Hint: That was a ring of increase damage"))
     ((rx "You don't see anything happen to the sink\\.") (nethack-interhack--hint-message "Hint: That was a ring of invisibility"))
     ((rx "You smell rotten \\w+\\.") (nethack-interhack--hint-message "Hint: That was a ring of poison resistance"))
     ((rx "You see some air in the sink\\.") (nethack-interhack--hint-message "Hint: That was a ring of see invisible"))
     ((rx "Static electricity surrounds the sink\\.") (nethack-interhack--hint-message "Hint: That was a ring of shock resistance"))
     ((rx "The hot water faucet flashes brightly for a moment\\.") (nethack-interhack--hint-message "Hint: That was a ring of fire resistance"))
     ((rx "You see the ring slide right down the drain!") (nethack-interhack--hint-message "Hint: That was a ring of free action"))
     ((rx "The sink quivers upward for a moment\\.") (nethack-interhack--hint-message "Hint: That was a ring of levitation"))
     ((rx "The sink looks as good as new\\.") (nethack-interhack--hint-message "Hint: That was a ring of regeneration"))
     ((rx "You thought your \\w+ got lost in the sink, but there it is!") (nethack-interhack--hint-message "Hint: This is a ring of searching"))
     ((rx "The ring is regurgitated!") (nethack-interhack--hint-message "Hint: This is a ring of slow digestion"))
     ((rx "The sink momentarily vanishes\\.") (nethack-interhack--hint-message "Hint: That was a ring of teleportation"))
     ((rx "The sink looks like it is being beamed aboard somewhere\\.") (nethack-interhack--hint-message "Hint: That was a ring of teleport control"))
     ((rx "The sink momentarily looks like a \\w+\\.") (nethack-interhack--hint-message "Hint: That was a ring of polymorph"))
     ((rx "The sink momentarily looks like a regularly erupting geyser\\.") (nethack-interhack--hint-message "Hint: That was a ring of polymorph control")))))


(defun nethack-interhack--randomized-item-type (str)
  (let* ((randomized-appearances '(("scroll" . ("ZELGO MER" "JUYED AWK YACC" "NR 9" "XIXAXA XOXAXA XUXAXA" "PRATYAVAYAH" "DAIYEN FOOELS" "LEP GEX VEN ZEA" "PRIRUTSENIE" "ELBIB YLOH" "VERR YED HORRE" "VENZAR BORGAVVE" "THARR" "YUM YUM" "KERNOD WEL" "ELAM EBOW" "DUAM XNAHT" "ANDOVA BEGARIN" "KIRJE" "VE FORBRYDERNE" "HACKEM MUCHE" "VELOX NEB" "FOOBIE BLETCH" "TEMOV" "GARVEN DEH" "READ ME" "ETAOIN SHRDLU" "LOREM IPSUM" "FNORD" "KO BATE" "ABRA KA DABRA" "ASHPD SODALG" "MAPIRO MAHAMA DIROMAT" "GNIK SISI VLE" "HAPAX LEGOMENON" "EIRIS SAZUN IDISI" "PHOL ENDE WODAN" "GHOTI" "ZLORFIK" "VAS CORP BET MANI" "STRC PRST SKRZ KRK" "XOR OTA"))
                                   ("potion" . ("ruby" "pink" "orange" "yellow" "emerald" "dark green" "cyan" "sky blue" "brilliant blue" "magenta" "purple-red" "puce" "milky" "swirly" "bubbly" "smoky" "cloudy" "effervescent" "black" "golden" "brown" "fizzy" "dark" "white" "murky"))
                                   ("spellbook" . ("parchment" "vellum" "ragged" "dog eared" "mottled" "stained" "cloth" "leather" "white" "pink" "red" "orange" "yellow" "velvet" "light green" "dark green" "turquoise" "cyan" "light blue" "dark blue" "indigo" "magenta" "purple" "violet" "tan" "plaid" "light brown" "dark brown" "gray" "wrinkled" "dusty" "bronze" "copper" "silver" "gold" "glittering" "shining" "dull" "thin" "thick"))
                                   ("ring" . ("pearl" "iron" "twisted" "steel" "wire" "engagement" "shiny" "bronze" "brass" "copper" "silver" "gold" "wooden" "granite" "opal" "clay" "coral" "black onyx" "moonstone" "tiger eye" "jade" "agate" "topaz" "sapphire" "ruby" "diamond" "ivory" "emerald"))
                                   ("amulet" . ("circular" "spherical" "oval" "triangular" "pyramidal" "square" "concave" "hexagonal" "octagonal"))
                                   ("wand" . ("aluminum" "balsa" "brass" "copper" "crystal" "curved" "ebony" "forked" "glass" "hexagonal" "iridium" "iron" "jeweled" "long" "maple" "marble" "oak" "pine" "platinum" "runed" "short" "silver" "spiked" "steel" "tin" "uranium" "zinc"))
                                   ("helm" . ("plumed helmet" "etched helmet" "crested helmet" "visored helmet"))
                                   ("cloak" . ("tattered cape" "ornamental cope" "opera cloak" "piece of cloth"))
                                   ("gloves" . ("old gloves" "padded gloves" "riding gloves" "fencing gloves"))
                                   ("boots" . ("mud boots" "snow boots" "riding boots" "buckled boots" "hiking boots" "combat boots" "jungle boots")))))
    (catch 'done
      (cl-loop
            for (type . appearances)
            in randomized-appearances
            do (when (cl-some (lambda (s) (string-match-p (concat ".*" (regexp-quote s) ".*") str)) appearances)
                 (throw 'done type))))))

(defun nethack-interhack--price-table (item-type)
  ;; this needs to happen at runtime, not when we add the advice
  (let* ((37-p (string= nethack-version "5.0.0"))
         (price-tables `(("scroll" . ((20 . ("identify"))
                                      (50 . ("light"))
                                      (60 . ("enchant weapon"))
                                      (80 . ("enchant armor" "remove curse"))
                                      (100 . ("confuse monster" "destroy armor" "fire" "food detection" "gold detection" "magic mapping" "scare monster" "teleportation"))
                                      (200 . ("amnesia" "create monster" "earth" "taming"))
                                      (300 . ("charging" "genocide" "punishment" "stinking cloud"))))
                         ("potion" . ((20 . (,@(when 37-p '("healing"))))
                                      (50 . ("fruit juice" "booze" "see invisible" "sickness"))
                                      (100 . (,@(unless 37-p '("healing")) "confusion" "extra healing" "hallucination" "restore ability" "sleeping"))
                                      (150 . ("blindness" "gain energy" "invisibility" "monster detection" "object detection"))
                                      (200 . ("enlightenment" "full healing" "levitation" "polymorph" "speed"))
                                      (250 . ("acid" "oil"))
                                      (300 . ("gain ability" "gain level" "paralysis"))))
                         ("spellbook" . ((100 . (,@(when 37-p '("confuse monster")) "force bolt" "protection" "detect monsters" "light" "sleep" "jumping" "healing" "knock"))
                                         (200 . (,@(when 37-p '("chain lightning")) ,@(unless 37-p '("confuse monster")) "magic missile" "drain life" "create monster" "detect food" "slow monster" "cure blindness" "wizard lock"))
                                         (300 . (,@(unless 37-p '("charm monster")) "remove curse" "clairvoyance" "detect unseen" "identify" "cause fear" "haste self" "cure sickness" "extra healing" "stone to flesh"))
                                         (400 . ("cone of cold" "fireball" "detect treasure" "invisibility" "levitation" "restore ability"))
                                         (500 . (,@(when 37-p '("charm monster")) "magic mapping" "dig"))
                                         (600 . ("create familiar" "turn undead" "teleport away" "polymorph"))
                                         (700 . ("finger of death" "cancellation"))))
                         ("ring" . ((100 . ("adornment" "hunger" "protection" "protection from shape changers" "stealth" "sustain ability" "warning"))
                                    (150 . ("aggravate monster" "cold resistance" "gain constitution" "gain strength" "increase accuracy" "increase damage" "invisibility" "poison resistance" "see invisible" "shock resistance"))
                                    (200 . ("fire resistance" "free action" "levitation" "regeneration" "searching" "slow digestion" "teleportation"))
                                    (300 . ("conflict" "polymorph" "polymorph control" "teleport control"))))
                         ("amulet" . nil) ;; all amulets cost the same, they can't be price id'd
                         ("wand" . ((100 . ("light" "nothing"))
                                    (150 . ("digging" "enlightenment" "locking" "magic missile" "make invisible" "opening" "probing" "secret door detection" "slow monster" "speed monster" "striking" "undead turning"))
                                    (175 . ("cold" "fire" "lightning" "sleep"))
                                    (200 . ("cancellation" "create monster" "polymorph" "teleportation"))
                                    (500 . ("death" "wishing"))))
                         ("helm" . ((50 . (,@(if 37-p '("caution") '("brilliance")) "opposite alignment" "telepathy"))))
                         ("cloak" . ((50 . ("displacement" "protection"))
                                     (60 . ("invisibility" "magic resistance"))))
                         ("gloves" . ((50 . ("fumbling" "dexterity" "power"))))
                         ("boots" . ((8 . ("elven boots" "kicking boots"))
                                     (30 . ("fumble boots" "levitation boots"))
                                     (50 . ("jumping boots" "speed boots" "water walking boots")))))))
    (cdr (assoc-string item-type price-tables))))

(defun nethack-interhack--sucker-p ()
  (or (and (string= (nth 1 (nethack-interhack--search-messages "the [^ ]+ \\([^ ]+\\), welcome back to NetHack!$" 100 t)) "tourist")
           (ignore-errors (< (string-to-number (cadr (assoc-string "experience-level" nethack-status-attributes))) 15)))
      (cl-some (lambda (i)
                 (string-match-p ".* dunce cap .*(being worn)" (cadr i)))
               nethack--inventory)
      (and (cl-some (lambda (i)
                      (string-match-p ".*shirt .*(being worn)" (cadr i)))
                    nethack--inventory)
           (cl-loop for (nil item . category) in nethack--inventory count
                 (and (string= category "Armor")
                      (string-match-p ".* (being worn)$" item))))))

(defun nethack-interhack-price-identification ()
  "Automatically price identify items in shops."
  (interactive)
  (nethack-interhack--toggle-advice
   nethack-nhapi-message "buy-price-identification"
   :after (_attr str)
   (when-let* (((string-match "^You see here an? \\(.*\\) (for sale, \\([[:digit:]]+\\) zorkmids?)\\.$" str))
               (price (string-to-number (match-string 2 str)))
               (item (match-string 1 str))
               (type (nethack-interhack--randomized-item-type item)))
     (let* ((price-table (nethack-interhack--price-table type))
            (charisma (string-to-number (cadr (assoc-string "charisma" nethack-status-attributes))))
            (charisma-modifiers '((0 . (2 . 1)) (6 . (3 . 2)) (8 . (4 . 3)) (11 . (1 . 1)) (16 . (3 . 4)) (18 . (2 . 3)) (19 . (1 . 2)) (100 . nil)))
            (charisma-markup (cl-loop with mkp = nil for (lvl . mod) in charisma-modifiers until (< charisma lvl) finally return mkp do (setq mkp mod)))
            ;; no markup, sucker XOR unidentified markup, sucker AND unidentified markup
            (markups `(,charisma-markup (4 . 3) ,@(when (nethack-interhack--sucker-p) '((4 . 3)))))
            (enchant-surcharges (if (member type '("boots" "gloves" "cloak" "helm" "ring")) '(("" . 0) ("+1 " . 10) ("+2 " . 20)) '(("" . 0))))
            (possible-items))
       (cl-loop
             with numerator = price
             with denominator = 1
             for (prefix . enchant-surcharge) in enchant-surcharges do
             (cl-loop for (denom . num) in markups do ;; take the inverse
                   (push (mapcar (lambda (e) (concat prefix e)) (alist-get (- (round (setq numerator (* numerator num)) (setq denominator (* denominator denom))) enchant-surcharge) price-table)) possible-items)))
       (nethack-interhack--hint-message "\nPossible %s: %s" type (string-join (mapcan #'identity possible-items) ", ")))))

  (nethack-interhack--toggle-advice
   nethack-nhapi-yn-function "sell-price-identification"
   :before (ques _choices _default) ;; we want this to echo after the prompt appears, but before the user makes a selection
   (when-let* (((string-match "^\\w+ offers \\([[:digit:]]+\\) gold pieces? for your \\(.*\\)\\.  Sell it\\?" ques))
               (item (match-string 2 ques))
               (price (string-to-number (match-string 1 ques)))
               (type (nethack-interhack--randomized-item-type item)))
     (let* ((price-table (nethack-interhack--price-table type))
            (markups `(,(if (nethack-interhack--sucker-p) '(1 . 3) '(1 . 2)) (3 . 4)))
            (possible-items))
       (cl-loop
             with numerator = price
             with denominator = 1
             for (denom . num) in markups do ;; take the inverse
             (push (alist-get (round (setq numerator (* numerator num)) (setq denominator (* denominator denom))) price-table) possible-items))
       (run-at-time 0.2 nil (lambda () (nethack-interhack--hint-message "\nPossible %s: %s" type (string-join (mapcan #'identity possible-items) ", "))))))))


(defun nethack-interhack-common-wishes ()
  "Provide a list of common items to wish for."
  (interactive)
  (nethack-interhack--toggle-advice
   nethack-nhapi-getlin "common-wishes"
   :around (fn prompt &optional initial)
   (if (string-match-p "For what do you wish\\?" prompt)
       (let* ((alignment (cadr (assoc-string "alignment" nethack-status-attributes)))
              (role (nth 1 (nethack-interhack--search-messages "the [^ ]+ \\([^ ]+\\), welcome back to NetHack!$" 100 t)))
              (bfg "blessed fixed greased ")
              (bfge "blessed fixed greased +3 ")
              (wishes `(("2 blessed scrolls of charging" . "Scrolls")
                        ("2 blessed scrolls of genocide" . "Scrolls")
                        (,(concat bfg "spellbook of identify") . "Spellbooks")
                        (,(concat bfg "spellbook of magic mapping") . "Spellbooks")
                        (,(concat bfge "gray dragon scale mail") . "Armor")
                        (,(concat bfge "silver dragon scale mail") . "Armor")
                        (,(concat bfge "cloak of magic resistance") . "Armor")
                        (,(concat bfge "speed boots") . "Armor")
                        (,(concat bfge "jumping boots") . "Armor")
                        (,(concat bfge "gauntlets of power") . "Armor")
                        (,(concat bfge "helm of brilliance") . "Armor")
                        (,(concat bfge "helm of telepathy") . "Armor")
                        (,(concat bfge "helm of opposite alignment") . "Armor")
                        (,(concat bfg "ring of conflict") . "Rings")
                        (,(concat bfg "ring of levitation") . "Rings")
                        (,(concat bfg "ring of teleport-control") . "Rings")
                        (,(concat bfge "Grayswandir") . "Weapons")
                        (,(concat bfg "cursed potions of gain level") . "Potions")
                        (,(concat bfg "amulet of life saving") . "Amulets")
                        ("uncursed fixed greased magic marker" . "Tools")
                        ("7 blessed fixed greased candles" . "Tools")
                        (,(concat bfg "bag of holding") . "Tools")
                        (,(concat bfg "tooled horn") . "Tools")

                        ,@(when (string= alignment "Lawful")
                            (mapcar (lambda (i) (cons (propertize (car i) 'face 'bold-italic) (cdr i)))
                                    `(,@(when (not (string-match-p "cave\\(wo\\)?man" role)) `((,(concat bfge "Sceptre of Might") . "Weapons")))
                                        ,@(when (not (string= "knight" role)) `((,(concat bfg "Magic Mirror of Merlin") . "Tools")))
                                        ,@(when (not (string= "archeologist" role)) `((,(concat bfg "Orb of detection") . "Tools"))))))
                        ,@(when (string= alignment "Neutral")
                            (mapcan (lambda (i) (cons (propertize (car i) 'face 'bold-italic) (cdr i)))
                                    `(,@(when (not (string= "wizard" role)) `((,(concat bfg "Eye of the Aethiopica") . "Tools")))
                                        ,@(when (not (string= "valkyrie" role)) `((,(concat bfg "Orb of Fate") . "Tools")))
                                        ,@(when (not (string= "monk" role)) `((,(concat bfg "Eyes of the Overworld") . "Tools")))
                                        ,@(when (not (string= "tourist" role)) `((,(concat bfg "Platinum Yendorian Express Card") . "Tools"))))))
                        ,@(when (string= alignment "Chaotic")
                            (mapcar (lambda (i) (cons (propertize (car i) 'face 'bold-italic) (cdr i)))
                                    `(,@(when (not (string= "rogue" role)) `((,(concat bfg "Master Key of Thievery") . "Tools"))))))))
              (completion-extra-properties (list :group-function
                                                 (lambda (cand transform)
                                                   (if transform cand
                                                     (cdr (assoc-string cand wishes))))))
              (ret))
         (while (or (not ret)
                    (and (string= ret "\033") (not (yes-or-no-p "Cancel wish?"))))
           (setq ret (condition-case _ (completing-read (concat prompt " ") wishes nil 'confirm-after-completion) (quit "\033"))))
         (funcall (if nethack-proc #'nethack-send #'message) ret))
     (funcall fn prompt initial))))


(defun nethack-interhack-common-genocides ()
  "Provide a list of common monsters/classes to genocide."
  (interactive)
  (nethack-interhack--toggle-advice
   nethack-nhapi-getlin "common-genocides"
   :around (fn prompt &optional initial)
   (if (string-match "What \\(type\\|class\\) of monsters? do you want to genocide\\?" prompt)
       (let* ((genocide-type (match-string 1 prompt))
              (self (nethack-interhack--search-messages "the \\([^ ]+\\) \\([^ ]+\\), welcome back to NetHack!$" 100 t))
              (monster-genocides '(("Mind flayers" . #("h" 0 1 (face nethack-magenta-face)))
                                   ("Master mind flayers" . #("h" 0 1 (face nethack-magenta-face)))
                                   ("Disenchanters" . #("R" 0 1 (face nethack-blue-face)))
                                   ("Rust monsters" . #("R" 0 1 (face nethack-blue-face)))
                                   ("Chameleons" . #(":" 0 1 (face nethack-brown-face)))
                                   ("Doppelgangers" . #("@" 0 1 (face nethack-white-face)))
                                   ("Gelatinous cubes" . #("b" 0 1 (face nethack-cyan-face)))
                                   ("Shocking spheres" . #("e" 0 1 (face nethack-blue-face)))
                                   ("Gremlins" . #("g" 0 1 (face nethack-green-face)))))
              (reverse-genocides '(("Wraiths" . #("W" 0 1 (face nethack-dark-gray-face)))
                                   ("Nurses" . #("@" 0 1 (face nethack-white-face)))
                                   ("Gray dragons" . #("D" 0 1 (face nethack-gray-face)))
                                   ("Silver dragons" . #("D" 0 1 (face nethack-bright-cyan-face)))
                                   ("Tengu" . #("i" 0 1 (face nethack-cyan-face)))
                                   ("Unicorns" . #("u" 0 1 (face nethack-white-face)))
                                   ("Cockatrices" . #("c" 0 1 (face nethack-yellow-face)))
                                   ("Giants" . #("H" 0 1 (face nethack-red-face)))
                                   ("Purple worms" . #("w" 0 1 (face nethack-magenta-face)))))
              (class-genocides '(("Liches" . #("L" 0 1 (face nethack-brown-face)))
                                 ("Humanoids" . #("h" 0 1 (face nethack-magenta-face)))
                                 ("Sea monsters" . #(";" 0 1 (face nethack-brown-face)))
                                 ("Rust monster or disenchanters" . #("R" 0 1 (face nethack-blue-face)))
                                 ("Eye or spheres" . #("e" 0 1 (face nethack-white-face)))
                                 ("Cockatrices" . #("c" 0 1 (face nethack-yellow-face)))))
              (stupid-genocides `(,(nth 2 self)
                                   ,@(when (or (string= (nth 1 self) "human") (string= (nth 1 self) "elven"))
                                       '("humans?"
                                         "el\\(?:f\\|ves\\|ven\\)"
                                         "doppelgangers?"
                                         "soldiers?"
                                         "sergeants?"
                                         "lieutenants?"
                                         "\\(?:watch \\)?captains?"
                                         "watchm\\(?:e\\|a\\)n"
                                         "nurses?"
                                         "archeologists?"
                                         "barbarians?"
                                         "cavem\\(a\\|e\\)n?"
                                         "healers?"
                                         "knights?"
                                         "monks?"
                                         "priests?"
                                         "rangers?"
                                         "rogues?"
                                         "samurais?"
                                         "tourists?"
                                         "valkyries?"
                                         "wizards?"
                                         "shopkeepers?"
                                         "guards?"
                                         "prisoners?"
                                         "oracles?"
                                         "priest\\(?:ess\\)?e?s?"
                                         "clerics?"
                                         "medusas?"
                                         "wizard of yendors?"
                                         "croesus\\(?:es\\)?"
                                         "charons?"
                                         "lord carnarvon"
                                         "pelias"
                                         "shaman karnov"
                                         "earendil"
                                         "elwing"
                                         "hippocrates"
                                         "king arthur"
                                         "grand master"
                                         "orion"
                                         "master of thieves"
                                         "lord sato"
                                         "twoflower"
                                         "norn"
                                         "neferet the green"
                                         "thoth amon"
                                         "master kaen"
                                         "master assassin"
                                         "ashikaga takauji"
                                         "dark one"
                                         "students?"
                                         "chieftains?"
                                         "neanderthals?"
                                         "attendants?"
                                         "pages?"
                                         "abbots?"
                                         "acolytes?"
                                         "hunters?"
                                         "thugs?"
                                         "ninjas?"
                                         "roshis?"
                                         "guides?"
                                         "warriors?"
                                         "apprentices?"))
                                   ,@(when (string= (nth 1 self) "dwarven")
                                       '("dwarfs?"
                                         "hobbits?"
                                         "bugbears?"
                                         "mind flayers?"))
                                   ,@(when (string= (nth 1 self) "gnomish")
                                       '("gnomes?"))
                                   ,@(when (string= (nth 1 self) "orcish")
                                       '("\\(?:hob\\)?goblins?"
                                         "orcs?"
                                         "uruk[- ]hais?"))))
              (candidates (if (string= genocide-type "class")
                              class-genocides
                            (append monster-genocides reverse-genocides)))
              (longest-candidate (apply #'max (mapcar (lambda (c) (length (car c))) candidates)))
              (completion-extra-properties `(:annotation-function
                                             ,(lambda (cand)
                                                (format "%s  %s"
                                                        (make-string (- longest-candidate (length cand)) ? )
                                                        (cdr (assoc-string cand candidates))))
                                             ,@(when (string= genocide-type "type")
                                                 (list :group-function
                                                       (lambda (cand transform)
                                                         (if transform cand
                                                           (if (assoc-string cand reverse-genocides)
                                                               "Reverse" "Normal")))))))
              (ret))
         (while (or (not ret)
                    (and (cl-some (lambda (r) (string-match-p r ret)) stupid-genocides) (not (yes-or-no-p "This will probably kill you. Continue?")))
                    (and (string= ret "\033") (not (yes-or-no-p "Cancel genocide?"))))
           (setq ret (condition-case _ (completing-read (concat prompt " ") candidates nil 'confirm-after-completion) (quit "\033"))))
         (funcall (if nethack-proc #'nethack-send #'message) ret))
     (funcall fn prompt initial))))


(defmacro nethack-interhack--special-spell (role str)
  ;; THIS IS UNHYGENIC
  (let ((s (copy-sequence str)))
    (add-face-text-property 0 (length s) 'underline t s)
    `(if (string-match-p ,role (nth 1 self)) ,s ,str)))

(defmacro nethack-interhack--emergency-spell (str)
  ;; THIS IS UNHYGENIC
  `(let ((s (copy-sequence ,str)))
     (add-face-text-property 0 (length s)
                             (pcase (nth 1 self)
                               ((rx "Healer") 'nethack-bright-green-face)
                               ((rx "Knight") 'nethack-green-face)
                               ((rx "Monk") 'nethack-green-face)
                               ((rx "Priest\\(ess\\)") 'nethack-green-face)
                               ((rx "Ranger") 'nethack-red-face)
                               ((rx "Tourist") 'nethack-brown-face)
                               ((rx "Valkyrie") 'nethack-green-face))
                             t s)
     s))

(defun nethack-interhack-magic-marker ()
  "Provide an annotated completing read interface for magic markers."
  (interactive)
  (nethack-interhack--toggle-advice
   nethack-nhapi-getlin "magic-marker"
   :around (fn prompt &optional initial)
   (if (string-match "What type of \\(scroll\\|spellbook\\) do you want to write\\?" prompt)
       (let* ((spell-type (match-string 1 prompt))
              (self (nethack-interhack--get-role-race))
              (spells `(("scroll" . (("Mail" . (0 . ""))
                                     ("Amnesia" . (1 . ""))
                                     ("Earth" . (1 . ""))
                                     ("Fire" . (1 . ""))
                                     ("Gold detection" . (1 . ""))
                                     ("Food detection" . (1 . ""))
                                     ("Light" . (1 . ""))
                                     ("Magic mapping" . (1 . ""))
                                     ("Create monster" . (2 . ""))
                                     ("Destroy armor" . (2 . ""))
                                     ("Punishment" . (2 . ""))
                                     ("Confuse monster" . (3 . ""))
                                     ("Identify" . (4 . ""))
                                     ("Charging" . (5 . ""))
                                     ("Enchant armor" . (5 . ""))
                                     ("Enchant weapon" . (5 . ""))
                                     ("Remove curse" . (5 . ""))
                                     ("Scare monster" . (6 . ""))
                                     ("Stinking cloud" . (6 . ""))
                                     ("Taming" . (6 . ""))
                                     ("Teleportation" . (6 . ""))
                                     ("Genocide" . (7 . ""))))
                        ("spellbook" . (("Novel" . (1 . ""))
                                        ("Detect monsters" . (1 . "Divination"))
                                        ("Force bolt" . (1 . "Attack"))
                                        (,(nethack-interhack--emergency-spell "Healing") . (1 . "Healing"))
                                        ("Jumping" . (1 . "Escape"))
                                        ("Knock" . (1 . "Matter"))
                                        ("Light" . (1 . "Divination"))
                                        ("Protection" . (1 . "Clerical"))
                                        ("Sleep" . (1 . "Enchantment"))
                                        ("Confuse monster" . (2 . "Enchantment"))
                                        ("Create monster" . (2 . "Clerical"))
                                        (,(nethack-interhack--emergency-spell "Cure blindness") . (2 . "Healing"))
                                        ("Detect food" . (2 . "Divination"))
                                        ("Drain life" . (2 . "Attack"))
                                        (,(nethack-interhack--special-spell "Wizard" "Magic missile") . (2 . "Attack"))
                                        ("Slow monster" . (2 . "Enchantment"))
                                        ("Wizard lock" . (2 . "Matter"))
                                        ("Cause fear" . (3 . "Enchantment"))
                                        (,(nethack-interhack--special-spell "Tourist" "Charm monster") . (3 . "Enchantment"))
                                        (,(nethack-interhack--special-spell "Samurai" "Clairvoyance") . (3 . "Divination"))
                                        (,(nethack-interhack--emergency-spell (nethack-interhack--special-spell "Healer" "Cure sickness")) . (3 . "Healing"))
                                        ("Detect unseen" . (3 . "Divination"))
                                        (,(nethack-interhack--emergency-spell "Extra healing") . (3 . "Healing"))
                                        (,(nethack-interhack--special-spell "Barbarian" "Haste self") . (3 . "Escape"))
                                        ("Identify" . (3 . "Divination"))
                                        (,(nethack-interhack--emergency-spell (nethack-interhack--special-spell "Priest\\(?:ess\\)" "Remove curse")) . (3 . "Clerical"))
                                        ("Stone to flesh" . (3 . "Healing"))
                                        (,(nethack-interhack--special-spell "Valkyrie" "Cone of cold") . (4 . "Attack"))
                                        (,(nethack-interhack--special-spell "Rogue" "Detect treasure") . (4 . "Divination"))
                                        ("Fireball" . (4 . "Attack"))
                                        (,(nethack-interhack--special-spell "Ranger" "Invisibility") . (4 . "Escape"))
                                        ("Levitation" . (4 . "Escape"))
                                        (,(nethack-interhack--emergency-spell (nethack-interhack--special-spell "Monk" "Restore ability")) . (4 . "Healing"))
                                        (,(nethack-interhack--special-spell "Cave\\(?:wo\\)man" "Dig") . (5 . "Matter"))
                                        (,(nethack-interhack--special-spell "Archeologist" "Magic mapping") . (5 . "Divination"))
                                        ("Create familiar" . (6 . "Clerical"))
                                        ("Polymorph" . (6 . "Matter"))
                                        ("Teleport away" . (6 . "Escape"))
                                        (,(nethack-interhack--special-spell "Knight" "Turn undead") . (6 . "Clerical"))
                                        ("Cancellation" . (7 . "Matter"))
                                        ("Finger of death" . (7 . "Attack"))))))
              (ink-costs '(("scroll" . ((0 . "1")
                                        (1 . "4–7")
                                        (2 . "5–9")
                                        (3 . "6–11")
                                        (4 . "7–13")
                                        (5 . "8–15")
                                        (6 . "10–19")
                                        (7 . "15–29")))
                           ("spellbook" . ((1 . "5–9")
                                           (2 . "10–19")
                                           (3 . "15–29")
                                           (4 . "20–39")
                                           (5 . "25–49")
                                           (6 . "30–59")
                                           (7 . "35–69")))))
              (ink-alist (cdr (assoc-string spell-type ink-costs)))
              (candidates (cdr (assoc-string spell-type spells)))
              (longest-candidate (apply #'max (mapcar (lambda (c) (length (car c))) candidates)))
              (completion-extra-properties `(:annotation-function
                                             ,(lambda (cand)
                                                (format "%s    %s"
                                                        (make-string (- longest-candidate (length cand)) ? )
                                                        (propertize (cddr (assoc-string cand candidates)) 'face 'font-lock-doc-face)))
                                              :display-sort-function
                                              ,(lambda (cands)
                                                 (cl-sort cands (lambda (a b) (< (cadr (assoc-string a candidates)) (cadr (assoc-string b candidates))))))
                                              :group-function
                                              ,(lambda (cand transform)
                                                 (if transform cand
                                                   (format "Level %s (%s charges)"
                                                           (cadr (assoc-string cand candidates))
                                                           (alist-get (cadr (assoc-string cand candidates)) ink-alist)))))))
         (funcall (if nethack-proc #'nethack-send #'message) (condition-case _ (completing-read (concat prompt " ") candidates nil 'confirm-after-completion) (quit "\033"))))
     (funcall fn prompt initial))))


(defun nethack-interhack--old-corpses-turn-number ()
  (if-let ((turn (cadr (assoc-string "time" nethack-status-attributes))))
      (string-to-number turn)
    (warn "Turn number not present in status attributes, disabling old corpses plugin...")
    (nethack-interhack-old-corpses) ;; disable
    most-positive-fixnum))

(defvar nethack-interhack--death-tracker nil)

(defun nethack-interhack-old-corpses ()
  "Warn the user if a corpse is too old to eat."
  (nethack-interhack--toggle-advice
   nethack-nhapi-message "old-corpses"
   :after (_attr str)
   (when (string-match-p "You kill the \\([^!]+\\)!" str)
     (push (cons (match-string 1 str) (nethack-interhack--old-corpses-turn-number)) nethack-interhack--death-tracker)))

  (nethack-interhack--toggle-advice
   nethack-nhapi-yn-function "old-corpses"
   :filter-args (ques choices default)
   (when-let* (((string-match-p "There is an? \\(.*?\\) corpse here; eat it\\?" ques))
               (monster (match-string 1 ques))
               (turns-ago (- (nethack-interhack--old-corpses-turn-number) (or (cdr-safe (assoc-string monster nethack-interhack--death-tracker)) -50)))
               ((and (<= turns-ago 50) (not (member monster '("lichen" "lizard"))))))
     (setq ques (append ques (format #("\nWarning! That monster type was killed %d turns ago." 0 52 (face nethack-interhack-hint-face)) turns-ago))))
   (list ques choices default)))



(provide 'nethack-interhack)
;;; nethack-interhack.el ends here

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
