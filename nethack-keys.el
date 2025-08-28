;;; nethack-keys.el --- key bindings for nethack-el -*- lexical-binding:t -*-

;; Copyright (C) 2002,2003,2005  Ryan Yeske and Shawn Betts

;; Author: Ryan C Yeske <rcyeske@vcn.bc.ca>
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

;;; Code:

(defvar nh-accelerator-chars
  [?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v
      ?w ?x ?y ?z ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S
      ?T ?U ?V ?W ?X ?Y ?Z ?* ?#]
  "Vector of accelerator characters.")

(defvar nh-accelerator-chars-regexp
  (concat "[" (mapconcat 'char-to-string nh-accelerator-chars nil) "]")
  "A regexp that matches one of the nh-accelerator-chars")

(defvar nh-menu-item-regexp
  (concat "^\\(" nh-accelerator-chars-regexp "\\) \\([-+]\\|[0-9]+\\) .+$")
  "A regexp that matches a menu item.")

;; from src/options.c:
;;  * Standard letters (for now) are:
;;  *
;;  *    <  back 1 page
;;  *    >  forward 1 page
;;  *    ^  first page
;;  *    |  last page
;;  *    :  search
;;  *
;;  *    page    all
;;  *     ,    select     .
;;  *     \    deselect   -
;;  *     ~    invert     @
(defvar nh-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    ;; add all of the accelerator characters
    (mapc (lambda (char)
            (define-key map (vector char) 'nh-menu-toggle-item))
          nh-accelerator-chars)
    (define-key map "\C-c\C-c" 'nh-menu-submit)
    (define-key map "\C-m" 'nh-menu-submit)
    (define-key map "\C-g" 'nh-menu-cancel)
    (define-key map "\e\e\e" 'nh-menu-cancel)
    (define-key map " " 'scroll-up)
    (define-key map "\C-?" 'scroll-down)
    (define-key map "\t" 'nh-menu-goto-next)
    (define-key map "\e\t" 'nh-menu-goto-prev)
    (define-key map "," 'nh-menu-toggle-all-items)
    (define-key map "-" 'nh-menu-toggle-all-items)
    map)
  "Keymap used in Nethack menus.")

;; cmd.c is where the command-key mappings are done in the nh src
(defvar nh-map-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "_" 'nethack-command-travel)        ;_
    (define-key map "^" 'nethack-command-identify-trap) ;^

    ;;^[ Cancel command
    ;; I don't think we need a cancel command (rcy)
    ;;(define-key map "\e[" 'nethack-command-cancel)

    ;;^A      Redo the previous command
    (define-key map "\C-a" 'nethack-command-redo-previous)
    ;;^C      Quit the game
    ;;(define-key map "\C-c\C-c" 'nethack-command-quit)
    ;;^D      Kick something (usually a door, chest, or box)
    (define-key map "\C-d" 'nethack-command-kick)
    ;;^P      Toggle through previously displayed game messages
    (define-key map "\C-p" 'nethack-command-previous-message)
    ;;^R      Redraw screen
    (define-key map "\C-r" 'nethack-command-redraw-screen)
    ;;^T      Teleport around level
    (define-key map "\C-t" 'nethack-command-teleport-around-level)
    ;;^Z      Suspend game (only if defined) (NOTE: this is useless in emacs i think)
    (define-key map "\M-z" 'nethack-command-suspend-game)
    ;;a       Apply (use) a tool
    (define-key map "a" 'nethack-command-apply)
    ;;A       Remove all armor
    (define-key map "A" 'nethack-command-remove-all-armor)
    ;;b       Go southwest 1 space
    (define-key map "b" 'nethack-command-southwest)
    ;;B       Go southwest until you are on top of something
    (define-key map "B" 'nethack-command-southwest-until-ontop)
    ;;^B      Go southwest until you are near something
    (define-key map "\M-\C-b" 'nethack-command-southwest-until-near)
    ;;c       Close a door
    (define-key map "c" 'nethack-command-close-door)
    ;;C       Call (name) a particular monster
    (define-key map "C" 'nethack-command-call-monster)
    ;;d       Drop an item
    (define-key map "d" 'nethack-command-drop)
    ;;D       Drop specific item types
    (define-key map "D" 'nethack-command-drop-specific-item)
    ;;e       Eat something
    (define-key map "e" 'nethack-command-eat)
    ;;E       Engrave writing on the floor
    (define-key map "E" 'nethack-command-engrave)
    ;;f       Fire ammunition from quiver
    (define-key map "f" 'nethack-command-fire)
    ;;F       Followed by direction, fight a monster (even if you don't sense it)
    (define-key map "F" 'nethack-command-force-fight)
    ;;g       Followed by direction, move until you are near something
    (define-key map "g" 'nethack-command-move-until-near)
    ;;G       Followed by direction, same as control-direction
    (define-key map "G" 'nethack-command-move)
    ;;h       Go west 1 space
    (define-key map "h" 'nethack-command-west)
    ;;H       Go west until you are on top of something
    (define-key map "H" 'nethack-command-west-until-ontop)
    ;;^H      Go west until you are near something (NOTE: what to do with this key?)
    (define-key map "\M-\C-h" 'nethack-command-west-until-near)
    ;;i       Show your inventory
    (define-key map "i" 'nethack-command-inventory)
    ;;I       Inventory specific item types
    (define-key map "I" 'nethack-command-type-inventory)
    ;;j       Go south 1 space (or if number_pad is on, jump to another location)
    (define-key map "j" 'nethack-command-south)
    ;;J       Go south until you are on top of something
    (define-key map "J" 'nethack-command-south-until-ontop)
    ;;^J      Go south until you are near something
    (define-key map "\M-\C-j" 'nethack-command-south-until-near)
    ;;k       Go north 1 space (or if number_pad is on, kick something)
    (define-key map "k" 'nethack-command-north)
    ;;K       Go north until you are on top of something
    (define-key map "K" 'nethack-command-north-until-ontop)
    ;;^K      Go north until you are near something
    (define-key map "\M-\C-k" 'nethack-command-north-until-near)
    ;;l       Go east 1 space (or if number_pad is on, loot a box on the floor)
    (define-key map "l" 'nethack-command-east)
    ;;L       Go east until you are on top of something
    (define-key map "L" 'nethack-command-east-until-ontop)
    ;;^L      Go east until you are near something
    (define-key map "\M-\C-l" 'nethack-command-east-until-near)
    ;;m       Followed by direction, move without picking anything up or fighting
    (define-key map "m" 'nethack-command-move-no-pickup-or-fight)
    ;;M       Followed by direction, move a distance without picking anything up
    (define-key map "M" 'nethack-command-move-distance-no-pickup)
    ;;n       Go southeast 1 space
    (define-key map "n" 'nethack-command-southeast)
    ;;N       Go southeast until you are on something (if number_pad, name an object)
    (define-key map "N" 'nethack-command-southeast-until-ontop)
    ;;^N      Go southeast until you are near something
    (define-key map "\M-\C-n" 'nethack-command-southeast-until-near)
    ;;o       Open a door
    (define-key map "o" 'nethack-command-open)
    ;;O       Show option settings, possibly change them
    (define-key map "O" 'nethack-command-settings)
    ;;p       Pay your shopping bill
    (define-key map "p" 'nethack-command-pay)
    ;;P       Put on an accessory (ring, amulet, etc)
    (define-key map "P" 'nethack-command-put-on)
    ;;q       Quaff (drink) something
    (define-key map "q" 'nethack-command-quaff)
    ;;Q       Select ammunition for quiver
    (define-key map "Q" 'nethack-command-select-ammo-for-quiver)
    ;;r       Read a scroll or spellbook
    (define-key map "r" 'nethack-command-read)
    ;;R       Remove an accessory (ring, amulet, etc)
    (define-key map "R" 'nethack-command-remove-accessory)
    ;;s       Search for traps and secret doors
    (define-key map "s" 'nethack-command-search)
    ;;S       Save the game
    (define-key map "S" 'nethack-command-save-game)
    ;;t       Throw something
    (define-key map "t" 'nethack-command-throw)
    ;;T       Take off one piece of armor
    (define-key map "T" 'nethack-command-remove-single-armor)
    ;;u       Go northeast 1 space (or if number_pad is on, untrap something)
    (define-key map "u" 'nethack-command-northeast)
    ;;U       Go northeast until you are on top of something
    (define-key map "U" 'nethack-command-northeast-until-ontop)
    ;;^U      Go northeast until you are near something
    (define-key map "\M-\C-u" 'nethack-command-northeast-until-near)
    ;;v       Show version
    (define-key map "v" 'nethack-command-version)
    ;;V       Show long version and game history
    (define-key map "V" 'nethack-command-version-and-history)
    ;;w       Wield (put in use) a weapon
    (define-key map "w" 'nethack-command-wield)
    ;;W       Wear a piece of armor
    (define-key map "W" 'nethack-command-wear-armor)
    ;;x       Swap wielded and secondary weapons
    (define-key map "x" 'nethack-command-swap-weapons)
    ;;X       Enter explore (discovery) mode (only if defined)
    (define-key map "X" 'nethack-command-explore-mode)
    ;;y       Go northwest 1 space
    (define-key map "y" 'nethack-command-northwest)
    ;;Y       Go northwest until you are on top of something
    (define-key map "Y" 'nethack-command-northwest-until-ontop)
    ;;^Y      Go northwest until you are near something
    (define-key map "\M-\C-y" 'nethack-command-northwest-until-near)
    ;;z       Zap a wand
    (define-key map "z" 'nethack-command-zap-wand)
    ;;Z       Zap (cast) a spell
    (define-key map "Z" 'nethack-command-cast-spell)
    ;;<       Go up a staircase
    (define-key map "<" 'nethack-command-up)
    ;;>       Go down a staircase
    (define-key map ">" 'nethack-command-down)
    ;;/       Show what type of thing a symbol corresponds to
    (define-key map "/" 'nethack-command-what-is-symbol)
    ;;?       Give a help message
    (define-key map "?" 'nethack-command-help)
    ;;&       Tell what a command does
    (define-key map "&" 'nethack-command-command-help)
    ;;!       Do a shell escape (only if defined)
    (define-key map "!" 'nethack-command-getpos-menu)
    ;;\       Show what object types have been discovered
    (define-key map "\\" 'nethack-command-show-discoveries)
    ;;.       Rest one move while doing nothing
    (define-key map "." 'nethack-command-rest-one-move)
    ;;        Rest one move while doing nothing (if rest_on_space option is on)
    ;;(define-key map " " 'nethack-command-rest-one-move)
    ;; :       Look at what is on the floor
    (define-key map ":" 'nethack-command-look-here)
    ;; ;       Show what type of thing a map symbol on the level corresponds to
    (define-key map ";" 'nethack-command-what-is-map-piece)
    ;;,       Pick up things at the current location
    (define-key map "," 'nethack-command-pick-up)
    ;;@       Toggle the pickup option on/off
    (define-key map "@" 'nethack-command-toggle-pickup)
    ;;)       Show the weapon currently wielded
    (define-key map ")" 'nethack-command-show-wielded-weapon)
    ;;[       Show the armor currently worn
    (define-key map "[" 'nethack-command-show-worn-armor)
    ;;=       Show the ring(s) currently worn
    (define-key map "=" 'nethack-command-show-worn-rings)
    ;;"       Show the amulet currently worn
    (define-key map "\"" 'nethack-command-show-worn-amulet)
    ;;(       Show the tools currently in use
    (define-key map "(" 'nethack-command-show-tool-in-use)
    ;;*       Show all equipment in use (combination of the ),[,=,",( commands)
    (define-key map "*" 'nethack-command-show-all-equipment-in-use)
    ;;$       Count your gold
    (define-key map "$" 'nethack-command-count-gold)
    ;;+       List known spells
    (define-key map "+" 'nethack-command-list-known-spells)
    ;;^X      Show your attributes (intrinsic ones included in debug or explore mode)
    (define-key map "\M-\C-x" 'nethack-command-show-attributes)


    ;;; standard wizard commands
    ;;^E      Search a room (available in debug mode only)
    (define-key map "\C-e" 'nethack-command-wizard-detect)
    ;;^F      Map the level (available in debug mode only)
    (define-key map "\C-f" 'nethack-command-wizard-map)
    ;;^G      Create a monster (available in debug mode only)
    (define-key map "\M-\C-g" 'nethack-command-wizard-genesis)
    ;;^I      Identify all items (available in debug mode only)
    (define-key map "\C-i" 'nethack-command-wizard-identify)
    ;;^O      Show location of special levels (available in debug mode only)
    (define-key map "\C-o" 'nethack-command-wizard-where)
    ;;^V      Teleport between levels (available in debug mode only)
    (define-key map "\C-v" 'nethack-command-wizard-level-teleport)
    ;;^W      Wish (available in debug mode only)
    (define-key map "\C-w" 'nethack-command-wizard-wish)

    ;;; extended commands

    ;;#       Perform an extended command
    (define-key map "#" 'nethack-command-read-extended)

    ;;M-p     Pray to the gods for help
    (define-key map "\ep" 'nethack-command-pray)
    ;;M-a     Adjust inventory letters
    (define-key map "\ea" 'nethack-command-adjust)
    ;;M-c     Talk to someone
    (define-key map "\ec" 'nethack-command-chat)
    ;;M-C     list which challenders you have adhered to
    (define-key map "\eC" 'nethack-command-conduct)
    ;;M-d     Dip an object into something
    (define-key map "\ed" 'nethack-command-dip)

    ;;M-e     Advance or check weapons skills
    (define-key map "\ee" 'nethack-command-enhance)
    ;;M-f     Force a lock
    (define-key map "\ef" 'nethack-command-force)
    ;;M-i     Invoke an object's special powers
    (define-key map "\ei" 'nethack-command-invoke)
    ;;M-j     Jump to another location
    (define-key map "\ej" 'nethack-command-jump)
    ;;M-l     Loot a box on the floor
    (define-key map "\el" 'nethack-command-loot)

    ;;M-m     Use a monster's special ability
    (define-key map "\em" 'nethack-command-monster)
    ;;M-n     Name an item or type of object
    (define-key map "\en" 'nethack-command-name)
    ;;M-o     Offer a sacrifice to the gods
    (define-key map "\eo" 'nethack-command-offer)
    ;;M-q     Quit
    (define-key map "\eq" 'nethack-command-quit)

    ;;M-r u   Rub a lamp
    (define-key map "\eru" 'nethack-command-rub)
    ;;M-r i   Ride
    (define-key map "\eri" 'nethack-command-ride)
    ;;M-s     Sit down
    (define-key map "\es" 'nethack-command-sit)
    ;;M-T     Turn undead
    (define-key map "\eT" 'nethack-command-turn)
    ;;M-t   toggle two-weapon combat
    (define-key map "\et" 'nethack-command-twoweapon)
    ;;M-u     Untrap something (trap, door, or chest)
    (define-key map "\eu" 'nethack-command-untrap)

    ;;M-v     Print compile time options for this version of NetHack
    (define-key map "\ev" 'nethack-command-extended-version)
    ;;M-w     Wipe off your face
    (define-key map "\ew" 'nethack-command-wipe)

    ;;; extended wizard commands....

    map)
  "Keymap used on the Nethack map.")


(provide 'nethack-keys)

;;; nethack-keys.el ends here
