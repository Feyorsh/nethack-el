;;; nethack-api.el -- low level Emacs interface the lisp window-port
;;; of Nethack-3.3.x
;;; $Id: nethack-api.el,v 1.38 2001/10/19 11:05:16 sabetts Exp $

;;; originally a machine translation of nethack-3.3.0/doc/window.doc
;;; from the nethack src package.

;;; Ryan Yeske (rcyeske@vcn.bc.ca)
;;; Sat Mar 18 11:24:02 2000

(require 'ewoc)
(require 'gamegrid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file documents the support for various windowing systems in
;; NetHack.  The support is through a standard interface, separating
;; the main NetHack code from window-system specific code.  The
;; implementation supports multiple window systems in the same binary.
;; Even if you only wish to support one window-port on your port, you
;; will need to follow the instructions in Section VII to get a
;; compilable binary.

;; Contents:
;; 	I.   Window Types and Terminology
;; 	II.  Interface Specification
;; 	III. Global variables
;; 	IV.  New or respecified common, high level routines
;; 	V.   Game startup
;; 	VI.  Conventions
;; 	VII. Implementation and Multi-window support

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I.  Window Types and Terminology
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; There are 5 basic window types, used to call create_nhwindow():

;;; FIXME: what are the values for these
;; (defconst nethack-api-wmessage nil)	;NHW_MESSAGE (top line)
;; (defconst nethack-api-wstatus nil)	;NHW_STATUS	(bottom lines)
;; (defconst nethack-api-wmap nil)	;NHW_MAP(main dungeon)
;; (defconst nethack-api-wmenu nil)	;NHW_MENU (inventory or other
;; 					;"corner" windows)
;; (defconst nethack-api-wtext nil)	;NHW_TEXT (help/text, full
					;screen paged window)

;; The tty window-port also uses NHW_BASE (the base display)
;; internally.

;; NHW_MENU windows can be used for either menu or text display.
;; Their basic feature is that for the tty-port, if the window is
;; small enough, it appears in the corner of the tty display instead
;; of overwriting the whole screen.  The first call to add information
;; to the window will decide if it is going to be used to display a
;; menu or text.  If start_menu() is called, then it will be used as a
;; menu.  If putstr() is called, it will be used as text.  Once
;; decided, there is no turning back.  For the tty-port, if the data
;; is too large for a single screen then the data is paged (with
;; --more--) between pages.  Only NHW_MENU type windows can be used
;; for menus.

;; NHW_TEXT windows are used to display a large amount of textual
;; data.  This is the type of window one would use for displaying a
;; help file, for example.  In the tty window-port, windows of type
;; NHW_TEXT can page using the DEF_PAGER, if DEF_PAGER is defined.
;; There exists an assumption that the font for text windows is
;; monospaced.  The help files are all formatted accordingly.

;; "window" is always of type winid.  This is currently implemented as
;; an integer, but doesn't necessarily have to be done that way.
;; There are a few fixed window names that are known throughout the
;; code:

;;; FIXME: what are the values for these
;; (defconst nethack-api-win-message nil);	WIN_MESSAGE (top line)
;; (defconst nethack-api-win-status nil) ;	WIN_STATUS (bottom lines)
;; (defconst nethack-api-win-map nil)	;	WIN_MAP (main dungeon)
;; (defconst nethack-api-win-inven nil)	;	WIN_INVEN (inventory)

;; Other windows are created and destroyed as needed.

;; "Port" in this document refers to a CPU/OS/hardware platform (UNIX, MSDOS
;; TOS, etc.)  "window-port" refers to the windowing platform.  This is
;; orthogonal (e.g.  UNIX might use either a tty window-port or an X11
;; window-port).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; II.  Interface Specification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All functions below are void unless otherwise noted.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A.  Low-level routines:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; raw_print(str) -- Print directly to a screen, or otherwise
;; guarantee that the user sees str.  raw_print() appends a newline to
;; str.  It need not recognize ASCII control characters.  This is used
;; during startup (before windowing system initialization -- maybe this
;; means only error startup messages are raw), for error messages, and
;; maybe other 'msg' uses.  E.g.  updating status for micros (i.e,
;; 'saving').

(defvar nethack-raw-print-buffer-name "*nhw raw-print*"
  "Buffer name for Nethack raw-print messages.")

(defun nethack-api-raw-print (str)
  (save-excursion
    (let ((buffer (get-buffer-create nethack-raw-print-buffer-name)))
    (set-buffer buffer)
    (insert str "\n")
    (display-buffer buffer)))
  'void)

;; raw_print_bold(str) -- Like raw_print(), but prints in
;; bold/standout (if possible).

(defun nethack-api-raw-print-bold (str)
  (nethack-api-raw-print (propertize str 'face 'bold))
  'void)


;; curs(window, x, y) -- Next output to window will start at (x,y),
;; also moves displayable cursor to (x,y).  For backward compatibility, 1
;; <= x < cols, 0 <= y < rows, where cols and rows are the size of
;; window.  -- For variable sized windows, like the status window, the
;; behavior when curs() is called outside the window's limits is
;; unspecified. The mac port wraps to 0, with the status window being 2
;; lines high and 80 columns wide.  -- Still used by curs_on_u(), status
;; updates, screen locating (identify, teleport).  -- NHW_MESSAGE,
;; NHW_MENU and NHW_TEXT windows do not currently support curs in the tty
;; window-port.

(defun nethack-api-curs (winid x y)
  ""
  (save-excursion
    (set-buffer (nethack-buffer winid))
    (cond ((eq nethack-buffer-type 'nhw-map)
	   (goto-char (gamegrid-cell-offset (- x 1) y)))
	  ((eq nethack-buffer-type 'nhw-status)
	   (setq nethack-status-line-number y))))
  'void)


;; putstr(window, attr, str) -- Print str on the window with the
;; given attribute.  Only printable ASCII characters (040-0126) must be
;; supported.  Multiple putstr()s are output on separate lines.
;; Attributes can be one of ATR_NONE (or 0) ATR_ULINE ATR_BOLD ATR_BLINK
;; ATR_INVERSE If a window-port does not support all of these, it may map
;; unsupported attributes to a supported one (e.g. map them all to
;; ATR_INVERSE).  putstr() may compress spaces out of str, break str, or
;; truncate str, if necessary for the display.  Where putstr() breaks a
;; line, it has to clear to end-of-line.  -- putstr should be implemented
;; such that if two putstr()s are done consecutively the user will see
;; the first and then the second.  In the tty port, pline() achieves this
;; by calling more() or displaying both on the same line.

(defun nethack-api-putstr (winid attr str)
  ""
  (save-excursion
    (set-buffer (nethack-buffer winid))
    (cond ((eq nethack-buffer-type 'nhw-status)
	   (if (= nethack-status-line-number 0)
	       (setcar nethack-status-lines str)
	     (setcdr nethack-status-lines str)
	   (set-buffer (nethack-buffer winid))
	   (erase-buffer)
	   (nethack-parse-status-lines (car nethack-status-lines) (cdr nethack-status-lines))
	   (insert (nethack-format-status nethack-status-string))))
	  ((eq nethack-buffer-type 'nhw-message)
	   (goto-char (point-max))
	   (insert "\n" (propertize str 'face 'bold))
	   ;; scroll to show maximum output on all windows displaying buffer
	   (let ((l (get-buffer-window-list (nethack-buffer winid))))
	     (save-selected-window
	       (mapc (lambda (w)
		       (select-window w)
		       (set-window-point w (point-max))
		       (recenter -1))
		     l))))
	  (t
	   (goto-char (point-max))
	   (insert (if (eq attr 'atr-none)
		       str
		     (propertize str 'face 'highlight)) ;FIXME: other attrs
		   "\n"))))
  'void)


;; get_nh_event() -- Does window event processing (e.g. exposure
;; events).  A noop for the tty and X window-ports.

(defun nethack-api-get-event ()
  ""

  'void)


;; int nhgetch() -- Returns a single character input from the
;; user. (int) -- In the tty window-port, nhgetch() assumes that tgetch()
;; will be the routine the OS provides to read a character.  Returned
;; character _must_ be non-zero.

(defun nethack-api-get-command ()
  ""
  (if (null nethack-command-queue)
      (setq nethack-waiting-for-command-flag t)
    (nethack-process-send-string (car nethack-command-queue))
    (setq nethack-command-queue (cdr nethack-command-queue)))
  'void)				   

;; int nh_poskey(int *x, int *y, int *mod) -- Returns a single
;; character input from the user or a a positioning event (perhaps from a
;; mouse).  If the return value is non-zero, a character was typed, else,
;; a position in the MAP window is returned in x, y and mod.  mod may be
;; one of CLICK_1 /* mouse click type 1 */ CLICK_2 /* mouse click type 2
;; */ The different click types can map to whatever the hardware
;; supports.  If no mouse is supported, this routine always returns a
;; non-zero character.

(defun nethack-api-poskey (x y mod)
  ""
  'unimplemented)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; B.  High-level routines:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; print_glyph(window, x, y, glyph) -- Print the glyph at (x,y) on the
;; given window.  Glyphs are integers at the interface, mapped to
;; whatever the window- port wants (symbol, font, color, attributes,
;; ...there's a 1-1 map between glyphs and distinct things on the map).

(defun nethack-api-print-glyph (winid x y type offset face glyph ch)
  ""
  (save-excursion
    (set-buffer (nethack-buffer winid))
    (setq x (- x 1)) 				; FIXME: hack
    (let ((inhibit-read-only t))
      (gamegrid-set-cell x y ch)
      (put-text-property (gamegrid-cell-offset x y)
			 (1+ (gamegrid-cell-offset x y))
			 'face
			 (cdr (assoc face nethack-color-alist)))))
  'void)


;; char yn_function(const char *ques, const char *choices, char
;; default) -- Print a prompt made up of ques, choices and default.  Read
;; a single character response that is contained in choices or default.
;; If choices is NULL, all possible inputs are accepted and returned.
;; This overrides everything else.  The choices are expected to be in
;; lower case.  Entering ESC always maps to 'q', or 'n', in that order,
;; if present in choices, otherwise it maps to default.  Entering any
;; other quit character (SPACE, RETURN, NEWLINE) maps to default.  -- If
;; the choices string contains ESC, then anything after it is an
;; acceptable response, but the ESC and whatever follows is not included
;; in the prompt.  -- If the choices string contains a '#' then accept a
;; count.  Place this value in the global *yn_number* and return '#'.  --
;; This uses the top line in the tty window-port, other ports might use a
;; popup.

(defun nethack-api-yn-function (ques choices default)
  ""  
  (let ((cursor-in-echo-area t)
	all-choices 
	key)

    (if (= default 0)
	(setq all-choices (string-to-list choices))
      (setq all-choices (string-to-list (concat (char-to-string default)
						choices))))
    ;; Add some special keys of our own to the choices
    (add-to-list 'all-choices 13)
    
    (message (concat ques " "))
    (setq key (read-char))

    (if (> (length choices) 0)
	(while (not (member key all-choices))
	  (message (concat ques " "))
	  (setq key (read-char))))

    (if (= 13 key)
	default
      key)))

(defun nethack-api-ask-direction ()
  "Prompt the user for a direction"
  (let ((cursor-in-echo-area t)
	(e (where-is-internal 'nethack-command-east nethack-mode-map))
	(w (where-is-internal 'nethack-command-west nethack-mode-map))
	(n (where-is-internal 'nethack-command-north nethack-mode-map))
	(s (where-is-internal 'nethack-command-south nethack-mode-map))
	(ne (where-is-internal 'nethack-command-northeast nethack-mode-map))
	(nw (where-is-internal 'nethack-command-northwest nethack-mode-map))
	(se (where-is-internal 'nethack-command-southeast nethack-mode-map))
	(sw (where-is-internal 'nethack-command-southwest nethack-mode-map))
	(ch (read-key-sequence-vector "In what direction? ")))
    (cond ((member ch n) 'n)
	  ((member ch s) 's)
	  ((member ch e) 'e)
	  ((member ch w) 'w)
	  ((member ch ne) 'ne)
	  ((member ch nw) 'nw)
	  ((member ch se) 'se)
	  ((member ch sw) 'sw))))

;; getlin const char *ques, char *input) -- Prints ques as a prompt
;; and reads a single line of text, up to a newline.  The string entered
;; is returned without the newline.  ESC is used to cancel, in which case
;; the string '\033\000' is returned.  -- getlin() must call
;; flush_screen(1) before doing anything.  -- This uses the top line in
;; the tty window-port, other ports might use a popup.

(defun nethack-api-getlin (ques)
  "" 
  (read-from-minibuffer (concat ques " ")))


;; int get_ext_cmd(void) -- Get an extended command in a window-port
;; specific way.  An index into extcmdlist[] is returned on a successful
;; selection, -1 otherwise.

;; player_selection() -- Do a window-port specific player type
;; selection.  If player_selection() offers a Quit option, it is its
;; responsibility to clean up and terminate the process.  You need to
;; fill in pl_character[0].

(defun nethack-api-player-selection ()
  " Does nothing right now, perhaps simply indicates that the
nethack-api-choose-X calls are to follow for actual
role/race/gender/align selection."  
  'void)

(defun nethack-choose-attribute (prompt alist abort)
  "Prompts user for an element from the cars of ALIST and returns the
corresponding cdr."
  (if (> (length alist) 1)
      (let ((completion-ignore-case t))
	(condition-case nil
	    (cdr (assoc (completing-read prompt alist nil t) alist))
	  (quit abort)))
    (cdar alist)))
  
(defun nethack-api-choose-role (role-alist)
  (nethack-choose-attribute "Choose role: " role-alist -1))

(defun nethack-api-choose-race (race-alist)
  (nethack-choose-attribute "Choose race: " race-alist -1))

(defun nethack-api-choose-gender (gender-alist)
  (nethack-choose-attribute "Choose gender: " gender-alist -1))

(defun nethack-api-choose-alignment (alignment-alist)
  (nethack-choose-attribute "Choose alignment: " alignment-alist -1))

;;  display_file(str, boolean complain) -- Display the file named str.
;; Complain about missing files iff complain is TRUE.

(defun nethack-api-display-file (str complain)
  (let ((file (concat nethack-directory str)))
    (if (file-exists-p file)
	(view-file file)
      (if complain (message "Cannot find file %s" file))))
  'void)

;; update_inventory() -- Indicate to the window port that the
;; inventory has been changed.  -- Merely calls display_inventory() for
;; window-ports that leave the window up, otherwise empty.

(defun nethack-api-update-inventory ()
  " FIXME: should set a flag or something, later"

  'void)


;; doprev_message() -- Display previous messages.  Used by the ^P
;; command.  -- On the tty-port this scrolls WIN_MESSAGE back one line.

(defun nethack-api-doprev-message ()
  ""
  (save-selected-window
    (save-excursion
      (walk-windows (lambda (w)
		      (select-window w)
		      (set-buffer (window-buffer))
		      (if (eq nethack-buffer-type 'nhw-message)
			  (scroll-down))))))
  'void)


;; update_positionbar(char *features) -- Optional, POSITIONBAR must be
;; defined. Provide some additional information for use in a horizontal
;; position bar (most useful on clipped displays).  Features is a series
;; of char pairs.  The first char in the pair is a symbol and the second
;; char is the column where it is currently located.  A '<' is used to
;; mark an upstairs, a '>' for a downstairs, and an '@' for the current
;; player location. A zero char marks the end of the list.

(defun nethack-api-update-positionbar (features)
  ""

  'unimplemented)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C.  Window Utility Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; init_nhwindows(int* argcp, char** argv) -- Initialize the windows
;; used by NetHack.  This can also create the standard windows listed at
;; the top, but does not display them.  -- Any commandline arguments
;; relevant to the windowport should be interpreted, and *argcp and *argv
;; should be changed to remove those arguments.  -- When the message
;; window is created, the variable iflags.window_inited needs to be set
;; to TRUE.  Otherwise all plines() will be done via raw_print().  ** Why
;; not have init_nhwindows() create all of the 'standard' ** windows?  Or
;; at least all but WIN_INFO? -dean

(defvar nethack-directory nil
  "Location of the actual nethack executable along with the help
files.")

(defun nethack-api-init-nhwindows (executable &rest args)
  " This is the first function sent by the nethack process.  Does
all of the appropriate setup."
  (setq nethack-directory (file-name-directory executable))
  'void)


;; exit_nhwindows(str) -- Exits the window system.  This should
;; dismiss all windows, except the 'window' used for raw_print().  str is
;; printed if possible.

(defun nethack-api-exit-nhwindows (str)
  "" 
  ;; clean up buffers
  (mapc (lambda (b) (kill-buffer (cdr b))) nethack-buffer-table)
  (setq nethack-buffer-table nil)

  (nethack-api-raw-print str)
  'void)

;; window = create_nhwindow(type) -- Create a window of type 'type'.

(defvar nethack-buffer-table nil
  "An alist of (DIGIT-ID . BUFFER) pairs")
 
(defvar nethack-buffer-type nil
  "Buffer local variable indicating the type of buffer this is.")

(defvar nethack-map-buffer nil)
(defvar nethack-status-buffer nil)
(defvar nethack-message-buffer nil)

(defun nethack-api-create-nhwindow (type)
  "Create a new window of TYPE and add it to the buffer table, preform
type specific initalizations, and return a digit id."
  (let* ((winid (+ 1 (apply 'max -1 (mapcar 'car nethack-buffer-table))))
	 (name (format "*%s* %d" (symbol-name type) winid))
	 (buf (get-buffer-create name)))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables)
      (setq nethack-buffer-type type)
      (cond ((eq type 'nhw-map)
	     (setq nethack-map-buffer (current-buffer))
	     (nethack-map-mode))
	    ((eq type 'nhw-status)
	     (setq nethack-status-buffer (current-buffer)))
	    ((eq type 'nhw-message)
	     (setq nethack-message-buffer (current-buffer)))))
    (push (cons winid buf) nethack-buffer-table)
    winid))

;; clear_nhwindow(window) -- Clear the given window, when
;; appropriate.

(defun nethack-api-clear-nhwindow (winid)
  ""
  (save-excursion
    (set-buffer (nethack-buffer winid))
    (cond ((eq nethack-buffer-type 'nhw-map)
	   (gamegrid-init (make-vector 256 nil))
	   (gamegrid-init-buffer nethack-map-width
				 nethack-map-height
				 ?.))
	  ((eq nethack-buffer-type 'nhw-message)
	   (set-text-properties (point-min) (point-max) nil))
	  (t
	   (let ((inhibit-read-only t))
	     (erase-buffer)))))
  'void)


;; display_nhwindow(window, boolean blocking) -- Display the window on
;; the screen.  If there is data pending for output in that window, it
;; should be sent.  If blocking is TRUE, display_nhwindow() will not
;; return until the data has been displayed on the screen, and
;; acknowledged by the user where appropriate.  -- All calls are blocking
;; in the tty window-port.  -- Calling display_nhwindow(WIN_MESSAGE,???)
;; will do a --more--, if necessary, in the tty window-port.

(defun nethack-api-display-nhwindow (winid blocking)
  (save-excursion
    (set-buffer (nethack-buffer winid))
    (if blocking
	(cond ((or (eq nethack-buffer-type 'nhw-menu)
		   (eq nethack-buffer-type 'nhw-text))
	       (nethack-protect-windows
		(let ((maybe-window
		       (display-message-or-buffer (nethack-buffer winid))))
		  (if (not (windowp maybe-window))
		      (let ((cursor-in-echo-area t))
			(read-char-exclusive)
			(nethack-process-send nil))
		    (setq nethack-window-configuration (current-window-configuration))
		    (select-window maybe-window)
		    (nethack-menu-reset-keymap)
		    (use-local-map nethack-menu-keymap)))))		      
	      ;; At the end of the game, the message buffer is shown
	      ;; one last time...we should do something better here,
	      ;; but the problem is we have no way of indicating to
	      ;; the user that we are waiting for a key to move on.
	      ((eq nethack-buffer-type 'nhw-message)
	       (nethack-process-send nil))
	      (t (message "displaying %d with blocking %s" winid blocking)))
      ;; Do nothing for nonblocking, except use if we are displaying
      ;; the status window, in which case set up all of the windows
      ;; for display for the first time.
      (if (eq nethack-buffer-type 'nhw-status)
	(nethack-setup-window-configuration))))
  'void)

(defvar nethack-status-window-height 4
  "Default height of the status window.")
(defvar nethack-message-window-height 10
  "Default height of the message window.")

(defun nethack-setup-window-configuration ()
  "Layout the nethack windows according to the values
`nethack-status-window-height' and `nethack-message-window-height'."
  (delete-other-windows)
  (switch-to-buffer nethack-status-buffer)
  (split-window-vertically (- nethack-status-window-height))
  (switch-to-buffer nethack-message-buffer)
  (split-window-vertically nethack-message-window-height)
  (switch-to-buffer-other-window nethack-map-buffer))

;; destroy_nhwindow(window) -- Destroy will dismiss the window if the
;; window has not already been dismissed.

(defun nethack-api-destroy-nhwindow (winid)
  (let ((buffer (nethack-buffer winid)))
    (delete-windows-on buffer nil)
    (kill-buffer buffer)
    (setq nethack-buffer-table (assq-delete-all winid nethack-buffer-table))
  'void))


;; start_menu(window) -- Start using window as a menu.  You must call
;; start_menu() before add_menu().  After calling start_menu() you may
;; not putstr() to the window.  Only windows of type NHW_MENU may be used
;; for menus.



;;; menus

(defvar nethack-menu nil)

(defun nethack-menu-item-print (item)
(let ((acc (elt item 0))
      (val (elt item 1))
      (str (elt item 2))
      (id  (elt item 3)))
  (insert (if (equal id -1)
	      ""
	    (concat (char-to-string acc) 
		    (if val " + " " - ")))
	  str)))
  
(defvar nethack-menu-how nil
  "One of pick-one, pick-none, pick-any.")

(defun nethack-menu-toggle-item ()
  "Toggle the menu item that is associated with the key event that
triggered this function being called."
  (interactive)
  (ewoc-map (lambda (i) 
	      (when (equal (elt i 0) last-command-event)
		(aset i 1 (not (elt i 1)))
		t)) ;; force redisplay
	    nethack-menu)
  (if (eq nethack-menu-how 'pick-one)
      (nethack-menu-submit)))

(defvar nethack-window-configuration nil
  "Window configuration for the default state (no menus or other
popups).")

(defun nethack-menu-submit ()
  "Submit the selected menu options to the nethack process."
  (interactive)
  (if nethack-menu
      (let ((selected (ewoc-collect nethack-menu (lambda (x) (aref x 1)))))
	(setq nethack-menu nil)
	(nethack-process-send
	 (mapcar (lambda (x) (list (aref x 3) -1)) selected)))
    (nethack-process-send nil))
  (set-window-configuration nethack-window-configuration))

(defun nethack-menu-cancel ()
  "Dismiss a menu with out making any choices."
  (interactive)
  (setq nethack-menu nil)
  (nethack-menu-submit))  

(defvar nethack-menu-keymap nil
  "Keymap for use in Nethack menus.")

(defun nethack-menu-reset-keymap ()
  (setq nethack-menu-keymap (make-sparse-keymap))
  (define-key nethack-menu-keymap (kbd "C-c C-c") 'nethack-menu-submit)
  (define-key nethack-menu-keymap (kbd "RET") 'nethack-menu-submit)
  (define-key nethack-menu-keymap (kbd "C-g") 'nethack-menu-cancel))

(defun nethack-api-start-menu (winid)
  ""
  (save-excursion
    (set-buffer (get-buffer-create (nethack-buffer winid)))
    (erase-buffer)
    (setq nethack-menu (ewoc-create 'nethack-menu-item-print "" ""))
    (nethack-menu-reset-keymap)
    (setq nethack-unassigned-accelerator-index 0))
  'void)


;; add_menu(windid window, int glyph, const anything identifier, char
;; accelerator, char groupacc, int attr, char *str, boolean preselected)
;; -- Add a text line str to the given menu window.  If identifier is 0,
;; then the line cannot be selected (e.g. a title).  Otherwise,
;; identifier is the value returned if the line is selected.  Accelerator
;; is a keyboard key that can be used to select the line.  If the
;; accelerator of a selectable item is 0, the window system is free to
;; select its own accelerator.  It is up to the window-port to make the
;; accelerator visible to the user (e.g. put 'a - ' in front of str).
;; The value attr is the same as in putstr().  Glyph is an optional glyph
;; to accompany the line.  If window port cannot or does not want to
;; display it, this is OK.  If there is no glyph applicable, then this
;; value will be NO_GLYPH.  -- All accelerators should be in the range
;; [A-Za-z].  -- It is expected that callers do not mix accelerator
;; choices.  Either all selectable items have an accelerator or let the
;; window system pick them.  Don't do both.  -- Groupacc is a group
;; accelerator.  It may be any character outside of the standard
;; accelerator (see above) or a number.  If 0, the item is unaffected by
;; any group accelerator.  If this accelerator conflicts with the menu
;; command (or their user defined alises), it loses.  The menu commands
;; and aliases take care not to interfere with the default object class
;; symbols.  -- If you want this choice to be preselected when the menu
;; is displayed, set preselected to TRUE.

(defvar nethack-unassigned-accelerators 
  [?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v
?w ?x ?y ?z ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S
?T ?U ?V ?W ?X ?Y ?Z]
  "Vector of accelerators that are used in menus that don't specify
accelerators.")

(defvar nethack-unassigned-accelerator-index 0
  "Index into `nethack-unassigned-accelerators' indicating the next
accelerator that will be used in unassigned menus.")

(defun nethack-specify-accelerator ()
  "Return the next accelerator from `nethack-unassigned-accelerators'
specified by `nethack-unassigned-accelerator-index'."
  (prog1
      (aref nethack-unassigned-accelerators 
	    nethack-unassigned-accelerator-index)
    (setq nethack-unassigned-accelerator-index
	  (+ 1 nethack-unassigned-accelerator-index))))

(defun nethack-api-add-menu (window glyph identifier accelerator groupacc attr str preselected)
  ""
  (let ((acc (if (and (not (= -1 identifier))
		      (zerop accelerator))
		 (nethack-specify-accelerator)
	       accelerator)))
    (ewoc-enter-last nethack-menu (vector
				   acc
				   preselected
				   (if (eq attr 'atr-none)
				       str
				     (propertize str 'face 'highlight))	;FIXME: other attrs
				   identifier))
    (if (not (= -1 identifier))
	(define-key nethack-menu-keymap (vector acc) 'nethack-menu-toggle-item)))
  'void)


;; end_menu(window, prompt) -- Stop adding entries to the menu and
;; flushes the window to the screen (brings to front?).  Prompt is a
;; prompt to give the user.  If prompt is NULL, no prompt will be
;; printed.  ** This probably shouldn't flush the window any more (if **
;; it ever did).  That should be select_menu's job.  -dean

(defun nethack-api-end-menu (window prompt)
  ""
  (save-excursion
    (set-buffer (nethack-buffer window))
    (ewoc-set-hf nethack-menu prompt "") ; might this clobber an existing footer?
    (ewoc-refresh nethack-menu)
    (ewoc-goto-node nethack-menu (ewoc-nth nethack-menu 0)))
  'void)


;; int select_menu(windid window, int how, menu_item **selected) --
;; Return the number of items selected; 0 if none were chosen, -1 when
;; explicitly cancelled.  If items were selected, then selected is filled
;; in with an allocated array of menu_item structures, one for each
;; selected line.  The caller must free this array when done with it.
;; The 'count' field of selected is a user supplied count.  If the user
;; did not supply a count, then the count field is filled with -1
;; (meaning all).  A count of zero is equivalent to not being selected
;; and should not be in the list.  If no items were selected, then
;; selected is NULL'ed out.  How is the mode of the menu.  Three valid
;; values are PICK_NONE, PICK_ONE, and PICK_ANY, meaning: nothing is
;; selectable, only one thing is selectable, and any number valid items
;; may selected.  If how is PICK_NONE, this function should never return
;; anything but 0 or -1.  -- You may call select_menu() on a window
;; multiple times -- the menu is saved until start_menu() or
;; destroy_nhwindow() is called on the window.  -- Note that NHW_MENU
;; windows need not have select_menu() called for them. There is no way
;; of knowing whether select_menu() will be called for the window at
;; create_nhwindow() time.

(defmacro nethack-protect-windows (&rest body)
  "Protect the status and message windows from obstruction by popup
menus." 
  `(progn
     (walk-windows (lambda (w)
		     (set-buffer (window-buffer w))
		     (if (or (eq nethack-buffer-type 'nhw-status)
			     (eq nethack-buffer-type 'nhw-message))
			 (set-window-dedicated-p w t))))
     (unwind-protect
	 (progn 
	   ,@body)
       (walk-windows (lambda (w)
		       (set-buffer (window-buffer w))
		       (if (or (eq nethack-buffer-type 'nhw-status)
			       (eq nethack-buffer-type 'nhw-message))
			   (set-window-dedicated-p w nil)))))))

(defun nethack-api-select-menu (winid how)
  ""
  (let ((buffer (nethack-buffer winid)))
    (if buffer
	(progn
	  (setq nethack-window-configuration (current-window-configuration))
	  (nethack-protect-windows
	   (pop-to-buffer (nethack-buffer winid) nil t))
	  (setq nethack-menu-how how)
	  (use-local-map nethack-menu-keymap))
      (error "No such winid: %d" winid)))
  'void)


;; char message_menu(char let, int how, const char *mesg) --
;; tty-specific hack to allow single line context-sensitive help to
;; behave compatibly with multi-line help menus.  -- This should only be
;; called when a prompt is active; it sends `mesg' to the message window.
;; For tty, it forces a --More-- prompt and enables `let' as a viable
;; keystroke for dismissing that prompt, so that the original prompt can
;; be answered from the message line 'help menu'.  -- Return value is
;; either `let', '\0' (no selection was made), or '\033' (explicit
;; cancellation was requested).  -- Interfaces which issue prompts and
;; messages to separate windows typically won't need this functionality,
;; so can substitute genl_message_menu (windows.c) instead.

(defun nethack-api-message-menu (let- how mesg)
  ""

  'unimplemented)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; D.  Misc. Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make_sound(???) -- To be determined later.  THIS IS CURRENTLY
;; UN-IMPLEMENTED.

(defun nethack-api-make-sound ()
  ""

  'unimplemented)


;; nhbell() -- Beep at user.  [This will exist at least until sounds
;; are redone, since sounds aren't attributable to windows anyway.]

(defun nethack-api-bell ()
  ""
  (ding)
  'void)


;; mark_synch() -- Don't go beyond this point in I/O on any channel
;; until all channels are caught up to here.  Can be an empty call for
;; the moment

(defun nethack-api-mark-synch ()
  "" 

  'unimplemented)


;; wait_synch() -- Wait until all pending output is complete
;; :flush for streams goes here:.  -- May also deal with exposure events
;; etc. so that the display is OK when return from wait_synch().

(defun nethack-api-wait-synch ()
  "Does nothing."
  'void)


;; delay_output() -- Causes a visible delay of 50ms in the output.
;; Conceptually, this is similar to wait_synch() followed by a nap(50ms),
;; but allows asynchronous operation.

(defun nethack-api-delay-output ()
  "Sleep for 50ms."
  ;; FIXME: sleep-for lets emacs process the nethack process input
  ;; stream and if there is any pending data on the stream
  ;; `nethack-process-filter' is called again which totally garbles
  ;; everything since it hasn't finished the first batch of commands
  (sleep-for 0 50)
  'dummy)


;; askname() -- Ask the user for a player name.

(defun nethack-api-askname ()
  ""

  'unimplemented)


;; cliparound(x, y)-- Make sure that the user is more-or-less centered
;; on the screen if the playing area is larger than the screen.  -- This
;; function is only defined if CLIPPING is defined.

(defun nethack-api-cliparound (x y)
  " FIXME: huh? not sure what to do here..."

  'void)


;; number_pad(state) -- Initialize the number pad to the given state.

(defun nethack-api-number-pad (state)
  ""

  'unimplemented)


;; suspend_nhwindows(str) -- Prepare the window to be suspended.

(defun nethack-api-suspend-nhwindows (str)
  ""  

  'unimplemented)


;; resume_nhwindows() -- Restore the windows after being suspended.

(defun nethack-api-resume-nhwindows ()
  ""

  'unimplemented)


;; start_screen() -- Only used on Unix tty ports, but must be declared
;; for completeness.  Sets up the tty to work in full-screen graphics
;; mode.  Look at win/tty/termcap.c for an example.  If your window-port
;; does not need this function just declare an empty function.

(defun nethack-api-start-screen ()
  ""

  'unimplemented)


;; end_screen() -- Only used on Unix tty ports, but must be declared
;; for completeness.  The complement of start_screen().

(defun nethack-api-end-screen ()
  ""

  'unimplemented)


;; outrip(winid, int) -- The tombstone code.  If you want the
;; traditional code use genl_outrip for the value and check the #if in
;; rip.c.

(defun nethack-api-outrip (window who int message)
  ""

  (save-excursion
    (set-buffer (nethack-buffer window))
    (insert (concat who " -- " message))
    'void))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; III.  Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following global variables are defined in decl.c and must be used by
;; the window interface to the rest of NetHack.

;;(defvar nethack-api-toplines ""
;;  "char toplines[BUFSZ] Contains the last message printed to the
;;WIN_MESSAGE window, used by Norep().")

;; winid WIN_MESSAGE, WIN_MAP, WIN_STATUS, WIN_INVEN
;;			The four standard windows.
;; (defvar nethack-api-win-message nil)
;; (defvar nethack-api-win-map nil)
;; (defvar nethack-api-win-status nil)
;; (defvar nethack-api-win-inven nil)

;; char *AE, *AS; Checked in options.c to see if we should switch to
;; DEC_GRAPHICS.  It is #ifdefed VMS and UNIX.
;; (defvar nethack-api-ae nil)
;; (defvar nethack-api-as nil)

;; int LI, CO; Set in sys/unix/ioctl.c.
;; (defvar nethack-api-li nil)
;; (defvar nethack-api-co nil)

;; The following appears to be Unix specific.  Other ports using the
;; tty window-port should also declare this variable in one of your
;; sys/*.c files.

;; (defvar nethack-api-ospeed nil
;;   " short ospeed; Set and declared in sys/unix/unixtty.c (don't know
;; about other sys files).")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IV.  New or respecified common, high level routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are not part of the interface, but mentioned here for your
;; information.

;; char display_inventory(lets, want_reply)
;; 		-- Calls a start_menu()/add_menu()/select_menu() sequence.
;; 		   It returns the item selected, or '\0' if none is selected.
;; 		   Returns '\033' if the menu was canceled.
;; raw_printf(str, ...)
;; 		-- Like raw_print(), but accepts arguments like printf().  This
;; 		   routine processes the arguments and then calls raw_print().
;; 		-- The mac version #defines error raw_printf.  I think this
;; 		   is a reasonable thing to do for most ports.
;; pline(str, ...)
;; 		-- Prints a string to WIN_MESSAGE using a printf() interface.
;; 		   It has the variants You(), Your(), Norep(), and others
;; 		   in pline.c which all use the same mechanism.  pline()
;; 		   requires the variable 'char toplines[]' be defined; Every
;; 		   putstr() on WIN_MESSAGE must copy str to toplines[] for use
;; 		   by Norep() and pline().  If the window system is not active
;; 		   (!iflags.window_inited) pline() uses raw_print().


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; V.  Game startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following is the general order in which calls from main() should be made,
;; as they relate to the window system.  The actual code may differ, but the
;; order of the calls should be the same.


;; choose_windows(DEFAULT_WINDOW_SYS) /* choose a default window system */
;; initoptions()			   /* read the resource file */
;; init_nhwindows()		   /* initialize the window system */
;; process_options(argc, argv)	   /* process command line options or equiv */
;; if(save file is present) {
;;   display_gamewindows()		   /* create & display the game windows */
;;   dorestore()			   /* restore old game; pline()s are OK */
;; } else {
;;   player_selection()		   /* select a player type using a window */
;;   display_gamewindows()		   /* create & display the game windows */
;; }
;; pline('Hello, welcome...');

;; Choose_windows() is a common routine, and calling it in main() is
;; necessary to initialize the function pointer table to _something_
;; so that calls to raw_print() will not fail.  Choose_windows()
;; should be called almost immediately upon entering main().  Look at
;; unixmain.c for an example.

;; Display_gamewindows() is a common routine that displays the three
;; standard game windows (WIN_MESSAGE, WIN_MAP, and WIN_STATUS).  It
;; is normally called just before the 'Hello, welcome' message.

;; Process_options() is currently still unique to each port.  There
;; may be need in the future to make it possible to replace this on a
;; per window-port basis.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VI.  Conventions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; init_nhwindows() is expected to display a gee-whiz banner window,
;; including the Copyright message.  It is recommended that the
;; COPYRIGHT_BANNER_A, COPYRIGHT_BANNER_B, and COPYRIGHT_BANNER_C
;; macros from patchlevel.h be used for constructing the Copyright
;; message.  COPYRIGHT_BANNER_A is a quoted string that has the
;; NetHack copyright declaration, COPYRIGHT_BANNER_B is a quoted
;; string that states who the copyright belongs to, and
;; COPYRIGHT_BANNER_C simply says 'See License for details.' Be sure
;; to #include 'patchlevel.h' to define these macros.  Using the
;; macros will prevent having to update the Copyright information in
;; each window-port prior to each release.

;; Ports (MSDOS, TOS, MAC, etc) _may_ use window-port specific
;; routines in their port specific files, _AT_THEIR_OWN_RISK_.  Since
;; 'port' and 'window-port' are orthogonal, you make your 'port' code
;; less portable by using 'window-port' specific routines.  Every
;; effort should be made to use window-port interface routines, unless
;; there is something port specific that is better suited
;; (e.g. msmsg() for MSDOS).

;; The tty window-port is contained in win/tty, the X window port is
;; contained in win/X11.  The files in these directories contain
;; _only_ window port code, and may be replaced completely by other
;; window ports.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VII.  Implementation and Multi-window support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NetHack 3.2 and higher support multiple window systems in the same
;; binary.  When writing a new window-port, you need to follow the
;; following guidelines:

;; 1) Pick a unique prefix to identify your window-port.  For example, the tty
;;    window port uses 'tty'; the X11 window-port uses 'X11'.
;; 2) When declaring your interface function, precede the function names with
;;    your unique prefix.  E.g:

;; 	void tty_init_nhwindows()
;; 	{
;; 		/* code for initializing windows in the tty port */
;; 	}

;;    When calling window functions from within your port code, we
;;    suggest calling the prefixed version to avoid unnecessary
;;    overhead.  However, you may safely call the non-prefixed version
;;    (e.g. putstr() rather than tty_putstr()) as long as you #include
;;    'hack.h'.  If you do not include hack.h and use the non-prefixed
;;    names, you will get compile or link-time errors.

;;    We also suggest declaring all functions and port-specific data
;;    with this prefix to avoid unexpected overlaps with other
;;    window-ports.  The tty and X11 ports do not currently follow
;;    this suggestion, but do use separate non-overlapping convention
;;    for naming data and internal functions.

;; 3) Declare a structure, 'struct window_procs prefix_procs', (with
;; your prefix instead of 'prefix') and fill in names of all of your
;; interface functions.  The first entry in this structure is the name
;; of your window-port, which should be the prefix.  The other entries
;; are the function addresses.

;;    Assuming that you followed the convention in (2), you can safely
;;    copy the structure definition from an existing window-port and
;;    just change the prefixes.  That will guarantee that you get the
;;    order of your initializations correct (not all compilers will
;;    catch out-of-order function pointer declarations).

;; 4) Add a #define to config.h identifying your window-port in the
;; 'Windowing systems' section.  Follow the 'prefix_GRAPHICS'
;; convention for your window-port.

;; 5) Add your prefix to the list of valid prefixes listed in the
;; 'Known systems are' comment.

;; 6) Edit makedefs.c and add a string for your windowing system to
;; window_opts inside an #ifdef prefix_GRAPHICS.

;; 7) Edit windows.c and add an external reference to your
;; prefix_procs inside an #ifdef prefix_GRAPHICS.  Also add an entry
;; to the win_choices structure for your window-port of the form:

;;     #ifdef prefix_GRAPHICS
;; 	{ &prefix_procs, prefix_init_function },
;;     #endif

;;    The init_function is necessary for some compilers and systems to
;;    force correct linking.  If your system does not need such
;;    massaging, you may put a null pointer here.

;;    You should declare prefix_procs and prefix_init_function as
;;    extern's in your win*.h file, and #include that file at the
;;    beginning of windows.c, also inside an #ifdef prefix_GRAPHICS.
;;    Some win*.h files are rather sensitive, and you might have to
;;    duplicate your prefix_procs and prefix_init_function's instead
;;    of including win*.h.  The tty port includes wintty.h, the X11
;;    port duplicates the declarations.

;; 8) If your port uses Makefile.src, add the .c and .o files and an
;; appropriate comment in the section on 'WINSRC' and 'WINOBJ'.  See
;; Makefile.src for the style to use.  If you don't use Makefile.src,
;; we suggest using a similar convention for the make-equivalent used
;; on your system.  Also add your new source and binaries to WINSRC
;; and WINOBJ (if you want the NetHack binary to include them, that
;; is).

;; 9) Look at your port's portmain.c (the file containing main()) and
;; make sure that all of the calls match the the requirements laid out
;; in Section V.

;; Now, proceed with compilation and installation as usual.  Don't
;; forget to edit Makefile.src (or its equivalent) and config.h to set
;; the window-ports you want in your binary, the default window-port
;; to use, and the .o's needed to build a valid game.

;; One caveat.  Unfortunately, if you incorrectly specify the
;; DEFAULT_WINDOW_SYS, NetHack will dump core (or whatever) without
;; printing any message, because raw_print() cannot function without
;; first setting the window-port.


(provide 'nethack-api)

;;; EOF
