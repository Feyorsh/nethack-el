;;; nethack.el --- Run Nethack as a subprocess -*- lexical-binding:t -*-

;; Copyright (C) 2003,2005  Ryan Yeske and Shawn Betts

;; Author: Ryan Yeske <rcyeske@vcn.bc.ca>
;; Maintainer: George Huebner <george@feyor.sh>
;; Created: Sat Mar 18 11:31:52 2000
;; Version: 0.15.0
;; Keywords: games
;; URL: https://github.com/Feyorsh/nethack-el

;; Package-Requires: ((emacs "27.1"))

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
;; Note: This package requires external libraries (if building from
;; source) and has been tested on Linux, macOS, and (NetHack
;; 3.7 only) Windows.
;;
;; Usage: M-x nethack

;;; Code:

(require 'nethack-compat)
(require 'nethack-api)
(require 'nethack-cmd)
(require 'nethack-keys)
(require 'nethack-options)
(require 'nethack-lisprec)
(require 'nethack-gen-tiles)

(require 'url)
(require 'dired-aux)
(require 'face-remap)

(defgroup nethack nil
  "Emacs Lisp frontend to the lisp window port of Nethack."
  :group 'games)

(defconst nethack-el-version "0.15.0")
(defconst nethack-el-earliest-compatible-version "0.13.0")
(defun nethack-el-version ()
  "Print version of nethack-el."
  (interactive)
  (message "nethack-el %s" nethack-el-version))

(defcustom nethack-status-window-height 4
  "Height of the status window."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-message-window-height 10
  "Height of the message window."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-status-highlight-delay
  (if-let ((op (cdr-safe (nethack-options-set-p "statushilites"))))
      (string-to-number op)
    5)
  "The number of turns to keep a changed status field highlighted."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-status-buffer-format
  "n s d c i W C A S\nl g hH pP a eED t Grfv"
  "Format string for the status in `nethack-status-buffer'."
  :type '(string)
  :group 'nethack)

(defcustom nethack-status-mode-line-format
  "s d c i W C g h p a e t"
  "Format string for the status on the mode-line."
  :type '(string)
  :group 'nethack)

(defcustom nethack-status-header-line-format
  "n <L,l> A   f"
  "Format string for the status on the header-line."
  :type '(string)
  :group 'nethack)

(defcustom nethack-status-style :buffer
  "Decides how the status will be displayed.
Valid values are :map, :header-line, :mode-line, or :buffer."
  :type '(symbol)
  :options '(:map :mode-line :header-line :buffer)
  :group 'nethack)

(defcustom nethack-purge-buffers t
  "When this variable is non-nil, kill all nethack buffers when nethack quits."
  :type '(boolean)
  :group 'nethack)

(defcustom nethack-want-completing-read t
  "When non-nil, prefer using `completing-read' to get input."
  :type '(boolean)
  :group 'nethack)

;;; Insert variables that control how the status gets displayed here.

(defcustom nethack-use-tiles nil
  "Name of XPM tileset to use when drawing the map and inventory."
  :type '(choice string (const nil))
  :options '("nethack367" "nethack370")
  :group 'nethack)

(defcustom nethack-map-mode-hook nil
  "Functions to be called after setting up the Nethack map."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-menu-mode-hook nil
  "Functions to be called after setting up a Nethack menu."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-before-print-message-hook nil
  "Hook run before a message is printed."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-end-hook nil
  "Hook run when nethack has ended."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-status-attribute-change-functions nil
  "List of functions to call after a status attribute change.
Three arguments are passed to each function: the name of the
attribute, the new value, the old value, and the percent."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-load-hook nil
  "Hook run after loading nethack."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-add-menu-hook nil
  "Hook run after a menu option has been added."
  :type '(hook)
  :group 'nethack)


(defgroup nethack-faces nil
  "Customizations for faces used by Enethack."
  :group 'nethack)

(defface nethack-message-highlight-face
  '((t (:foreground "black" :background "green")))
  "The face used to highlight new text in the message window."
  :group 'nethack-faces)

(defface nethack-atr-none-face
  `((t ()))
  "Nethack default face."
  :group 'nethack-faces)

(defface nethack-atr-dim-face
  `((t (:weight light)))
  "Nethack dim face."
  :group 'nethack-faces)

(defface nethack-atr-blink-face
  `((t (:inverse-video t)))
  "Nethack blink face."
  :group 'nethack-faces)

(defface nethack-atr-uline-face
  `((t (:underline t)))
  "Nethack underline face."
  :group 'nethack-faces)

(defface nethack-atr-inverse-face
  `((t (:inverse-video t)))
  "Nethack inverse face."
  :group 'nethack-faces)

(defface nethack-atr-bold-face
  `((t (:bold t)))
  "Nethack bold face."
  :group 'nethack-faces)

(defface nethack-black-face
  `((t (:foreground "dark blue")))
  "Nethack black face."
  :group 'nethack-faces)

(defface nethack-red-face
  `((((type tty) (class color))
     (:foreground "red"))
    (((class color))
     (:foreground "red"))
    (t (:foreground "gray")))
  "Nethack red face."
  :group 'nethack-faces)

(defface nethack-green-face
  `((((type tty) (class color))
     (:foreground "green"))
    (((class color) (background dark))
     (:foreground "lime green"))
    (((class color) (background light))
     (:foreground "lime green"))
    (t (:foreground "gray")))
  "Nethack green face."
  :group 'nethack-faces)

(defface nethack-brown-face
  `((((type tty) (class color))
     (:foreground "yellow"))
    (((class color) (background dark))
     (:foreground "chocolate"))
    (((class color) (background light))
     (:foreground "brown"))
    (t (:foreground "gray")))
  "Nethack brown face."
  :group 'nethack-faces)

(defface nethack-blue-face
  `((((type tty) (class color))
     (:foreground "blue"))
    (((class color) (background dark))
     (:foreground "dark blue"))
    (((class color) (background light))
     (:foreground "dark blue"))
    (t (:foreground "gray")))
  "Nethack blue face."
  :group 'nethack-faces)

(defface nethack-magenta-face
  `((((type tty) (class color))
     (:foreground "magenta"))
    (((class color) (background dark))
     (:foreground "dark magenta"))
    (((class color) (background light))
     (:foreground "dark magenta"))
    (t (:foreground "gray")))
  "Nethack magenta face."
  :group 'nethack-faces)

(defface nethack-cyan-face
  `((((type tty) (class color))
     (:foreground "cyan"))
    (((class color) (background dark))
     (:foreground "dark cyan"))
    (((class color) (background light))
     (:foreground "cyan4"))
    (t (:foreground "gray")))
  "Nethack cyan face."
  :group 'nethack-faces)

(defface nethack-gray-face
  `((((type tty) (class color))
     (:foreground "white"))
    (((class color) (background dark))
     (:foreground "lightgray"))
    (((class color) (background light))
     (:foreground "darkgray"))
    (t (:foreground "gray")))
  "Nethack gray face."
  :group 'nethack-faces)

(defface nethack-dark-gray-face
  `((((type tty) (class color))
     (:foreground "black" :bold t))
    (((class color) (background dark))
     (:foreground "darkgray"))
    (((class color) (background light))
     (:foreground "lightgray"))
    (t (:foreground "gray")))
  "Nethack dark gray face."
  :group 'nethack-faces)

(defface nethack-orange-face
  `((((type tty) (class color))
     (:foreground "red" :bold t))
    (((class color))
     (:foreground "orange"))
    (t (:foreground "gray")))
  "Nethack light orange face."
  :group 'nethack-faces)

(defface nethack-bright-green-face
  `((((type tty) (class color))
     (:foreground "green" :bold t))
    (((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "dark green"))
    (t (:foreground "gray")))
  "Nethack bright green face."
  :group 'nethack-faces)

(defface nethack-yellow-face
  `((((type tty) (class color))
     (:foreground "yellow" :bold t))
    (((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "yellow3"))
    (t (:foreground "gray")))
  "Nethack yellow face."
  :group 'nethack-faces)

(defface nethack-bright-blue-face
  `((((type tty) (class color))
     (:foreground "blue" :bold t))
    (((class color) (background dark))
     (:foreground "blue"))
    (((class color) (background light))
     (:foreground "blue"))
    (t (:foreground "gray")))
  "Nethack bright blue face."
  :group 'nethack-faces)

(defface nethack-bright-magenta-face
  `((((type tty) (class color))
     (:foreground "magenta" :bold t))
    (((class color))
     (:foreground "magenta"))
    (t (:foreground "gray")))
  "Nethack bright magenta face."
  :group 'nethack-faces)

(defface nethack-bright-cyan-face
  `((((type tty) (class color))
     (:foreground "cyan" :bold t))
    (((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "cyan3"))
    (t (:foreground "gray")))
  "Nethack bright cyan face."
  :group 'nethack-faces)

(defface nethack-white-face
  `((((type tty) (class color))
     (:foreground "white" :bold t))
    (((class color) (background dark))
     (:foreground "white"))
    (((class color) (background light))
     (:foreground "black"))
    (t (:foreground "gray")))
  "Nethack white face."
  :group 'nethack-faces)

(defcustom nethack-mono-font-family nil
  "Monospace font family to use for `nethack-faces'."
  :type '(choice string (const nil))
  :set (lambda (var val)
         (set-default var val)
         (when (fboundp #'nethack-faces-setup-mono-font)
           (nethack-faces-setup-mono-font)))
  :group 'nethack-faces)


(defun nethack-faces-find-mono-fonts ()
  "List of monospaced fonts that probably work with Nethack."
  (when (display-graphic-p)
    (set-face-attribute (make-empty-face 'tmp-face) nil :fontset "fontset-nethackmono")
    (unwind-protect
         (seq-filter (lambda (ffamily)
                       ;; "⌡" is used because it's not in DEC/IBM
                       ;; but is representative of characters that are.
                       ;; Unfortunately we cannot programmatically
                       ;; test that box drawing characters are
                       ;; rendered correctly---remedy is for the user
                       ;; to set nethack-mono-font-family
                       (set-fontset-font "fontset-nethackmono" ?⌡ (font-spec :family ffamily))
                       (when-let ((info (font-info ffamily))
                                  (rendered-font (font-at 0 nil #("⌡" 0 1 (face tmp-face)))))
                         (and (string= (font-get rendered-font :family) ffamily)
                              (string-match-p "spacing=100" (aref info 1)))))
                     (font-family-list))
      (unintern 'tmp-face nil))))

(defun nethack-faces-setup-mono-font ()
  "Setup `nethack-faces' to use a monospaced font.

`nethack-mono-font-family' is used if not nil, otherwise search for a
monospace Unicode font on the user's system.  This rigmarole is
necessary to avoid DEC/IBMgraphics making lines shift over slightly."
  (when (display-graphic-p)
    (create-fontset-from-fontset-spec "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-nethackmono")
    (set-fontset-font "fontset-nethackmono" 'unicode (font-spec :family (or nethack-mono-font-family
                                                                            (car (nethack-faces-find-mono-fonts)))))
    (mapcan (lambda (f) (set-face-attribute f nil :fontset "fontset-nethackmono")) nethack-colors)))

(nethack-faces-setup-mono-font)



;;; Installation

(defconst nethack-el-directory
  (file-name-as-directory (or (and load-file-name
                                   (file-name-directory load-file-name))
                              default-directory))
  "The directory from where this library was first loaded.")

(defcustom nethack-build-directory
  (expand-file-name "build" nethack-el-directory)
  "The directory in which to build nethack.
You can influence the location of the build directory by setting this
variable.
If `nethack-program' is set to a working compatible version of NetHack
complied with the lisp patch, then `nethack-build-directory' is never
used."
  :type '(string)
  :group 'nethack)

(defcustom nethack-program
  (expand-file-name (pcase system-type
                      ('windows-nt "binary/NetHack.exe")
                      (_ "games/nethack"))
                    nethack-build-directory)
  "Program to run to start a game of Nethack.
If this variable is custom-set outside of the default
`nethack-build-directory', and it does indeed point to a working
compatible version of NetHack with the lisp patch, then
`nethack-build-directory' is never consulted during
installation."
  :type '(string)
  :group 'nethack)

(defcustom nethack-program-args nil
  "Arguments to pass to `nethack-program'."
  :type '(repeat string)
  :group 'nethack)

(defcustom nethack-environment nil
  "Additional environment variables to pass to NetHack process.
See https://nethackwiki.com/wiki/Environment_variable for more information."
  :type '(repeat string)
  :group 'nethack)

(defcustom nethack-wizmode nil
  "Whether NetHack should be launched in wizard (debug) mode."
  :type '(boolean)
  :group 'nethack)

(defcustom nethack-version
  "3.6.7"
  "The NetHack version to download, install, and bulid."
  :group 'nethack
  :type 'string)

;; It might be a bad, bad practice to make these functions, but it made sense at
;; the time.
(defun nethack-version-nodots ()
  "The NetHack version without separating dots."
  (replace-regexp-in-string "\\." "" nethack-version))

(defun nethack-query-for-version ()
  "Queries the user for the NetHack version.

The two supported versions are 3.6.7 and 3.7.0, although the user can
input a different version at their own peril."
  (interactive)
  (completing-read "NetHack version: " '("3.6.7" "3.7.0") nil nil))

(defun nethack-installed-p ()
  "Determine if a patched NetHack is installed.
Checks whether a NetHack executable exists, and if running it results
in an output with prefix ``(nethack-nhapi-raw-print'' with the correct
NetHack version and the correct version for the lisp-patch."
  (and nethack-program
       (file-executable-p nethack-program)
       (let ((version-string
              (shell-command-to-string
               (concat nethack-program " --version"))))
         (version<=
          nethack-el-earliest-compatible-version
          (and (string-match
                (concat "NetHack Version "
                        "[0-9]+\\.[0-9]+\\.[0-9]+\\(?:-[0-9]+\\)?"
                        " lisp-patch "
                        "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)")
                version-string)
               (match-string-no-properties 1 version-string))))))

(defun nethack-build (&optional no-download-p)
  "Build the NetHack program in the background.
If CALLBACK is non-nil, it should be a function.  It is called
with the compiled executable as the single argument or nil, if
the build failed.

If NO-DOWNLOAD-P is non-nil, then no NetHack tarball will be
downloaded and the source will be assumed to be in
`nethack-build-directory'.

If BUILD-DIRECTORY is non-nil, then `nethack-build-directory'
will be set to BUILD-DIRECTORY.  The NetHack executable will be
located within the BUILD-DIRECTORY.

Returns the buffer of the compilation process."
  (unless no-download-p
    (let ((default-directory (make-temp-file "nethack-el" t)))
      (nethack-build-download)
      (nethack-build-untar)
      (delete-directory nethack-build-directory t)
      (rename-file "build" nethack-build-directory)))
  (let ((default-directory nethack-build-directory))
    (nethack-build-patch)
    (nethack-build-setup)
    (nethack-build-tiles)
    (nethack-build-compile)))

(defun nethack-build-download ()
  "Download the nethack source from nethack.org.
The source is saved as nethack.tar.gz within the
`default-directory'."
  (let ((download-url (concat "https://github.com/NetHack/NetHack/archive/NetHack-"
                              (pcase nethack-version
                                ("3.6.7" "3.6.7_Released")
                                ("3.7.0" "3.7")
                                (_ (user-error "Unsupported NetHack version %s" nethack-version)))
                              ".tar.gz")))
    (url-copy-file download-url (expand-file-name "nethack.tar.gz") t)))

(defun nethack-build-untar ()
  "Decompress nethack source from nethack.tar.gz to build."
  (dired-compress-file "nethack.tar.gz")
  (delete-file "nethack.tar.gz")
  (rename-file (car (file-expand-wildcards "NetHack*")) "build"))

(defmacro nethack-build--replace-regexp-in-file (filename regex to-string)
  `(with-temp-buffer
    (setq buffer-file-name ,filename)
    (revert-buffer t t)
    (while (re-search-forward ,regex nil t)
      (replace-match ,to-string nil nil))
    (save-buffer)
    (kill-buffer)))

(defun nethack-build-patch ()
  "Patch the NetHack source with lisp patches."
  ;; patch -Nr- -p1 < ../../enh-$(NH_VER_NODOTS).patch || true
  (process-file-shell-command
   "patch -Nr- -p1"
   (concat nethack-el-directory "enh-" (nethack-version-nodots) ".patch"))
  ;; patches that do not relate to the lisp port but are otherwise required to build/run nethack go here
  ;; we can assume we have grep and gzip, but we should not assume they're in /bin
  (nethack-build--replace-regexp-in-file "sys/unix/hints/lisp" "/bin/gzip" (executable-find "gzip"))
  (nethack-build--replace-regexp-in-file "sys/unix/sysconf" "/bin/grep" (executable-find "grep"))
  (nethack-build--replace-regexp-in-file "sys/unix/sysconf" "GDBPATH" "#GDBPATH")
  (when (eq system-type 'darwin)
    ;; fix compiler error on 3.6.7, is harmless on 3.7
    (nethack-build--replace-regexp-in-file "include/tradstdc.h" "\\(define \\(?:__\\)?warn_unused_result\\(?:__\\)?\\) .*" "\\1 __unused__")))

(defun nethack-build-setup ()
  "Run any pre-build setup before building NetHack."
  ;; cd sys/unix && $(SHELL) ./setup.sh hints/linux-lisp
  ;; or on windows,
  ;; cp sys/windows/GNUmakefile* src/
  (pcase system-type
    ('windows-nt (mapcar (lambda (f) (copy-file f (expand-file-name (concat "src/" (file-name-nondirectory f))))) (file-expand-wildcards (expand-file-name "sys/windows/GNUmakefile*"))))
    (_ (let ((default-directory (expand-file-name "sys/unix")))
         (process-file-shell-command "./setup.sh hints/lisp")))))

(defun nethack-build-compile ()
  "Compile NetHack with make.
CALLBACK is called when the compilation finishes (with no
arguments).

Returns the buffer of the compilation process.

Requires `make', `gcc', `bison' or `yacc', `flex' or `lex', and
the ncurses-dev library for your system."
  ;; make fetch-lua && make install
  (let* ((default-directory (if (eq system-type 'windows-nt) (expand-file-name "src") default-directory))
         (compilation-cmd
          ;; Right now, since there are two make arguments passed here, the
          ;; comint mode sees this as two different compiles and gives messages
          ;; in the order:  "Comint finished, Building the NetHack program
          ;; succeeded, Comint finished".  This is maybe a little bad as it
          ;; obscures the message that the build is done.  Still, it works for
          ;; now, so I'll just need to remember that it's currently a little
          ;; HACK-y.
          (concat (when (string= nethack-version "3.7.0")
		    (format "make fetch%slua && " (if (eq system-type 'windows-nt) "" "-")))
                  "make PREFIX=" nethack-build-directory
                  (unless (eq system-type 'windows-nt) " all install")))
         (compilation-buffer
          (compilation-start compilation-cmd t))) ; Use compilation-shell-minor-mode

    (if (get-buffer-window compilation-buffer)
        (select-window (get-buffer-window compilation-buffer))
      (pop-to-buffer compilation-buffer)
      (with-current-buffer compilation-buffer
        (setq-local compilation-error-regexp-alist nil)
        (current-buffer)))))

(defun nethack-build-tiles ()
  "Compile nethackVER-tiles.el.gz from the XPMs in the nethack directory."
  (let ((nethack-source-directory default-directory)
        (default-directory (file-name-directory (directory-file-name default-directory))))
    (nethack-gen-tiles nethack-source-directory (concat "nethack" (nethack-version-nodots)))))


;;; Initialization

;;;###autoload
(defun nethack-install (&optional version no-download-p)
  "Download, install, and patch nethack.
If the `nethack-program' is not running or does not appear to be
working, attempt to rebuild it.  If this build succeeded,
continue with the activation of the package.  Otherwise fail
silently, that is, no error is is signaled.

If VERSION is nil, ask the user if they want to build nethack and if
so, what version.

Do not download (but do untar) if NO-DOWNLOAD-P is non-nil."
  (interactive)
  (unless (nethack-installed-p)
    (if (or version
            (y-or-n-p "Need to (re)build the NetHack program, do it now?"))
        (progn
          (setq-default nethack-version (or version (nethack-query-for-version)))
          (when (and (eq system-type 'windows-nt) (not (string= nethack-version "3.7.0")))
            (user-error "NetHack version %s is not supported on Windows" nethack-version))
          (nethack-build no-download-p))
      (message "NetHack not activated"))))


;;; Process
(defvar nethack-proc nil)
(defvar nethack-proc-buffer-name "*nethack-output*")
(defvar nethack-proc-kill-buffer-on-quit t
  "When the process ends kill the process buffer if this is t.")
(defvar nethack-log-buffer "*nethack-log*")
(defvar nethack-start-time nil)

;;;###autoload
(defun nethack ()
  "Start a game of Nethack.
The variable `nethack-program' is the name of the executable to run."
  (interactive)
  (if (nethack-installed-p)
      (if (nethack-is-running)
          (progn
            (message "NetHack process already running...")
            (nethack-restore-windows))
        ;; Start the process.
        (nethack-kill-buffers)
        (when (get-buffer nethack-proc-buffer-name)
          (kill-buffer nethack-proc-buffer-name))
        (nethack-start (let ((process-environment (append (when nethack-wizmode `(,(concat "NETHACKOPTIONS=@" nethack-options-file))) nethack-environment process-environment))
                             (default-directory (concat (when (and nethack-wizmode (not (eq system-type 'windows-nt))) "/sudo::") default-directory))
                             (nethack-program-args (append (when nethack-wizmode '("-D" "-u" "wizard")) nethack-program-args)))
                         (apply #'start-file-process "nh" nethack-proc-buffer-name
                                nethack-program nethack-program-args))))
    (nethack-install)))

;;;###autoload
(defun nethack-remote (connection-command &optional host port)
  "Start a game of Nethack with CONNECTION-COMMAND.

CONNECTION-COMMAND is a format string with two specs, %s for host and
%p for port.

As an example, one could login to NAO with (nethack-remote \"ssh -o
SetEnv=DGLAUTH=username:password nethack@%s\" \"nethack.alt.org\"),
where \"nethack.alt.org\" could be substituted for \"hardfought.org\".
"
  (interactive (let* ((servers '("nethack.alt.org" "hardfought.org"))
                      (protocols '(("SSH" . "ssh -o SetEnv=DGLAUTH=%d nethack@%s")
                                   ("Telnet" . "telnet -l %d %s")))
                      (protocol (let ((completion-extra-properties
                                        (list :annotation-function
                                              (lambda (p)
                                                (format #("%s    %s" 6 8 (face font-lock-doc-face))
                                                        (make-string (- (apply #'max (mapcar (lambda (e) (length (car e))) protocols)) (length (car (assoc-string p protocols)))) ? )
                                                        (cdr (assoc-string p protocols)))))))
                                   (or (completing-read "Protocol (or command to connect): " protocols nil 'confirm-after-completion)))))
                 (when (string-match-p "%d" protocol)
                   (setq protocol (format-spec protocol
                                               `((?d . ,(or (getenv "DGLAUTH")
                                                            (format "%s:%s"
                                                                    (read-string "Username: ")
                                                                    (read-passwd "Password: "))))))))
                 (list protocol
                       (when (string-match-p "%s" protocol)
                         (completing-read "Server: " servers nil nil))
                       (when (string-match-p "%p" protocol)
                         (read-number "Port: ")))))
  (if (nethack-is-running)
      (progn
        (message "NetHack process already running...")
        (nethack-restore-windows))
    (nethack-kill-buffers)
    (when (get-buffer nethack-proc-buffer-name)
      (kill-buffer nethack-proc-buffer-name))
    (get-buffer-create nethack-proc-buffer-name)
    (nethack-start (open-network-stream "nh" nethack-proc-buffer-name
                          host port :type 'shell :shell-command connection-command))))

(defun nethack-is-running ()
  "Return T if nethack is already running."
  (and (processp nethack-proc)
       (member (process-status nethack-proc) '(open run))))

(defun nethack-start (process)
  "Given PROCESS, start nethack.
Assumes nethack is not already running."
  (save-excursion
    (setq nethack-proc process)
    (setq nethack-lisprec--timer nil)
    (setq nethack-start-time (current-time))
    (nethack-reset-status-variables)
    (set-process-filter nethack-proc #'nethack-filter)
    (set-process-sentinel nethack-proc #'nethack-sentinel)))

(defvar nethack-use-tiles--tileset nil)
(defvar nethack-use-tiles--font-height nil)
(defun nethack-toggle-tiles ()
  "Toggle the use of tiles on the map."
  (interactive)
  (if nethack-use-tiles
      (progn
        (setq nethack-use-tiles--tileset nethack-use-tiles)
        (with-current-buffer nethack-map-buffer
          (face-remap-remove-relative nethack-use-tiles--font-height))
        (setq nethack-use-tiles nil))
    (setq nethack-use-tiles (or nethack-use-tiles--tileset (concat "nethack" (nethack-version-nodots))))
    (with-current-buffer nethack-map-buffer (nethack-map-mode)))
  (nethack-command-redraw-screen 2))

(defvar nethack-tiles-scale nil)
(defun nethack-rescale-tiles ()
  (save-excursion
    (with-current-buffer nethack-map-buffer
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((image (get-text-property (point) 'display)))
          (image--set-property image :scale nethack-tiles-scale))
        (goto-char (next-single-char-property-change (point) 'display))))
    (clear-image-cache)))

(defun nethack-text-scale-increase (inc)
  (interactive "p")
  (unless (derived-mode-p 'nethack-map-mode)
    (error "Not in NetHack map mode"))
  (if (eq (next-single-char-property-change (point-min) 'display) (point-max))
      (text-scale-increase inc)
    (setq nethack-tiles-scale (min 8.0 (ffloor (+ nethack-tiles-scale inc))))
    (nethack-rescale-tiles)))

(defun nethack-text-scale-decrease (dec)
  (interactive "p")
  (unless (derived-mode-p 'nethack-map-mode)
    (error "Not in NetHack map mode"))
  (if (eq (next-single-char-property-change (point-min) 'display) (point-max))
      (text-scale-decrease dec)
    (setq nethack-tiles-scale (max 0.5 (- nethack-tiles-scale dec)))
    (nethack-rescale-tiles)))

;;;; Process code to communicate with the Nethack executable
(defconst nethack-prompt-regexp
  "^\\(command\\|menu\\|dummy\\|direction\\|number\\|string\\)> *")

(define-error 'nethack-illegal-function "Illegal output from nethack process detected")
(defun nethack-safe-eval-region (start end)
  "Evaluate region, only allowing nethack-nhapi-* functions.

This is not totally foolproof, but it serves as a first line of
defense against malicious input."
  (let ((current-buf (current-buffer))
        (nhapi-file (locate-library "nethack-api"))
        (sexps nil)
        (start-pos 0))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert-buffer-substring-no-properties current-buf start end)
      (catch 'done
        (while t
          (let* ((parse-sexp-ignore-comments t)
                 (sexp-end (scan-sexps start-pos 1))
                 (sexp-start (when sexp-end (scan-sexps sexp-end -1))))
            (when (or (null sexp-end) (>= sexp-end (point-max)))
              (throw 'done nil))
            (setq sexps (cons (buffer-substring sexp-start sexp-end) sexps))
            (setq start-pos sexp-end)))))
    (mapc (lambda (s)
              ;; ensure every function is defined in nethack-api.el, failing shut on errors
              (unless (ignore-errors
                        (let ((func (ignore-error end-of-file (caar (read-from-string s)))))
                          (or (null func) (string= (symbol-file func) nhapi-file))))
                ;; do not terminate process in case this is a false positive
                (signal 'nethack-illegal-function s)))
            sexps)
    (eval-region start end)))

(defun nethack-sentinel (proc msg)
  "Nethack background process sentinel.
PROC is the process object and MSG is the exit message."
  (with-current-buffer (process-buffer proc)
    (nethack-log (buffer-substring (point-min) (point)))
    (nethack-safe-eval-region (point-min) (point-max))
    (insert "Nethack " msg))
    ;; (when (not (string-equal msg "Nethack finished"))
    ;;    (pop-to-buffer (current-buffer)))

  (delete-process proc)
  (unless (or (not nethack-lisprec-record) (string-match-p "exited abnormally" msg))
    (let ((save-silently)
          (filename (format-time-string "%F.%T.lisprec"))
          (dired-compress-file-default-suffix ".gz"))
      (with-current-buffer nethack-log-buffer
        (append-to-file nil nil filename))
      (dired-compress-file filename)
      (message "Recorded to %s.gz" filename)))
  (when nethack-proc-kill-buffer-on-quit
    (kill-buffer (get-buffer nethack-proc-buffer-name)))
  (when nethack-purge-buffers
    (nethack-kill-buffers))
  (let ((raw-print-buffer (get-buffer nethack-raw-print-buffer-name)))
    (when raw-print-buffer
      (switch-to-buffer raw-print-buffer))))

(defun nethack-log (string &optional print-timestamp)
  (with-current-buffer (get-buffer-create nethack-log-buffer)
    (goto-char (point-max))
    (insert (string-trim string))
    (when print-timestamp
      (insert (format " ; %s" (time-subtract (current-time) nethack-start-time))))
    (insert "\n")))

(defvar nethack-at-prompt nil)
(defvar nethack-at-prompt-hook nil
  "Called when there is a prompt.
Takes one arg: the kind of prompt.
Either \"command\" or \"menu\"")
(defun nethack-filter (proc string)
  "Insert contents of STRING into the buffer associated with PROC.
Evaluate the buffer contents if we are looking at a prompt and then
delete the contents, perhaps logging the text."
  ;; insert output into process buffer
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (forward-line 0)
    (when (looking-at nethack-prompt-regexp)
      (let ((prompt (match-string 1)))
        (nethack-log (buffer-substring (point-min) (point)) t)
        (save-restriction
          (narrow-to-region (point-min) (point))
          (nethack-safe-eval-region (point-min) (point-max)))
        (cond ((or (equal prompt "command")
                   (equal prompt "menu")
                   (equal prompt "dummy"))
               ;; I Don't think we need this...
               ;; (nethack-nhapi-print-status)
               (sit-for 0)
               (setq nethack-at-prompt t)
               (run-hook-with-args 'nethack-at-prompt-hook prompt)))))))

(defun nethack-send (form)
  (when nethack-proc
    (let ((command (cond
                     ((null form) "()") ; the process doesn't handle `nil'
                     ((stringp form) form)
                     (t (prin1-to-string form)))))
      (with-current-buffer (process-buffer nethack-proc) (erase-buffer))
      (process-send-string nethack-proc (concat command "\n"))
      (nethack-log (format ";;; %s\n" command)))))

(defun nethack-send-and-wait (form)
  (when nethack-proc
    (nethack-send form)
    ;; wait until we get back to a "command" prompt before returning
    (setq nethack-at-prompt nil)
    (while (and (member (process-status nethack-proc) '(open run))
                (not nethack-at-prompt))
      (accept-process-output nethack-proc))))

(defun nethack-restore-windows ()
  "Restore NetHack window layout."
  (interactive)
  (when nethack--window-configuration-before
    (nethack-nhapi-restore-window-configuration)))

;;; Buffer code (aka windows in Nethack)
(defvar nethack-map-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "w   " table)
    (modify-syntax-entry ?\) "w   " table)
    (modify-syntax-entry ?\[ "w   " table)
    (modify-syntax-entry ?\] "w   " table)
    ;; Although the backslash isn't strictly necessary, it's needed so that
    ;; parinfer-rust-mode won't error here
    (modify-syntax-entry ?\{ "w   " table)
    (modify-syntax-entry ?\} "w   " table)
    table)
  "Syntax table used in the Nethack map.")

(define-derived-mode nethack-map-mode nil "NetHack Map"
  "Major mode for the main Nethack map window.

\\{nethack-map-mode-map}"
  (use-local-map nethack-map-mode-map)
  (set-syntax-table nethack-map-mode-syntax-table)
  ;; make scroll-other-window work on the message buffer
  (setq-local other-window-scroll-buffer nethack-message-buffer)
  (setq-local scroll-conservatively 0)  ; recenter
  (setq-local scroll-margin 3)
  (setq-local cursor-in-non-selected-windows nil)
  (when (and nethack-use-tiles (display-graphic-p))
    (let ((cookie (face-remap-add-relative 'default :height 8)))
      (setq nethack-use-tiles--font-height (or nethack-use-tiles--font-height cookie)))
    ;; for not clobbering our keybindings, and preventing the user
    ;; from accidentally messing up the map tiles
    (setq-local image-map (make-sparse-keymap)))
  (run-hooks 'nethack-map-mode-hook))

(define-derived-mode nethack-message-mode text-mode "NetHack Messages"
  "Major mode for the Nethack message window."
  (setq buffer-read-only t))
(put #'nethack-message-mode 'mode-class 'special)

(define-derived-mode nethack-status-mode nil "NetHack Status"
  "Major mode for the Nethack status window."
  (setq buffer-read-only t))
(put #'nethack-status-mode 'mode-class 'special)

(defun nethack-kill-buffers ()
  "Kill all nethack associated buffers except the nethack process buffer."
  (dolist (buf (list nethack-log-buffer nethack-map-buffer nethack-status-buffer nethack-message-buffer))
    (when (and buf (buffer-live-p (get-buffer buf))) (kill-buffer buf)))
  (mapc (lambda (x) (when (buffer-live-p (cdr x))
                      (kill-buffer (cdr x))))
        nethack-menu-buffer-table))


(run-hooks 'nethack-load-hook)

(provide 'nethack)

;;; nethack.el ends here
