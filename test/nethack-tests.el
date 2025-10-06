;;; nethack-tests.el --- Tests for nethack-el  -*- lexical-binding:t -*-

;; Copyright (C) 2025 George Huebner

;; Author: George Huebner <george@feyor.sh>
;; Maintainer: George Huebner <george@feyor.sh>
;; Created: Sun Oct 5 12:51:32 2025

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

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'nethack)


(defvar nethack--ert-test-directory nil)
(defvar nethack--ert-lisprec-filename (expand-file-name "test.lisprec.gz" (file-name-directory (or buffer-file-name load-file-name))))

(defmacro nethack--with-version (version &rest body)
  `(progn
     (unless (and nethack--ert-test-directory (file-exists-p nethack--ert-test-directory))
       (setq nethack--ert-test-directory (make-temp-file "nethack-ert" t)))
     (let* ((nethack-build-directory (expand-file-name ,version nethack--ert-test-directory))
            (nethack-program (expand-file-name (pcase system-type
                                                 ('windows-nt "binary/NetHack.exe")
                                                 (_ "games/nethack"))
                                               nethack-build-directory))
            compile-done
            (compilation-finish-functions `(,(lambda (&rest _) (setq compile-done t)))))
       (mkdir nethack-build-directory t)
       (unless (nethack-installed-p)
         (nethack-install ,version)
         (while (not compile-done) (accept-process-output)))
       ,@body)))

(ert-deftest nethack-test-install:3.6.7 ()
  (skip-when (eq system-type 'windows-nt))
  (nethack--with-version "3.6.7"
                         (should (nethack-installed-p))))

(ert-deftest nethack-test-install:3.7.0 ()
  (nethack--with-version "3.7.0"
                         (should (nethack-installed-p))))

(ert-deftest nethack-test-lisprec ()
  (nethack--with-version "3.7.0"
                         (save-current-buffer
                           (let* (nethack-use-tiles
                                  playback-done
                                  (nethack-lisprec-playback-finished-hook `(,(lambda () (setq playback-done t)))))
                             (nethack-lisprec-playback nethack--ert-lisprec-filename)
                             (nethack-lisprec-seek-to '(0 150 0 0))
                             (while (not playback-done) (accept-process-output))
                             (should (with-current-buffer nethack-raw-print-buffer-name (search-backward-regexp "You reached the 18th place on the top 100 list\\." nil t)))))))

(defun nethack-remote-server ()
  (make-network-process :name "nh-server"
                        :server t
                        :host 'local
                        :service t
                        :sentinel (lambda (proc status) (when (eq (process-status proc) 'open)
                                                          (process-send-string
                                                           proc
                                                           (string-trim-right
                                                            (shell-command-to-string nethack-program) " ([[:ascii:]]*"))))))

(ert-deftest nethack-test-remote ()
  (skip-unless (executable-find "nc"))
  (nethack--with-version "3.7.0"
                         (let ((nethack-server-proc (nethack-remote-server))
                               need-options-called)
                           (unwind-protect
                                (cl-letf (((symbol-function 'nethack-nhapi-need-options-file) (lambda () (setq need-options-called t))))
                                  (nethack-remote "nc %s %p" "localhost" (process-contact nethack-server-proc :service))
                                  (while (not (get-buffer nethack-log-buffer)) (accept-process-output))
                                  (should need-options-called))
                             (ignore-errors
                               (set-process-sentinel nethack-proc nil)
                               (delete-process nethack-proc)
                               (delete-process nethack-server-proc)
                               (nethack-kill-buffers))
                             (setq nethack-proc nil)))))



(provide 'nethack-tests)

;;; nethack-tests.el ends here
