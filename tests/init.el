;;; init.el --- Testing -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025 Campbell Barton <ideasman42@gmail.com>

;;; Commentary:

;; The Emacs configuration used when running tests.
;; See: `repeat_fu_tests.py' for launching Emacs with this file.

;;; Code:

;; Load `repeat-fu' from this repository, never an installed copy.
(add-to-list 'load-path (file-name-concat (file-name-directory load-file-name) ".."))

(require 'repeat-fu)

;; NOTE: the preset is not set here, each test binds `repeat-fu-backend' to the
;; preset it covers.  Tests must not depend on each other's configuration.


;; ---------------------------------------------------------------------------
;; Key-map
;;
;; Tests press keys, they never call the commands under test.  What `repeat-fu'
;; records & replays *is* the key sequence, so a test that called the command
;; would exercise a path a user can't reach.
;;
;; Function keys are used since they don't shadow anything tests may type.

(defconst repeat-fu-test-key-execute (kbd "<f5>")
  "The key bound to `repeat-fu-execute'.")
(defconst repeat-fu-test-key-copy-to-last-kbd-macro (kbd "<f6>")
  "The key bound to `repeat-fu-copy-to-last-kbd-macro'.")
(defconst repeat-fu-test-key-listener-register (kbd "<f7>")
  "The key bound to `repeat-fu-test-listener-register'.")
(defconst repeat-fu-test-key-insert-x (kbd "<f8>")
  "The key bound to `repeat-fu-test-insert-x'.")
(defconst repeat-fu-test-key-insert-y (kbd "<f9>")
  "The key bound to `repeat-fu-test-insert-y'.")

(global-set-key repeat-fu-test-key-execute 'repeat-fu-execute)
(global-set-key repeat-fu-test-key-copy-to-last-kbd-macro 'repeat-fu-copy-to-last-kbd-macro)
(global-set-key repeat-fu-test-key-listener-register 'repeat-fu-test-listener-register)
(global-set-key repeat-fu-test-key-insert-x 'repeat-fu-test-insert-x)
(global-set-key repeat-fu-test-key-insert-y 'repeat-fu-test-insert-y)

;; Commands for `repeat-fu-declare' tests to declare properties on.  There are
;; two because a declaration cannot be revoked, so a test sharing a command with
;; another would inherit whatever it declared.
(defun repeat-fu-test-insert-x ()
  "Insert \"x\"."
  (interactive)
  (insert "x"))

(defun repeat-fu-test-insert-y ()
  "Insert \"y\"."
  (interactive)
  (insert "y"))

(defvar repeat-fu-test-listener-token nil
  "Token stored by `repeat-fu-test-listener-register'.")

;; A listener registered from a command, which is how external code uses the
;; API.  Registering straight from a test body would skip the case the listener
;; must handle - the command doing the registering being excluded.
(defun repeat-fu-test-listener-register ()
  "Register a listener, storing its token for the test to collect from."
  (interactive)
  (setq repeat-fu-test-listener-token (repeat-fu-listener-register)))


;; ---------------------------------------------------------------------------
;; Emacs Defaults

(setq inhibit-startup-screen t)

;; TODO: no test uses the region yet.  Repeating a change made over one is worth
;; covering (`repeat-fu-declare' has a `:skip-active' key for it), so the configuration
;; those tests need is set up ready for them.

;; Don't nag on use of the inactive region.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Disabled by default in batch mode, needed for region tests to work.
(transient-mark-mode 1)

;; Just avoid added overhead.
(global-eldoc-mode 0)

;; This causes a pre-command hook to run that prints a message before each macro.
;; Harmless but noisy and unnecessary for tests.
(tooltip-mode 0)

(setq ring-bell-function #'ignore)

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
