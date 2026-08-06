;;; repeat_fu_tests.el --- Testing -*- lexical-binding: t; coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025 Campbell Barton <ideasman42@gmail.com>

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-repeat-fu
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Integration tests for recording & replaying commands with `repeat-fu'.
;; Tests run in batch mode via the `repeat_fu_tests.py' wrapper.

;;; Usage

;; Run via: python3 tests/repeat_fu_tests.py

;;; Code:

(require 'ert)
(require 'repeat-fu)
(require 'repeat-fu-preset-multi)
(require 'repeat-fu-preset-single)


;; ---------------------------------------------------------------------------
;; Message Capture (suppress minibuffer noise during tests)

(defvar repeat-fu-test--captured-messages nil
  "List of messages captured during test execution (newest first).")

(defun repeat-fu-test--message-capture (format-string &rest args)
  "Capture message instead of displaying.
FORMAT-STRING & ARGS match `message', the formatted message is
stored in `repeat-fu-test--captured-messages'."
  (when format-string
    (push (apply #'format format-string args) repeat-fu-test--captured-messages))
  ;; Return nil like `message' does when format-string is nil.
  nil)

(defvar repeat-fu-test--messages-reported nil
  "The captured messages a test last read, see `with-repeat-fu-test'.")

(defun repeat-fu-test-messages ()
  "Return captured messages in chronological order."
  (setq repeat-fu-test--messages-reported (reverse repeat-fu-test--captured-messages))
  repeat-fu-test--messages-reported)

(defmacro with-repeat-fu-test-message-capture (&rest body)
  "Execute BODY with messages captured instead of displayed.
Messages are stored in `repeat-fu-test--captured-messages'.
Use `repeat-fu-test-messages' to get them in chronological order."
  (declare (indent 0))
  `(let ((repeat-fu-test--captured-messages nil)
         (repeat-fu-test--messages-reported nil)
         (inhibit-message t)
         (echo-keystrokes 0)
         (orig-message (symbol-function 'message)))
     (unwind-protect
         (progn
           (fset 'message #'repeat-fu-test--message-capture)
           ,@body)
       (fset 'message orig-message))))


;; ---------------------------------------------------------------------------
;; Internal Functions/Macros

(defmacro simulate-input (&rest keys)
  "Helper macro to simulate input using KEYS, each a key sequence.
KEYS run as a single keyboard macro."
  (declare (indent 0))
  `(let ((keys-list (list ,@keys))
         (minibuffer-message-timeout 0))
     (execute-kbd-macro (apply #'vconcat keys-list))))

(defmacro simulate-input-catching-quit (&rest keys)
  "Simulate input from KEYS, returning non-nil if a quit was raised.

`keyboard-quit' signals out of `execute-kbd-macro', ending the macro, so
the keys a test presses after it need a `simulate-input' of their own.
`should-error' cannot be used for this, a quit is not an error condition
& escapes ERT without the test being reported either way."
  (declare (indent 0))
  `(condition-case nil
       (progn
         (simulate-input
           ,@keys)
         nil)
     (quit
      t)))

(defun buffer-reset-text (initial-buffer-text)
  "Use INITIAL-BUFFER-TEXT to initialize the buffer with text."
  (buffer-disable-undo)
  (erase-buffer)
  ;; Don't move the cursor.
  (save-excursion (insert initial-buffer-text))
  (buffer-enable-undo))

(defmacro with-repeat-fu-test-buffer (initial-buffer-text &rest body)
  "Run BODY in a temporary buffer with INITIAL-BUFFER-TEXT.
BODY is responsible for setting the major-mode & enabling `repeat-fu-mode',
in the order a user would perform them.
Nest within `with-repeat-fu-test' for a test needing a second buffer."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer "untitled"))
         (buf-prev (current-buffer)))
     (switch-to-buffer buf)
     (buffer-reset-text ,initial-buffer-text)
     ;; Protected, a failing `should' signals out of BODY.  Without this the
     ;; buffer stays alive & current, so the next test runs against a
     ;; different buffer than the one it created.
     (unwind-protect
         (progn
           ,@body)
       (kill-buffer buf)
       ;; Return a nested use to the buffer it was entered from.
       (when (buffer-live-p buf-prev)
         (switch-to-buffer buf-prev)))))

(defmacro with-repeat-fu-test (initial-buffer-text &rest body)
  "Run BODY in a temporary buffer with INITIAL-BUFFER-TEXT.
BODY is responsible for setting the major-mode & enabling `repeat-fu-mode',
in the order a user would perform them.

Each test binds the configuration it covers - `repeat-fu-backend' for the
preset & `repeat-fu-global-mode' for whether recording is shared between
buffers.  A test using the shared buffer must record its own change before
repeating, since what an earlier test recorded is still there.

Messages are captured and not displayed.  Use `repeat-fu-test-messages'
to retrieve captured messages for validation.  Any message a test has not
read back fails it instead of passing unnoticed."
  (declare (indent 1))
  `(with-repeat-fu-test-message-capture
     (let ( ;; Isolate `kill-ring' state for clipboard tests.
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           ;; Isolate the macro `repeat-fu-copy-to-last-kbd-macro' writes to.
           (last-kbd-macro nil))
       (prog1 (with-repeat-fu-test-buffer ,initial-buffer-text
                ,@body)
         ;; NOTE: this runs after the buffer is killed & left, so a message caused by that
         ;; cleanup fails the test too.  Whatever a test read back it has already checked,
         ;; what must not have happened is another message arriving since - so this is
         ;; run unconditionally rather than being skipped for a test that checked its own.
         (let ((messages-reported repeat-fu-test--messages-reported))
           (should (equal messages-reported (repeat-fu-test-messages))))))))

(defun repeat_fu_tests-run-all ()
  "Run all tests in batch mode."
  (ert-run-tests-batch-and-exit))


;; ---------------------------------------------------------------------------
;; Tests: Core
;;
;; Recording & replaying itself, whichever preset is in use.

(ert-deftest count-repeats-with-the-prefix-argument ()
  "A numeric prefix on the repeat is the number of times to replay.

Workflow: type text, then repeat with a numeric prefix argument.
Verifies: the macro runs that many times, on top of the original."
  (let ((repeat-fu-backend (repeat-fu-preset-multi))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (simulate-input
        (kbd "a b c")
        (kbd "C-u 3")
        repeat-fu-test-key-execute)
      (should (equal "abcabcabcabc" (buffer-string)))
      (should (equal nil (repeat-fu-test-messages))))))


(ert-deftest execute-with-nothing-recorded-reports-it ()
  "Repeating before anything has been recorded reports it.

Workflow: press the repeat key without making a change first.
Verifies: the buffer is left alone & the reason is reported."
  (let ((repeat-fu-backend (repeat-fu-preset-single))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test "abc"
      (text-mode)
      (repeat-fu-mode 1)
      (simulate-input
        repeat-fu-test-key-execute)
      (should (equal "abc" (buffer-string)))
      (should (equal '("Nothing to repeat.") (repeat-fu-test-messages))))))

(ert-deftest execute-without-the-mode-enabled-errors ()
  "The repeat commands report an error when the mode isn't enabled.

Guards `repeat-fu--mode-enabled-or-error'.  The commands are bound
globally, so they are reachable from a buffer that never enabled the mode
& would otherwise replay whatever was recorded elsewhere.

Workflow: press the repeat key without enabling the mode.
Verifies: the error names the mode & the buffer is left alone."
  (let ((repeat-fu-backend (repeat-fu-preset-single))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test "abc"
      (text-mode)
      (let ((err
             (should-error
              (simulate-input
                repeat-fu-test-key-execute)
              :type 'user-error)))
        ;; The quotes are translated to match `text-quoting-style'.
        (should
         (equal
          (substitute-quotes "repeat-fu: `repeat-fu-mode' must be enabled!")
          (error-message-string err))))
      (should (equal "abc" (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Tests: Mode Setup
;;
;; Enabling the mode itself, rather than what it goes on to record.

(ert-deftest mode-preset-is-loaded-by-name ()
  "The preset named by `repeat-fu-preset' is loaded when enabling the mode.

Every other test sets `repeat-fu-backend' directly, which is the path a
user takes only to write their own repeat logic.  By default the backend
is nil & the named preset supplies it.

Workflow: enable the mode with the preset named rather than set, then
type & repeat.
Verifies: the named preset is what repeats, the single preset repeating
the last change on its own."
  (let ((repeat-fu-backend nil)
        (repeat-fu-preset 'single)
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (simulate-input
        (kbd "a b c")
        repeat-fu-test-key-execute)
      (should (equal "abcc" (buffer-string))))))

;; TODO: cover `repeat-fu-global-mode' being enabled - the default, where a
;; change made in one buffer repeats in another.  Every test turns it off.
;;
;; Writing it needs the buffer switch to happen from a key rather than from
;; the test body: enabling the mode & switching between two `simulate-input'
;; calls leaves a post-command-hook running without its pre, which records an
;; empty key sequence carrying the previous command's change flag.  The single
;; preset then picks that as the change to repeat & the replay does nothing.

(ert-deftest mode-re-enable-keeps-the-buffer-preset ()
  "Enabling the mode again leaves the buffer on the preset it was enabled with.

Guards the order at the start of `repeat-fu-mode'.  The preset callbacks
are buffer-local as well, so refreshing them before the local values were
set up had the shared value copied straight over them - a buffer enabling
the mode a second time silently moved to whichever preset another buffer
enabled last.

Workflow: enable with the single preset, enable a second buffer with the
multi preset, then enable again in the first & repeat there.
Verifies: the repeat follows the preset the buffer was enabled with, the
single preset repeats the last change where the multi preset repeats the
whole run."
  (let ((repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (setq-local repeat-fu-backend (repeat-fu-preset-single))
      (repeat-fu-mode 1)
      (with-repeat-fu-test-buffer ""
        (text-mode)
        (setq-local repeat-fu-backend (repeat-fu-preset-multi))
        (repeat-fu-mode 1))
      ;; Enabling again, as a mode hook running twice would.
      (repeat-fu-mode 1)
      (simulate-input
        (kbd "a b c")
        repeat-fu-test-key-execute)
      (should (equal "abcc" (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Tests: Listener API
;;
;; Listeners tap the keystroke stream for external code, independent of the
;; preset - the multi preset is bound only because the mode needs one.

(ert-deftest listener-records-keys ()
  "Keys pressed while a listener is registered are collected.

Workflow: register a listener, type text, then collect.
Verifies: the keys typed are returned in the order pressed."
  (let ((repeat-fu-backend (repeat-fu-preset-multi))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (let ((token (repeat-fu-listener-register)))
        (simulate-input
          (kbd "a b c"))
        (should (equal "a b c" (key-description (repeat-fu-listener-collect token))))))))

(ert-deftest listener-records-nothing-when-no-keys-pressed ()
  "A listener with no keys to report collects nothing.

Workflow: register a listener & collect without pressing a key.
Verifies: nothing is returned, rather than an empty vector."
  (let ((repeat-fu-backend (repeat-fu-preset-multi))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (let ((token (repeat-fu-listener-register)))
        (should (equal nil (repeat-fu-listener-collect token)))))))

(ert-deftest listener-excludes-registering-command ()
  "The command that registers a listener is not recorded by it.

Recording starts with the next command, so external code registering
from a command does not collect the key that invoked it.

Workflow: press a key whose command registers a listener, type text,
then collect.
Verifies: only the text typed afterwards is returned."
  (let ((repeat-fu-backend (repeat-fu-preset-multi))
        (repeat-fu-global-mode nil)
        (repeat-fu-test-listener-token nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (simulate-input
        repeat-fu-test-key-listener-register
        (kbd "a b c"))
      (should
       (equal
        "a b c" (key-description (repeat-fu-listener-collect repeat-fu-test-listener-token)))))))

(ert-deftest listener-collect-keeps-recording ()
  "Collecting from a listener leaves it recording.

Workflow: type, collect, type again, then collect again.
Verifies: the second collect returns the keys from both, the first
collect neither stops the listener nor consumes what it holds."
  (let ((repeat-fu-backend (repeat-fu-preset-multi))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (let ((token (repeat-fu-listener-register)))
        (simulate-input
          (kbd "a b"))
        (should (equal "a b" (key-description (repeat-fu-listener-collect token))))
        (simulate-input
          (kbd "c"))
        (should (equal "a b c" (key-description (repeat-fu-listener-collect token))))))))

(ert-deftest listener-multiple-concurrent ()
  "Listeners registered at different points each record from their own.

Workflow: register a listener, type, register a second, type again.
Verifies: the first holds both keys & the second only what followed it."
  (let ((repeat-fu-backend (repeat-fu-preset-multi))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (let ((token-first (repeat-fu-listener-register)))
        (simulate-input
          (kbd "a"))
        (let ((token-second (repeat-fu-listener-register)))
          (simulate-input
            (kbd "b"))
          (should (equal "a b" (key-description (repeat-fu-listener-collect token-first))))
          (should (equal "b" (key-description (repeat-fu-listener-collect token-second)))))))))

(ert-deftest listener-unregister-and-collect-stops-recording ()
  "Unregistering returns what was recorded & stops the listener.

Workflow: type, unregister & collect, type again, then collect.
Verifies: the keys typed before are returned, and the token holds
nothing afterwards."
  (let ((repeat-fu-backend (repeat-fu-preset-multi))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (let ((token (repeat-fu-listener-register)))
        (simulate-input
          (kbd "a"))
        (should (equal "a" (key-description (repeat-fu-listener-unregister-and-collect token))))
        (simulate-input
          (kbd "b"))
        (should (equal nil (repeat-fu-listener-collect token)))))))

(ert-deftest listener-nil-token-is-inert ()
  "A nil token is accepted by the listener functions.

Callers hold a token that may never have been registered, so these must
not signal on one.

Workflow: collect & unregister with no token.
Verifies: nothing is returned & nothing is signalled."
  (let ((repeat-fu-backend (repeat-fu-preset-multi))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (should (equal nil (repeat-fu-listener-collect nil)))
      (should (equal nil (repeat-fu-listener-unregister-and-collect nil)))
      ;; Signalling here fails the test.
      (repeat-fu-listener-unregister nil))))

(ert-deftest listener-excludes-repeat-replay ()
  "A repeat feeds nothing to the listener.

The replayed keys were already recorded when first pressed, collecting
them again would report keys the user never pressed.

Workflow: type, then press the repeat key, then collect.
Verifies: neither the repeat key nor the keys it replayed are returned."
  (let ((repeat-fu-backend (repeat-fu-preset-multi))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (let ((token (repeat-fu-listener-register)))
        (simulate-input
          (kbd "a b c")
          repeat-fu-test-key-execute)
        (should (equal "abcabc" (buffer-string)))
        (should (equal "a b c" (key-description (repeat-fu-listener-collect token))))))))


;; ---------------------------------------------------------------------------
;; Tests: Abandoning an Edit
;;
;; Reusing the macro a repeat last ran after `keyboard-quit',
;; see `repeat-fu-last-used-on-quit'.

(ert-deftest quit-reuses-the-last-used-macro ()
  "An edit abandoned with a quit leaves the last used macro in place.

Workflow: repeat a change, make another edit, abandon it with a quit,
then repeat again.
Verifies: the macro used before is repeated, the abandoned edit is not."
  (let ((repeat-fu-backend (repeat-fu-preset-single))
        (repeat-fu-global-mode nil)
        (repeat-fu-last-used-on-quit t))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      ;; Repeating makes this the last used macro.
      (simulate-input
        (kbd "a")
        repeat-fu-test-key-execute)
      (should (equal "aa" (buffer-string)))
      ;; The edit to abandon.
      (simulate-input
        (kbd "z"))
      (should
       (simulate-input-catching-quit
         (kbd "C-g")))
      (simulate-input
        repeat-fu-test-key-execute)
      (should (equal "aaza" (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Tests: Command Declarations
;;
;; How `repeat-fu-declare' marks commands, read back with the
;; `repeat-fu-command-test-*' predicates.
;;
;; NOTE: a declaration cannot be revoked, marking only happens for a non-nil
;; value, so each test declares symbols of its own rather than sharing them.

(ert-deftest declare-marks-a-single-symbol ()
  "A lone symbol is accepted, not only a list of them.

Workflow: declare one symbol as skipped.
Verifies: it reads back as skipped, with the properties not named by the
declaration left alone."
  (repeat-fu-declare 'repeat-fu-test-declare-lone :skip t)
  (should (equal t (repeat-fu-command-test-skip 'repeat-fu-test-declare-lone)))
  (should (equal nil (repeat-fu-command-test-skip-change 'repeat-fu-test-declare-lone)))
  (should (equal nil (repeat-fu-command-test-skip-active 'repeat-fu-test-declare-lone))))

(ert-deftest declare-marks-a-list-of-symbols ()
  "Several symbols & several properties in one declaration.

`:skip-active' is only read by the presets for other packages, so this is
as far as it can be covered here.

Workflow: declare two symbols with two properties at once.
Verifies: both symbols carry both properties."
  (repeat-fu-declare
   '(repeat-fu-test-declare-first repeat-fu-test-declare-second)
   :skip-change t
   :skip-active t)
  (dolist (sym '(repeat-fu-test-declare-first repeat-fu-test-declare-second))
    (should (equal t (repeat-fu-command-test-skip-change sym)))
    (should (equal t (repeat-fu-command-test-skip-active sym)))
    (should (equal nil (repeat-fu-command-test-skip sym)))))

(ert-deftest declare-rejects-unknown-keyword ()
  "An unsupported key is reported rather than ignored.

Workflow: declare with a key that is not supported.
Verifies: the key is named in the error."
  (let ((err
         (should-error (repeat-fu-declare 'repeat-fu-test-declare-unknown :bogus t) :type 'error)))
    (should (equal "Unknown keyword: :bogus" (error-message-string err)))))

(ert-deftest declare-rejects-trailing-argument ()
  "An argument left without a value is reported.

Workflow: declare with a trailing argument that has no key.
Verifies: the leftover argument is named in the error."
  (let ((err
         (should-error
          (repeat-fu-declare 'repeat-fu-test-declare-trailing :skip t 'trailing)
          :type 'error)))
    (should
     (equal "Arguments contain non key/value pairs: (trailing)" (error-message-string err)))))

(ert-deftest declare-skip-excludes-command-from-recording ()
  "A skipped command is not recorded, so it never repeats.

Workflow: type, press a key whose command is declared skipped, repeat.
Verifies: the typing repeats, not the skipped command - which would
insert its own character again, see `single-repeat-last-change-only'."
  (repeat-fu-declare 'repeat-fu-test-insert-x :skip t)
  (let ((repeat-fu-backend (repeat-fu-preset-single))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (simulate-input
        (kbd "a")
        repeat-fu-test-key-insert-x
        repeat-fu-test-key-execute)
      (should (equal "axa" (buffer-string))))))

(ert-deftest declare-skip-change-excludes-command-as-the-change ()
  "A command declared skip-change is not taken as the change to repeat.

Unlike `:skip' the command is still recorded, it is passed over when
looking for the change a repeat should replay.

Workflow: type, press a key whose command is declared skip-change, repeat.
Verifies: the typing repeats, not the declared command."
  (repeat-fu-declare 'repeat-fu-test-insert-y :skip-change t)
  (let ((repeat-fu-backend (repeat-fu-preset-single))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (simulate-input
        (kbd "a")
        repeat-fu-test-key-insert-y
        repeat-fu-test-key-execute)
      (should (equal "aya" (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Tests: Multi Preset

;; NOTE: coverage here is thin - what the multi preset repeats is a run of
;; changes, the rules deciding where a run starts & ends are not covered.

(ert-deftest multi-repeat-insert-text ()
  "Repeat typed text with the multi preset.

Workflow: type text, then press the repeat key.
Verifies: the typed text is inserted a second time."
  (let ((repeat-fu-backend (repeat-fu-preset-multi))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (simulate-input
        (kbd "a b c")
        repeat-fu-test-key-execute)
      (should (equal "abcabc" (buffer-string)))
      (should (equal nil (repeat-fu-test-messages))))))


;; ---------------------------------------------------------------------------
;; Tests: Single Preset

(ert-deftest single-repeat-last-change-only ()
  "Repeat the last change, not the run of typing leading to it.

This is what separates the single preset from the multi preset, which
repeats the whole run.

Workflow: type three characters, then press the repeat key.
Verifies: only the last character is inserted again."
  (let ((repeat-fu-backend (repeat-fu-preset-single))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (simulate-input
        (kbd "a b c")
        repeat-fu-test-key-execute)
      (should (equal "abcc" (buffer-string)))
      (should (equal nil (repeat-fu-test-messages))))))

(ert-deftest single-repeat-excludes-motion ()
  "Motion after the change is not part of the repeat.

Workflow: type a character, move back over it, then press the repeat key.
Verifies: the character is inserted at point, with point left after the
insert - replaying the motion as well would step back over it."
  (let ((repeat-fu-backend (repeat-fu-preset-single))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (simulate-input
        (kbd "x")
        (kbd "C-b")
        repeat-fu-test-key-execute)
      (should (equal "xx" (buffer-string)))
      (should (equal 2 (point)))
      (should (equal nil (repeat-fu-test-messages))))))

(ert-deftest single-repeat-includes-numeric-prefix ()
  "A numeric prefix set before the change repeats along with it.

Workflow: insert with a numeric prefix, then press the repeat key.
Verifies: the prefix is replayed too, inserting the same count again
instead of a single character."
  (let ((repeat-fu-backend (repeat-fu-preset-single))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (simulate-input
        (kbd "C-u 3 a")
        repeat-fu-test-key-execute)
      (should (equal "aaaaaa" (buffer-string)))
      (should (equal nil (repeat-fu-test-messages))))))

(ert-deftest single-repeat-after-undo ()
  "Undo is skipped when looking for the change to repeat.

Point is moved before repeating so the two outcomes differ.  Repeating
from where the character was typed would undo the undo, putting it back
at the end - only replaying the insert follows point.

Workflow: append a character, undo it, move to the line start & repeat.
Verifies: the insert repeats at point, not the undo."
  (let ((repeat-fu-backend (repeat-fu-preset-single))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test "abc"
      (text-mode)
      (repeat-fu-mode 1)
      (simulate-input
        (kbd "C-e x")
        (kbd "C-x u")
        (kbd "C-a")
        repeat-fu-test-key-execute)
      (should (equal "xabc" (buffer-string)))
      ;; Emacs reports its own undo, the repeat reports nothing.
      (should (equal '("Undo") (repeat-fu-test-messages))))))

(ert-deftest single-copy-to-last-kbd-macro ()
  "Copy the macro the repeat would run into `last-kbd-macro'.

Workflow: type three characters, then press the copy key.
Verifies: the macro holds the last change on its own & the buffer is
left alone, the keys are copied rather than run."
  (let ((repeat-fu-backend (repeat-fu-preset-single))
        (repeat-fu-global-mode nil))
    (with-repeat-fu-test ""
      (text-mode)
      (repeat-fu-mode 1)
      (simulate-input
        (kbd "a b c")
        repeat-fu-test-key-copy-to-last-kbd-macro)
      (should (equal "abc" (buffer-string)))
      (should (equal "c" (key-description last-kbd-macro)))
      (should (equal '("Copied.") (repeat-fu-test-messages))))))

(provide 'repeat_fu_tests)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; repeat_fu_tests.el ends here
