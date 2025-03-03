;;; repeat-fu.el --- Minor mode to repeat typing or commands -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 1995 James Gillespie
;; Copyright (C) 2000 Robert Wyrick <rob@wyrick.org>
;; Copyright (C) 2025 Campbell Barton <ideasman42@gmail.com>

;; Author: Campbell Barton <ideasman42@gmail.com>

;; Maintainer: Campbell Barton <ideasman42@gmail.com>
;; Keywords: convenience
;; Version: 0.1
;; URL: https://codeberg.org/ideasman42/emacs-repeat-fu
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; The purpose of this minor mode is to record & replay actions.

;;; Code:

;; ---------------------------------------------------------------------------
;; Custom Variables

(require 'kmacro)

(defgroup repeat-fu nil
  "Enhanced repeat mode."
  :group 'convenience)

(defcustom repeat-fu-preset 'multi
  "The named preset to use (as a symbol).
This loads a bundled preset with the `repeat-fu-preset-' prefix.
If you wish to define your own repeat logic, set:
`repeat-fu-backend' P-list directly.

By convention, the following rules are followed for bundled presets.

- Any selection that uses the mouse cursor causes selection
  actions to be ignored as they can't be repeated reliably.
- Undo/redo commands wont be handled as a new edit to be repeated.
  This means it's possible to undo the `repeat-fu-execute' and repeat the
  action at a different location instead of repeating the undo."
  :type 'symbol)

(defcustom repeat-fu-global-mode t
  "When true, `repeat-fu' shares its command buffer between buffers."
  :type 'boolean)

(defcustom repeat-fu-buffer-size 512
  "Maximum number of steps to store.
When nil, all commands are stored,
the `repeat-fu-backend' is responsible for ensuring buffer doesn't expand indefinitely."
  :type 'integer)


;; ---------------------------------------------------------------------------
;; Public Variables

(defvar repeat-fu-backend nil
  "A P-list of callbacks used to implement repeat logic:

:macros-extract
  Extract a macro from the history.
  This function takes an argument, a list of commands with the most
  recent command ordered first.

  Return a list of macros to be passed to `:macros-select'.

:macros-select
  Select the macro to use.

  The purpose of this function is to select the macro to use
  based on the context.  One macro may be returned when there
  is an active region, another may be used when there isn't.

  Takes the macros returned by `:macros-extract'
  and returns the macro which should be used.

:pre-data (optional)
  Store data before the command .

:post-data
  Store data after the command.
  Takes a single argument - the output of the `:pre-data' callback.


The value must be set before the mode has been initialized.
If the value is changed, `repeat-fu-mode' will have to be restarted.")

;; Set while executing the command.
(defvar repeat-fu-this-command-bitmask 0
  "This flag is accumulated while executing the command.
The :post-data callback in `repeat-fu-backend' may use it.")

(defconst repeat-fu-flag-is-prefix 1
  "Set when the command is a prefix command.")
(defconst repeat-fu-flag-is-change 2
  "Set when the command makes a change.")
(defconst repeat-fu-flag-free 4
  "This flag and higher bits are free and can be used by presets.")


;; ---------------------------------------------------------------------------
;; Internal Variables


;; Callbacks
;; =========
;;
;; Set from `repeat-fu-backend' or `repeat-fu-preset'.

(defvar repeat-fu--macros-extract-fn nil)
(defvar repeat-fu--macros-select-fn 'car)
(defvar repeat-fu--pre-fn 'ignore)
(defvar repeat-fu--post-fn 'ignore)


;; Tracking State
;; ==============

;; Needed for commands that override this-command.
;; This data stored by `repeat-fu--pre-fn' on entering each command.
(defvar repeat-fu--pre-data nil)

;; Did last command change buffer?
(defvar repeat-fu--is-change nil)

;; Marker variable to show the prefix argument has been changed.
(defvar repeat-fu--is-prefix nil)

;; Use an accumulating buffer OR a ring buffer.

;; Needed so `repeat-fu-execute' can skip itself.
;; Executing the macro overwrites `this-command.'.
;; When set, the command is not stored.
(defvar repeat-fu--cmd-skip nil)


;; Stores Macro Data
;; =================

;; Accumulating buffer.
;; Saved commands (accumulation buffer).
(defvar repeat-fu--cmd-accum nil)

;; Ring buffer.
;; Saved commands (ring buffer).
(defvar repeat-fu--cmd-ring nil)

;; The last extracted macros.
(defvar repeat-fu--macros-last nil)


;; ---------------------------------------------------------------------------
;; Internal Ring Buffer Implementation
;; `((index . length) . vector-buffer)'

(defun repeat-fu--ringbuf-make (size)
  "Return a new ring-buffer of SIZE."
  (declare (important-return-value t))
  (cons (cons 0 0) (make-vector size nil)))

(defun repeat-fu--ringbuf-clear (rbuf)
  "Empty the ring-buffer RBUF."
  (declare (important-return-value nil))
  ;; Set the length to zero (OK to leave index where it is).
  (setcdr (car rbuf) 0)
  rbuf)

(defun repeat-fu--ringbuf-push (rbuf elem)
  "Push ELEM onto the ring-buffer RBUF."
  (declare (important-return-value nil))
  (pcase-let ((`(,pos-len . ,buf) rbuf))
    (let ((pos (car pos-len))
          (len (cdr pos-len))
          (ring-capacity (length buf)))


      (unless (and (not (zerop len))
                   (let ((num-and-elem (aref buf pos)))
                     (cond
                      ((equal elem (cdr num-and-elem))
                       ;; Bump the previous element count.
                       (setcar num-and-elem (1+ (car num-and-elem)))
                       t)
                      (t
                       nil))))

        ;; Add new element.
        (unless (eq len ring-capacity)
          (setcdr pos-len (1+ len)))

        (aset buf (setcar pos-len (mod (1+ pos) ring-capacity)) (cons 1 elem))))))

(defun repeat-fu--ringbuf-as-list (rbuf)
  "Return ring-buffer RBUF as a list."
  (declare (important-return-value t))
  (let ((result nil))
    (pcase-let ((`(,pos-len . ,buf) rbuf))
      (let ((pos (car pos-len))
            (len (cdr pos-len))
            (ring-capacity (length buf)))

        (let ((i (mod (- pos len) ring-capacity)))
          (dotimes (_ len)
            (setq i (mod (1+ i) ring-capacity))
            (pcase-let ((`(,n . ,elem) (aref buf i)))
              (dotimes (_ n)
                (push elem result)))))))
    result))


;; ---------------------------------------------------------------------------
;; Internal Buffer Functions

(defun repeat-fu--cmd-buffer-get ()
  "Return a list from the underlying buffer type."
  (declare (important-return-value t))
  (cond
   (repeat-fu-buffer-size
    (repeat-fu--ringbuf-as-list repeat-fu--cmd-ring))
   (t
    repeat-fu--cmd-accum)))

(defun repeat-fu--cmd-buffer-push (elem)
  "Push ELEM to the underlying buffer type."
  (declare (important-return-value nil))
  ;; (printf "* [%s] %S\n" (format-kbd-macro (car elem)) (cdr elem))
  (cond
   ((null (car elem))
    (when this-command
      (error "Internal error `keys' was null with %S" this-command)))

   (repeat-fu-buffer-size
    (repeat-fu--ringbuf-push repeat-fu--cmd-ring elem))
   (t
    (push elem repeat-fu--cmd-accum))))


;; ---------------------------------------------------------------------------
;; Internal Callback Wrappers

(defsubst repeat-fu--extract-fn-wrapper (cmd-buffer)
  "Extract the macro from CMD-BUFFER."
  (declare (important-return-value t))
  ;; For debugging.
  ;; (seq-do-indexed (lambda (e i) (printf "%d [%S]\n" i e)) cmd-buffer)
  (condition-case err
      (funcall repeat-fu--macros-extract-fn cmd-buffer)
    (error
     (progn
       (message "Repeat-FU: error in `repeat-fu--macros-extract-fn' (%s)"
                (error-message-string err))
       nil))))

(defsubst repeat-fu--select-fn-wrapper (kbuf-list)
  "Select the macro from KBUF-LIST."
  (declare (important-return-value t))
  (condition-case err
      (funcall repeat-fu--macros-select-fn kbuf-list)
    (error
     (progn
       (message "Repeat-FU: error in `repeat-fu--post-fn' (%s)" (error-message-string err))
       nil))))

(defsubst repeat-fu--pre-fn-wrapper ()
  "Pre command wrapper."
  (declare (important-return-value t))
  (funcall repeat-fu--pre-fn))

(defsubst repeat-fu--post-fn-wrapper (data-pre)
  "Post command wrapper taking DATA-PRE from before the function runs."
  (declare (important-return-value t))
  (funcall repeat-fu--post-fn data-pre))


;; ---------------------------------------------------------------------------
;; Internal Functions

(defmacro repeat-fu--without-hooks (&rest body)
  "Run BODY without hooks."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (repeat-fu--hooks-remove)
         ,@body)
     ;; Protected.
     (repeat-fu--hooks-add)))

(defun repeat-fu--extract-repeat-macro-or-last ()
  "Extract a macro from previous commands or return the last extracted macro."
  (declare (important-return-value t))
  (let ((kbuf-list (repeat-fu--extract-fn-wrapper (repeat-fu--cmd-buffer-get))))
    (when kbuf-list
      (repeat-fu--clear)
      (setq repeat-fu--macros-last kbuf-list)))
  (repeat-fu--select-fn-wrapper repeat-fu--macros-last))

;; NOTE: This is a little tricky ... when the prefix-argument is changed it doesn't leave
;; much of a trace. It resets `this-command' and `real-this-command' to the previous ones.
;; Hence the best way (that I know of) to tell whether the last command was changing the
;; prefix is by adding a hook into `prefix-command-preserve-state-hook'.
(defun repeat-fu--prefix-command-hook ()
  "Run on prefix commands."
  (declare (important-return-value nil))
  (setq repeat-fu-this-command-bitmask
        (logior repeat-fu-this-command-bitmask repeat-fu-flag-is-prefix)))


;; The `after-change-functions' hook.
(defun repeat-fu--after-change (_start _end _prevlen)
  "Run on change."
  (declare (important-return-value nil))

  (setq repeat-fu-this-command-bitmask
        (logior repeat-fu-this-command-bitmask repeat-fu-flag-is-change)))

;; The `pre-command-hook'.
(defun repeat-fu--pre-hook ()
  "Run before commands."
  (declare (important-return-value nil))

  (setq repeat-fu-this-command-bitmask 0)
  (setq repeat-fu--cmd-skip nil)
  (setq repeat-fu--pre-data (repeat-fu--pre-fn-wrapper)))


(defun repeat-fu--post-hook ()
  "The heart of repeat-fu mode."
  (declare (important-return-value nil))

  (unless repeat-fu--cmd-skip
    (let ((info (repeat-fu--post-fn-wrapper repeat-fu--pre-data)))
      (unless (eq info t)
        (let ((keys (this-command-keys-vector)))
          (repeat-fu--cmd-buffer-push (cons keys info)))))))

(defun repeat-fu--hooks-remove ()
  "Remove hooks."
  (declare (important-return-value nil))

  (remove-hook 'pre-command-hook #'repeat-fu--pre-hook t)
  (remove-hook 'post-command-hook #'repeat-fu--post-hook t)
  (remove-hook 'after-change-functions #'repeat-fu--after-change t)
  (remove-hook 'prefix-command-preserve-state-hook #'repeat-fu--prefix-command-hook t))

(defun repeat-fu--hooks-add ()
  "Setup hooks."
  (declare (important-return-value nil))

  (add-hook 'pre-command-hook #'repeat-fu--pre-hook nil t)
  (add-hook 'post-command-hook #'repeat-fu--post-hook nil t)
  (add-hook 'after-change-functions #'repeat-fu--after-change nil t)
  (add-hook 'prefix-command-preserve-state-hook #'repeat-fu--prefix-command-hook nil t))

(defun repeat-fu--clear ()
  "A clear function, intended to be used by pre/post callbacks."
  (declare (important-return-value nil))

  (setq repeat-fu--cmd-accum nil)
  (when repeat-fu--cmd-ring
    (repeat-fu--ringbuf-clear repeat-fu--cmd-ring)))

(defun repeat-fu--preset-refresh ()
  "Refresh values after changing the preset."
  (declare (important-return-value nil))

  (let ((preset repeat-fu-backend)
        (macros-extract-fn nil)
        (macros-select-fn nil)
        (pre-fn nil)
        (post-fn nil)

        (key nil)

        (success t))

    (while (keywordp (setq key (car preset)))
      (setq preset (cdr preset))
      (pcase key
        (:macros-extract (setq macros-extract-fn (pop preset)))
        (:macros-select (setq macros-select-fn (pop preset)))
        (:pre-data (setq pre-fn (pop preset)))
        (:post-data (setq post-fn (pop preset)))
        (_
         (progn
           (setq preset (cdr-safe preset))
           (message "unknown keyword: %S" key)))))

    (when preset
      (message "presets contain non key/value pairs: %S " preset)
      (setq success nil))

    (unless macros-extract-fn
      (message "Error, the key :macros-extract was missing!")
      (setq success nil))
    (unless macros-select-fn
      (message "Error, the key :macros-select was missing!")
      (setq success nil))
    (unless pre-fn
      (message "Error, the key :post-data was missing")
      (setq success nil))

    (setq repeat-fu--pre-fn pre-fn)
    (setq repeat-fu--post-fn post-fn)
    (setq repeat-fu--macros-extract-fn macros-extract-fn)
    (setq repeat-fu--macros-select-fn macros-select-fn)

    success))


;; ---------------------------------------------------------------------------
;; Internal Command Marking

(defun repeat-fu--mark-commands-setup ()
  "Mark commands for skipping or skipping changes from."
  (declare (important-return-value nil))

  ;; From `files.el'.
  (repeat-fu--commands-mark-skip (list 'save-buffer) t)
  ;; From `mwheel.el'.
  (repeat-fu--commands-mark-skip
   (list 'mwheel-scroll 'mouse-wheel-text-scale 'mouse-wheel-global-text-scale) t)

  ;; From `simple.el'.
  (repeat-fu--commands-mark-skip-change (list 'undo 'undo-redo 'undo-only) t)
  ;; From `undo-fu.el' (package).
  (repeat-fu--commands-mark-skip-change (list 'undo-fu-only-undo 'undo-fu-only-redo) t)
  ;; From `mouse.el'.
  (repeat-fu--commands-mark-skip-active (list 'mouse-set-region 'mouse-set-point) t))


;; ---------------------------------------------------------------------------
;; Private Marking Implementation

(defun repeat-fu--commands-mark (commands value id)
  "Mark symbols from COMMANDS with ID set to VALUE."
  (declare (important-return-value nil))

  (dolist (cmd commands)
    (put cmd id value)))

(defun repeat-fu--commands-mark-skip (commands value)
  "Ignore COMMANDS entirely.
Commands that aren't related to editing should be marked especially save.
The property is set to VALUE which should typically be true."
  (declare (important-return-value nil))
  (repeat-fu--commands-mark commands value 'redo-fu-skip))

(defun repeat-fu--commands-mark-skip-active (commands value)
  "Ignore COMMANDS from region activation.
This means, any commands that create the region using these commands
won't repeat any active-region manipulation.
This should typically be used any region manipulation that uses the mouse.

Commands such as save & undo should be included.
The property is set to VALUE which should typically be true."
  (declare (important-return-value nil))
  (repeat-fu--commands-mark commands value 'redo-fu-skip-active))


(defun repeat-fu--commands-mark-skip-change (commands value)
  "Ignore COMMANDS from changes so they are not repeated.
Commands such as save & undo should be included.
The property is set to VALUE which should typically be true."
  (declare (important-return-value nil))
  (repeat-fu--commands-mark commands value 'redo-fu-skip-change))

;; ---------------------------------------------------------------------------
;; Public Functions


(defun repeat-fu-command-test-skip (cmd)
  "Return the `redo-fu-skip' property of CMD."
  (declare (important-return-value t))
  (cond
   ((symbolp cmd)
    (get cmd 'redo-fu-skip))
   (t
    nil)))

(defun repeat-fu-command-test-skip-active (cmd)
  "Return the `redo-fu-skip-active' property of CMD."
  (declare (important-return-value t))
  (cond
   ((symbolp cmd)
    (get cmd 'redo-fu-skip-active))
   (t
    nil)))

(defun repeat-fu-command-test-skip-change (cmd)
  "Return the `redo-fu-skip-change' property of CMD."
  (declare (important-return-value t))
  (cond
   ((symbolp cmd)
    (get cmd 'redo-fu-skip-change))
   (t
    nil)))


;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun repeat-fu-declare (symbols &rest plist)
  "Support for controlling how `repeat-fu' handles commands.

SYMBOLS may be a symbol or list of symbols, matching command names.

The PLIST must only contain the following keys.

:skip
   When non-nil, the command is ignored by `repeat-fu' entirely.

   By default, `save-buffer' uses this so repeating an action never saves.
:skip-active
   When non-nil, the command won't include the active-region
   when one of these functions was used to create it.

   By default, ``mouse-set-region`` uses this so repeating an action
   doesn't attempt to replay the mouse-drag used for selection.
:skip-change
   When non-nil, commands that change the buffer will be skipped
   when detecting commands to be repeated.

   This is used for `undo' (and related undo commands),
   so it's possible to undo `repeat-fu-execute' and repeat the action elsewhere
   without the undo action being repeated.

   This is different from :skip since undo actions *can* be repeated
   when part of multiple edits in `insert' mode - for presets that support this.

The values should be t, other values such as function calls
to make these checks conditional may be supported in the future."
  (let ((key nil)
        (value-skip nil)
        (value-skip-change nil)
        (value-skip-active nil))

    (while (keywordp (setq key (car plist)))
      (setq plist (cdr plist))
      (pcase key
        (:skip (setq value-skip (pop plist)))
        (:skip-change (setq value-skip-change (pop plist)))
        (:skip-active (setq value-skip-active (pop plist)))
        (_ (error "Unknown keyword: %S" key))))

    (when plist
      (error "Arguments contain non key/value pairs: %S" plist))

    (when symbols
      (unless (listp symbols)
        (setq symbols (list symbols)))

      (when value-skip
        (repeat-fu--commands-mark-skip symbols value-skip))
      (when value-skip-change
        (repeat-fu--commands-mark-skip-change symbols value-skip-change))
      (when value-skip-active
        (repeat-fu--commands-mark-skip-active symbols value-skip-active)))))

;;;###autoload
(defun repeat-fu-execute (arg)
  "Execute stored commands.
The prefix argument ARG serves as a repeat count."
  (declare (important-return-value nil))
  (interactive "p")

  (unless arg
    (setq arg 1))

  ;; Execution must not trigger infinite recursion,
  ;; nor record any new steps.
  (setq repeat-fu--cmd-skip t)

  (let ((kbuf (repeat-fu--extract-repeat-macro-or-last)))
    (cond
     ((null kbuf)
      (message "Nothing to repeat"))
     (t
      ;; Execute the macro.
      (repeat-fu--without-hooks
        (condition-case nil
            (with-undo-amalgamate
              (execute-kbd-macro kbuf arg))
          ((error quit exit)
           (repeat-fu--clear)
           (setq repeat-fu--macros-last nil)
           (message "Repeat reset (failed)"))))))))

;;;###autoload
(defun repeat-fu-copy-to-last-kbd-macro ()
  "Copy the current `repeat-fu' command buffer to the `last-kbd-macro' variable.
Then it can be called with `call-last-kbd-macro', named with
`name-last-kbd-macro', or even saved for later use with
`name-last-kbd-macro'"
  (declare (important-return-value nil))
  (interactive)

  (let ((kbuf (repeat-fu--extract-repeat-macro-or-last)))
    (cond
     ((null kbuf)
      (message "Nothing to copy."))
     (t
      (setq last-kbd-macro kbuf)
      (message "Copied.")))))

;; ---------------------------------------------------------------------------
;; Minor Mode

;;;###autoload
(define-minor-mode repeat-fu-mode
  "Record commands to be repeated later."
  :global nil

  (let ((local-vars
         (list
          'repeat-fu--cmd-accum
          'repeat-fu--cmd-ring
          'repeat-fu--cmd-skip
          'repeat-fu--is-change
          'repeat-fu--is-prefix
          'repeat-fu--macros-extract-fn
          'repeat-fu--macros-last
          'repeat-fu--macros-select-fn
          'repeat-fu--post-fn
          'repeat-fu--pre-data)))

    (cond
     ((not repeat-fu-mode)
      (repeat-fu--hooks-remove)

      ;; Always kill local, in case this was ever local.
      (dolist (var local-vars)
        (kill-local-variable var)
        (set var (default-value var))))
     (t
      (unless repeat-fu-backend
        (let ((preset-value (symbol-name repeat-fu-preset)))
          (let ((preset-sym (intern (concat "repeat-fu-preset-" preset-value))))
            (when (condition-case err
                      (progn
                        (require preset-sym)
                        t)
                    (error (message "repeat-fu: preset '%s' not found! (%s)"
                                    preset-value
                                    (error-message-string err))
                           nil))
              (setq repeat-fu-backend (funcall preset-sym))))))

      (repeat-fu--preset-refresh)

      (repeat-fu--hooks-add)

      (repeat-fu--mark-commands-setup)

      (cond
       (repeat-fu-global-mode
        (dolist (var local-vars)
          (kill-local-variable var)
          (set var (default-value var))))
       (t
        (dolist (var local-vars)
          (make-local-variable var)
          (set var (default-value var)))))

      (when repeat-fu-buffer-size
        ;; Only re-create the vector if necessary.
        (when (or (null repeat-fu-global-mode) (null repeat-fu--cmd-ring))
          (setq repeat-fu--cmd-ring (repeat-fu--ringbuf-make repeat-fu-buffer-size))))))))

(provide 'repeat-fu)

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; repeat-fu.el ends here
