;;; repeat-fu-preset-meep.el --- Repeat for Meep modal editing -*- lexical-binding: t -*-
;; URL: https://codeberg.org/ideasman42/emacs-repeat-fu
;; Version: 0.1

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025 Campbell Barton <ideasman42@gmail.com>

;; Author: Campbell Barton <ideasman42@gmail.com>

;;; Commentary:
;; Preset for Meep modal editing.
;;
;; This has matching functionality to the Meow preset.

;;; Code:

(require 'repeat-fu)

(declare-function meep-state "meep" ())

(declare-function meep-command-is-digit-argument "meep" (cmd))
(declare-function meep-command-is-mark-activate "meep" (cmd))
(declare-function meep-command-is-mark-on-motion-exclude "meep" (cmd))
(declare-function meep-command-is-mark-set-on-motion "meep" (cmd))
(declare-function meep-command-is-mark-set-on-motion-adjust "meep" (cmd))
(declare-function meep-command-is-mark-set-on-motion-no-repeat "meep" (cmd))
(declare-function meep-state-insert "meep" ())

(defconst repeat-fu-preset-meep--flag-is-active repeat-fu-flag-free)

(defun repeat-fu-preset-meep--pre-data ()
  "Pre command callback."
  (meep-state))

(defun repeat-fu-preset-meep--post-data (data-pre)
  "Return macro data per step, used for replay.
DATA-PRE stores the state when the command began."
  (cond
   ((repeat-fu-command-test-skip this-command)
    t)
   (t
    (let* ((cmd-flag repeat-fu-this-command-bitmask)
           (state-post (meep-state))
           ;; Only store a pair if the state changed (else just store the state).
           (state-maybe-pair
            (cond
             ((eq data-pre state-post)
              data-pre)
             (t
              (cons data-pre state-post)))))
      ;; Check `deactivate-mark' because the end user result is that the region isn't visible.
      (when (and (region-active-p) (not deactivate-mark))
        (setq cmd-flag (logior cmd-flag repeat-fu-preset-meep--flag-is-active)))

      `[,cmd-flag ,this-command ,state-maybe-pair]))))

(defun repeat-fu-preset-meep--macros-extract (cmd-buffer)
  "Extract the macro from CMD-BUFFER."
  (let* ((vec (vconcat cmd-buffer))
         (vec-len (length vec))
         (vec-end (1- vec-len))

         (elem-scan-while-max-fn
          (lambda (i fn)
            (while (and (< i vec-end) (funcall fn (1+ i)))
              (setq i (1+ i)))
            i))
         (elem-scan-while-min-fn
          (lambda (i fn)
            (while (and (> i 0) (funcall fn (1- i)))
              (setq i (1- i)))
            i))

         ;; Local lookups.
         (elem-is-prefix-fn
          (lambda (i) (not (zerop (logand (aref (cdr (aref vec i)) 0) repeat-fu-flag-is-prefix)))))
         (elem-is-change-fn
          (lambda (i) (not (zerop (logand (aref (cdr (aref vec i)) 0) repeat-fu-flag-is-change)))))
         (elem-is-active-fn
          (lambda (i)
            (not
             (zerop (logand (aref (cdr (aref vec i)) 0) repeat-fu-preset-meep--flag-is-active)))))

         (elem-cmd-fn (lambda (i) (aref (cdr (aref vec i)) 1)))

         (elem-state-pre-fn
          (lambda (i)
            (let ((state-maybe-pair (aref (cdr (aref vec i)) 2)))
              (cond
               ((consp state-maybe-pair)
                (car state-maybe-pair))
               (t
                state-maybe-pair)))))
         (elem-state-post-fn
          (lambda (i)
            (let ((state-maybe-pair (aref (cdr (aref vec i)) 2)))
              (cond
               ((consp state-maybe-pair)
                (cdr state-maybe-pair))
               (t
                state-maybe-pair))))))

    ;; Find the first change.
    (let ((index-change -1))
      (let ((i 0))
        (while (< i vec-len)
          (when (funcall elem-is-change-fn i)
            ;; The first change has been found, skip this if it's an undo.
            ;; Without this, undo then repeat would not be possible as the repeat
            ;; would detect the undo as the action to repeat.
            ;;
            ;; Note that undo *may* be included when building a larger list of actions
            ;; since it may be needed when an action includes text input which is undone
            ;; however, it's important not to detect this as the first change.
            (unless (repeat-fu-command-test-skip-change (funcall elem-cmd-fn i))
              (setq index-change i)
              (setq i vec-len)))
          (setq i (1+ i))))

      (cond
       ((eq -1 index-change)
        ;; A signal nothing could be found.
        nil)
       (t
        ;; Values inclusive.
        (let ((index-min index-change)
              (index-max index-change)
              (index-max-no-active nil)

              (index-min-insert nil)
              (index-max-insert nil)
              (state-insert (meep-state-insert))

              (do-scan-for-mark-commands t))

          ;; Expand while in insert mode.
          (when (eq state-insert (funcall elem-state-post-fn index-change))
            (setq index-min
                  (funcall elem-scan-while-min-fn
                           index-min
                           (lambda (i) (eq state-insert (funcall elem-state-post-fn i)))))
            (setq index-max
                  (funcall elem-scan-while-max-fn
                           index-max
                           (lambda (i) (eq state-insert (funcall elem-state-post-fn i)))))

            ;; Set range for the insert-mode-only macro.
            (setq index-min-insert index-min)
            (setq index-max-insert index-max)
            ;; If the first command "entered" insert mode, skip it.
            ;; Since the purpose is to replay in insert mode without entering/exiting.
            (unless (eq state-insert (funcall elem-state-pre-fn index-max-insert))
              (setq index-max-insert (1- index-max-insert)))

            (unless (funcall elem-is-change-fn index-max)
              ;; If entering insert mode did not change anything (not the "change" command).
              ;; Then don't include any previous motion.
              (setq do-scan-for-mark-commands nil)))

          ;; Some commands don't support building a region,
          ;; detect this and prevent additional commands being included when they shouldn't.
          (when do-scan-for-mark-commands
            ;; Not selected.
            (unless (funcall elem-is-active-fn (1+ index-max))
              (let ((cmd (funcall elem-cmd-fn index-max)))
                ;; The command doesn't respect non-active region.
                (when (meep-command-is-mark-on-motion-exclude cmd)
                  (setq do-scan-for-mark-commands nil)))))

          ;; If the state was exited, keep this as part of the repeated command.
          (when (and (> index-min 0)
                     (eq 'bray-state-stack-pop (funcall elem-cmd-fn (1- index-min))))
            (setq index-min (1- index-min)))

          ;; The last command before selection.
          (setq index-max-no-active index-max)

          ;; If the action operated on the selection.
          ;; Scan for the operations that built the selection.
          (when do-scan-for-mark-commands
            ;; Editing command began with a selection.
            (when (and (< index-max vec-end) (funcall elem-is-active-fn (1+ index-max)))
              (let ((index-max-orig index-max)
                    (ok t))
                (while (and (< index-max vec-end) (funcall elem-is-active-fn (1+ index-max)))
                  (setq index-max (1+ index-max))

                  ;; Any change within the selection breaks the chain.
                  (when (or (funcall elem-is-change-fn index-max)
                            (let ((cmd (funcall elem-cmd-fn index-max)))
                              (or
                               ;; Mouse event created the selection,
                               ;; ignore the selection creation entirely.
                               (repeat-fu-command-test-skip-active cmd)
                               ;; Invoked by a jump action which should not be repeated.
                               (meep-command-is-mark-set-on-motion-no-repeat cmd))))
                    ;; Break.
                    (setq index-max vec-end)
                    (setq ok nil)))

                ;; Unlike "Meow", "Meep" only allows certain commands to activate the selection.
                ;; This could be relaxed as it's quite a constraint,
                ;; nevertheless, in Meep, selection is a deliberate operation,
                ;; not typically started by other actions, so keep this unless
                ;; there is a good reason to change it.

                ;; Reverse is a special case, as this will have selected "and"
                ;; depended on the previous motion.
                ;; In this case there is no need to make any further changes.
                ;; We can continue to scan for mark commands.
                (let ((cmd (funcall elem-cmd-fn index-max)))
                  (unless (meep-command-is-mark-activate cmd)
                    ;; This is an opinionated decision.
                    ;; Check the command that invoked this because selection
                    ;; could have been invoked by the mouse cursor or by
                    ;; some other action - such as dragging with the mouse cursor
                    ;; which the user would not expect to be part of a "chain" of commands.
                    (unless (memq
                             cmd
                             ;; Limit the number of commands,
                             ;; some commands such as jump to the next search item
                             ;; may select but are not primarily selection commands
                             ;; so it's confusing to include them here.
                             (list 'meep-region-toggle 'meep-region-syntax-expand))
                      (setq ok nil)))

                  (cond
                   (ok
                    ;; Don't scan for mark-commands as the selection commands serve this purpose.
                    (setq do-scan-for-mark-commands nil))
                   (t
                    (setq index-max index-max-orig)))))))

          ;; If the first insert command changes, scan for commands that mark the region.
          (when do-scan-for-mark-commands
            (let ((index-max-orig index-max)
                  (ok nil))
              ;; This is reasonably involved,
              ;; check if the motion should be included before the edit.
              (while (and (< index-max vec-end)
                          ;; Check this command can be adjusted.
                          (meep-command-is-mark-set-on-motion-adjust
                           (funcall elem-cmd-fn (1+ index-max))))
                (setq index-max (1+ index-max)))

              ;; Numeric commands are a special case where the "pre" command is needed
              ;; since the post command gets set by running the macro.
              (while (and (< index-max vec-end)
                          (meep-command-is-digit-argument (funcall elem-cmd-fn (1+ index-max))))
                (setq index-max (1+ index-max)))

              (when (and (< index-max vec-end)
                         (meep-command-is-mark-set-on-motion (funcall elem-cmd-fn (1+ index-max))))
                (setq index-max (1+ index-max))
                (setq ok t))

              (unless ok
                (setq index-max index-max-orig))))

          ;; Finally add all numeric-prefix commands (if any).
          (setq index-max
                (funcall elem-scan-while-max-fn
                         index-max
                         (lambda (i) (funcall elem-is-prefix-fn i))))
          (setq index-max-no-active
                (funcall elem-scan-while-max-fn
                         index-max-no-active
                         (lambda (i) (funcall elem-is-prefix-fn i))))

          (let ((result [])
                (result-no-active [])
                (result-only-insert []))

            ;; All command macro.
            (let ((i index-max))
              (while (>= i index-min)
                (setq result (vconcat result (car (aref vec i))))
                (setq i (1- i))))

            ;; Change only macro (no preceding selection).
            (cond
             ((eq index-max index-max-no-active)
              (setq result-no-active result))
             (t
              (let ((i index-max-no-active))
                (while (>= i index-min)
                  (setq result-no-active (vconcat result-no-active (car (aref vec i))))
                  (setq i (1- i))))))

            ;; Insert mode only macro.
            (when (and index-min-insert index-max-insert)
              (let ((i index-max-insert))
                (while (>= i index-min-insert)
                  (setq result-only-insert (vconcat result-only-insert (car (aref vec i))))
                  (setq i (1- i)))))

            (list result result-no-active result-only-insert))))))))

(defun repeat-fu-preset-meep--macros-select (kbuf-list)
  "Select the macro from KBUF-LIST."
  (nth
   (cond
    ((eq (meep-state) (meep-state-insert))
     ;; Repeat in insert mode.
     2)
    ((region-active-p)
     ;; If there is already an active region, don't replay the commands
     ;; that created the active region.
     1)
    (t
     ;; Replay all commands (including active region creation).
     0))
   kbuf-list))

;;;###autoload
(defun repeat-fu-preset-meep ()
  "Return the meep preset."
  (list
   :macros-extract #'repeat-fu-preset-meep--macros-extract
   :macros-select #'repeat-fu-preset-meep--macros-select
   :pre-data #'repeat-fu-preset-meep--pre-data
   :post-data #'repeat-fu-preset-meep--post-data))

(provide 'repeat-fu-preset-meep)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "repeat-fu.el"
;; End:
;;; repeat-fu-preset-meep.el ends here
