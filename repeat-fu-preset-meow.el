;;; repeat-fu-preset-meow.el --- Repeat for Meow modal editing -*- lexical-binding: t -*-
;; URL: https://codeberg.org/ideasman42/emacs-repeat-fu
;; Version: 0.1

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025 Campbell Barton <ideasman42@gmail.com>

;; Author: Campbell Barton <ideasman42@gmail.com>

;;; Commentary:
;; Preset for `Meow modal editing <https://github.com/meow-edit/meow>`__.
;;
;; A preset written for meow which repeats the last edit
;; along with selection commands preceding the edit.
;;
;; Changes made in insert mode are considered a single edit.
;; When entering insert mode changes the buffer (typically `meow-change')
;; the events that constructed the selection are included.
;;
;; This means the following is a single, repeatable action:
;;
;; - Mark 3 words (`meow-next-word', `meow-expand-3').
;; - Change them (`meow-change', "replacement text").
;; - Leave insert mode (`meow-insert-exit').
;;
;; The cursor can be moved elsewhere and `repeat-fu-execute'
;; will replace 3 words at the new location.

;;; Code:

(require 'repeat-fu)

(declare-function meow--current-state "meow-util" ())

(defconst repeat-fu-preset-meow--flag-is-active repeat-fu-flag-free)

(defvar repeat-fu-preset-meow--commands-mark-skip (list 'mouse-set-region 'mouse-set-point)
  "These commands may record the beginning of a selection.
They should not be recorded to initialize a selection.")

;; Limit the number of commands, some commands such as jump to the next search item
;; may select but are not primarily selection commands so it's confusing to include them here.
(defvar repeat-fu-preset-meow--commands-mark-skip-jump (list 'meow-visit)
  "These commands cause a selection on jumping.")

(defun repeat-fu-preset-meow--pre-data ()
  "Pre command callback."
  (meow--current-state))

(defun repeat-fu-preset-meow--post-data (data-pre)
  "Return macro data per step, used for replay.
DATA-PRE stores the state when the command began."
  (cond
   ((repeat-fu-command-test-skip this-command)
    t)
   (t
    (let* ((cmd-flag repeat-fu-this-command-bitmask)
           (state-post (meow--current-state))
           ;; Only store a pair if the state changed (else just store the state).
           (state-maybe-pair
            (cond
             ((eq data-pre state-post)
              data-pre)
             (t
              (cons data-pre state-post)))))
      ;; Check `deactivate-mark' because the end user result is that the region isn't visible.
      (when (and (region-active-p) (null deactivate-mark))
        (setq cmd-flag (logior cmd-flag repeat-fu-preset-meow--flag-is-active)))

      `[,cmd-flag ,this-command ,state-maybe-pair]))))

(defun repeat-fu-preset-meow--macros-extract (cmd-buffer)
  "Extract the macro from CMD-BUFFER."
  (let* ((vec (vconcat cmd-buffer))
         (vec-len (length vec))
         (vec-end (1- vec-len))

         (elem-scan-while-max-fn
          (lambda (i fn)
            (while (and (< i vec-end) (funcall fn (1+ i)))
              (incf i))
            i))
         (elem-scan-while-min-fn
          (lambda (i fn)
            (while (and (> i 0) (funcall fn (1- i)))
              (decf i))
            i))

         ;; Local lookups.
         (elem-is-prefix-fn
          (lambda (i)
            (null (zerop (logand (aref (cdr (aref vec i)) 0) repeat-fu-flag-is-prefix)))))
         (elem-is-change-fn
          (lambda (i)
            (null (zerop (logand (aref (cdr (aref vec i)) 0) repeat-fu-flag-is-change)))))
         (elem-is-active-fn
          (lambda (i)
            (null
             (zerop (logand (aref (cdr (aref vec i)) 0) repeat-fu-preset-meow--flag-is-active)))))

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
          (incf i)))

      (cond
       ((eq -1 index-change)
        ;; A signal nothing could be found.
        nil)
       (t
        ;; Values inclusive.
        (let ((index-min index-change)
              (index-max index-change)

              (index-max-no-active index-change)

              (index-min-insert nil)
              (index-max-insert nil)

              (do-scan-for-active-commands t))

          ;; Expand while in insert mode.

          (when (eq 'insert (funcall elem-state-post-fn index-change))
            (setq index-min
                  (funcall elem-scan-while-min-fn
                           index-min
                           (lambda (i) (eq 'insert (funcall elem-state-post-fn i)))))
            (setq index-max
                  (funcall elem-scan-while-max-fn
                           index-max
                           (lambda (i) (eq 'insert (funcall elem-state-post-fn i)))))

            ;; Set range for the insert-mode-only macro.
            (setq index-min-insert index-min)
            (setq index-max-insert index-max)
            ;; If the first command "entered" insert mode, skip it.
            ;; Since the purpose is to replay in insert mode without entering/exiting.
            (unless (eq 'insert (funcall elem-state-pre-fn index-max-insert))
              (decf index-max-insert))

            (unless (funcall elem-is-change-fn index-max)
              ;; If entering insert mode did not change anything (not the "change" command).
              ;; Then don't include any previous motion.

              (setq do-scan-for-active-commands nil)))

          ;; If the state was exited, keep this as part of the repeated command.
          (when (and (> index-min 0) (eq 'meow-insert-exit (funcall elem-cmd-fn (1- index-min))))
            (decf index-min))

          ;; The last command before selection.
          (setq index-max-no-active index-max)

          ;; If the action operated on the selection.
          ;; Scan for the operations that built the selection.
          (when do-scan-for-active-commands
            ;; Editing command began with a selection.
            (when (and (< index-max vec-end) (funcall elem-is-active-fn (1+ index-max)))
              (let ((index-max-orig index-max)
                    (ok t))

                (while (and (< index-max vec-end) (funcall elem-is-active-fn (1+ index-max)))
                  (incf index-max)

                  ;; Any change within the selection breaks the chain.
                  (when (or (funcall elem-is-change-fn index-max)
                            (let ((cmd (funcall elem-cmd-fn index-max)))
                              (or
                               ;; Mouse event created the selection, ignore it.
                               (repeat-fu-command-test-skip-active cmd)
                               ;; Invoked by a jump action which should not be repeated.
                               (memq cmd repeat-fu-preset-meow--commands-mark-skip-jump))))
                    ;; Break.
                    (setq index-max vec-end)
                    (setq ok nil)))

                (unless ok
                  (setq index-max index-max-orig)))))

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
                (decf i)))

            ;; Change only macro (no preceding selection).
            (cond
             ((eq index-max index-max-no-active)
              (setq result-no-active result))
             (t
              (let ((i index-max-no-active))
                (while (>= i index-min)
                  (setq result-no-active (vconcat result-no-active (car (aref vec i))))
                  (decf i)))))

            ;; Insert mode only macro.
            (when (and index-min-insert index-max-insert)
              (let ((i index-max-insert))
                (while (>= i index-min-insert)
                  (setq result-only-insert (vconcat result-only-insert (car (aref vec i))))
                  (decf i))))

            (list result result-no-active result-only-insert))))))))

(defun repeat-fu-preset-meow--macros-select (kbuf-list)
  "Select the macro from KBUF-LIST."
  (nth
   (cond
    ((eq 'insert (meow--current-state))
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
(defun repeat-fu-preset-meow ()
  "Return the meow preset."
  (list
   :macros-extract #'repeat-fu-preset-meow--macros-extract
   :macros-select #'repeat-fu-preset-meow--macros-select
   :pre-data #'repeat-fu-preset-meow--pre-data
   :post-data #'repeat-fu-preset-meow--post-data))

(provide 'repeat-fu-preset-meow)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "repeat-fu.el"
;; End:
;;; repeat-fu-preset-meow.el ends here
