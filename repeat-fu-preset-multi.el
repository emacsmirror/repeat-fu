;;; repeat-fu-preset-multi.el --- Multi-command repeat -*- lexical-binding: t -*-
;; URL: https://codeberg.org/ideasman42/emacs-repeat-fu
;; Version: 0.1

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025 Campbell Barton <ideasman42@gmail.com>

;; Author: Campbell Barton <ideasman42@gmail.com>

;;; Commentary:
;; Preset for Emacs to repeat multiple consecutive commands.
;;
;; Repeats the last changing edits
;; along with any preceding prefix arguments.
;; Multiple calls to the same command are grouped
;; so you can, for example, repeat text insertion elsewhere.
;;
;; Events creating a selection (active-region)
;; leading up to the edit will also be repeated
;; unless repeat runs with an active-region
;; in which case they will be skipped.

;;; Code:

(require 'repeat-fu)

(defconst repeat-fu-preset-multi--flag-is-active repeat-fu-flag-free)

;; This is not needed currently.
(defun repeat-fu-preset-multi--pre-data ()
  "Pre command callback."
  nil)

(defun repeat-fu-preset-multi--post-data (_data-pre)
  "Return macro data per step, used for replay."
  (cond
   ((repeat-fu-command-test-skip this-command)
    t)
   (t
    (let ((cmd-flag repeat-fu-this-command-bitmask))
      ;; Check `deactivate-mark' because the end user result is that the region isn't visible.
      (when (and (region-active-p) (null deactivate-mark))
        (setq cmd-flag (logior cmd-flag repeat-fu-preset-multi--flag-is-active)))

      `[,cmd-flag ,this-command]))))

(defun repeat-fu-preset-multi--macros-extract (cmd-buffer)
  "Extract the macro from CMD-BUFFER."
  (let* ((vec (vconcat cmd-buffer))
         (vec-len (length vec))
         (vec-end (1- vec-len))

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
             (zerop (logand (aref (cdr (aref vec i)) 0) repeat-fu-preset-multi--flag-is-active)))))

         (elem-cmd-fn (lambda (i) (aref (cdr (aref vec i)) 1))))

    ;; Find the first change.
    (let ((index-change -1))
      (let ((i 0))
        (while (< i vec-len)
          (when (funcall elem-is-change-fn i)
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
              (index-max-no-active nil)

              (result [])
              (result-no-active []))

          ;; Since this is the "multi" version and Emacs has no "insert" mode.
          ;; Include all changes with the matching command.
          ;; This accounts for text entry.
          (when (< index-max vec-end)
            (let* ((cmd-self-insert
                    (cond
                     ((derived-mode-p 'org-mode)
                      'org-self-insert-command)
                     (t
                      'self-insert-command)))

                   (cmd-self-insert-compat
                    (list 'backward-delete-char 'backward-delete-char-untabify))

                   (cmd-change
                    (let ((cmd (funcall elem-cmd-fn index-change)))
                      ;; Special case, detect if delete was part of text insertion,
                      ;; if so, treat this as text insertion.
                      (when (memq cmd cmd-self-insert-compat)
                        (let ((i index-change))
                          ;; Skip over consecutive delete commands.
                          (while (and (< i vec-end)
                                      (memq (funcall elem-cmd-fn (1+ i)) cmd-self-insert-compat))
                            (incf i))
                          (when (and (< i vec-end)
                                     (eq cmd-self-insert (funcall elem-cmd-fn (1+ i))))
                            (setq cmd cmd-self-insert))))

                      cmd))

                   (elem-change-ignore-fn
                    (lambda (i)
                      (let ((cmd (funcall elem-cmd-fn i)))
                        (cond
                         ((repeat-fu-command-test-skip-change (funcall elem-cmd-fn i))
                          t)
                         ;; This is not so fool-proof at edge cases.
                         ((eq cmd-change cmd-self-insert)
                          (memq cmd cmd-self-insert-compat))
                         (t
                          nil)))))

                   (elem-change-match-fn
                    (lambda (i)
                      (let ((cmd (funcall elem-cmd-fn i)))
                        (eq cmd cmd-change))))

                   (ok t))

              (while ok
                (setq ok nil)
                (let ((i index-max))
                  ;; Skip over ignore.
                  (while (and (< i vec-end) (funcall elem-change-ignore-fn (1+ i)))
                    (incf i))
                  ;; Scan up match.
                  (while (and (< i vec-end) (funcall elem-change-match-fn (1+ i)))
                    (incf i)
                    (setq ok t))

                  (when ok
                    (setq index-max i))))))

          (setq index-max-no-active index-max)

          ;; Editing command began with a selection.
          (when (and (< index-max vec-end) (funcall elem-is-active-fn (1+ index-max)))
            (let ((index-max-orig index-max)
                  (ok t))
              (while (and (< index-max vec-end) (funcall elem-is-active-fn (1+ index-max)))
                (incf index-max)

                ;; Any change within the selection breaks the chain.
                (when (funcall elem-is-change-fn index-max)
                  ;; Break.
                  (setq index-max vec-end)
                  (setq ok nil)))
              (unless ok
                (setq index-max index-max-orig))))

          ;; Finally add all numeric-prefix commands (if any).
          (while (and (< index-max vec-end) (funcall elem-is-prefix-fn (1+ index-max)))
            (incf index-max))

          (let ((i index-max))
            (while (>= i index-min)
              (setq result (vconcat result (car (aref vec i))))
              (decf i)))

          (cond
           ((eq index-max index-max-no-active)
            (setq result-no-active result))
           (t
            (let ((i index-max-no-active))
              (while (>= i index-min)
                (setq result-no-active (vconcat result-no-active (car (aref vec i))))
                (decf i)))))

          (list result result-no-active)))))))

(defun repeat-fu-preset-multi--macros-select (kbuf-list)
  "Select the macro from KBUF-LIST."
  (nth
   (cond
    ((region-active-p)
     ;; If there is already an active region, don't replay the commands
     ;; that created the active region.
     1)
    (t
     ;; Replay all commands (including active region creation).
     0))
   kbuf-list))

;;;###autoload
(defun repeat-fu-preset-multi ()
  "Return the multi preset."
  (list
   :macros-extract #'repeat-fu-preset-multi--macros-extract
   :macros-select #'repeat-fu-preset-multi--macros-select
   :pre-data #'repeat-fu-preset-multi--pre-data
   :post-data #'repeat-fu-preset-multi--post-data))

(provide 'repeat-fu-preset-multi)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "repeat-fu.el"
;; End:
;;; repeat-fu-preset-multi.el ends here
