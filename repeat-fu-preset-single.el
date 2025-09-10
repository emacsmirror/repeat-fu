;;; repeat-fu-preset-single.el --- Simple preset -*- lexical-binding: t -*-
;; URL: https://codeberg.org/ideasman42/emacs-repeat-fu
;; Version: 0.1

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025 Campbell Barton <ideasman42@gmail.com>

;; Author: Campbell Barton <ideasman42@gmail.com>

;;; Commentary:
;; Preset for Emacs (single) repeat.
;;
;; This is a very simple form of repeating.
;;
;; - Find the last change.
;; - Include any prefix commands.

;;; Code:

(require 'repeat-fu)

;; This is not needed currently.
(defun repeat-fu-preset-single--pre-data ()
  "Pre command callback."
  nil)

(defun repeat-fu-preset-single--post-data (_data-pre)
  "Return macro data per step, used for replay."
  (cond
   ((repeat-fu-command-test-skip this-command)
    t)
   (t
    `[,repeat-fu-this-command-bitmask ,this-command])))

(defun repeat-fu-preset-single--macros-extract (cmd-buffer)
  "Extract the macro from CMD-BUFFER."
  (let* ((vec (vconcat cmd-buffer))
         (vec-len (length vec))
         (vec-end (1- vec-len))

         ;; Local lookups.
         (elem-is-prefix-fn
          (lambda (i) (not (zerop (logand (aref (cdr (aref vec i)) 0) repeat-fu-flag-is-prefix)))))
         (elem-is-change-fn
          (lambda (i) (not (zerop (logand (aref (cdr (aref vec i)) 0) repeat-fu-flag-is-change)))))

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
              (result []))

          ;; Finally add all numeric-prefix commands (if any).
          (while (and (< index-max vec-end) (funcall elem-is-prefix-fn (1+ index-max)))
            (incf index-max))

          (let ((i index-max))
            (while (>= i index-min)
              (setq result (vconcat result (car (aref vec i))))
              (decf i))
            (list result))))))))

;;;###autoload
(defun repeat-fu-preset-single ()
  "Return the single preset."
  (list
   :macros-extract #'repeat-fu-preset-single--macros-extract
   :macros-select #'car
   :pre-data #'repeat-fu-preset-single--pre-data
   :post-data #'repeat-fu-preset-single--post-data))

(provide 'repeat-fu-preset-single)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "repeat-fu.el"
;; End:
;;; repeat-fu-preset-single.el ends here
