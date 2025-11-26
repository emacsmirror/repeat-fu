;;; init.el --- Example init for meow. -*- lexical-binding: t -*-

;; This is a minimal example with HJKL motion, insert & normal modes.

;; Run with:
;; emacs --init-dir ./examples/meow

;; Only to support running this within a git repository.
(add-to-list 'load-path (file-name-concat (file-name-directory user-init-file) ".." ".."))

;; No need for startup screen.
(setq inhibit-startup-screen t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ----------------------------------------------------------------------------
;; Packages

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Initialize when non-interactive so other commands succeed.
(when noninteractive
  (require 'package)
  (package-initialize))

(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(use-package repeat-fu
  :commands (repeat-fu-mode repeat-fu-execute)
  :config (setq repeat-fu-preset 'meow)
  :ensure nil

  :hook
  ((meow-mode)
   .
   (lambda ()
     (when (and (null (minibufferp)) (null (derived-mode-p 'special-mode)))
       (repeat-fu-mode)
       (define-key meow-normal-state-keymap (kbd "C-'") 'repeat-fu-execute)
       (define-key meow-insert-state-keymap (kbd "C-'") 'repeat-fu-execute)))))


;; ----------------------------------------------------------------------------
;; Meow

(use-package meow)

(defun meow-setup ()
  (meow-motion-define-key '("j" . meow-next) '("k" . meow-prev) '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  ;; Extra key binding.
  (define-key meow-normal-state-keymap (kbd "C-'") 'repeat-fu-execute)
  (define-key meow-insert-state-keymap (kbd "C-'") 'repeat-fu-execute))

(meow-global-mode)

(meow-setup)

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-load-packages-local: ("use-package" "use-package-core")
;; End:
;;; init.el ends here
