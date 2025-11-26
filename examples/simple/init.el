;;; init.el --- Example init. -*- lexical-binding: t -*-

;; This is a minimal example with HJKL motion, insert & normal modes.

;; Run with:
;; emacs --init-dir ./examples/simple

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

(setq use-package-always-ensure nil)
(setq use-package-always-defer t)

;; Defines all repeat-fu commands.
(require 'repeat-fu)
(setq repeat-fu-preset 'multi)

(delete-selection-mode t)

(global-set-key (kbd "C-.") 'repeat-fu-execute)

;; Enable repeat-fu for "typical" editing operation.
(add-hook
 'after-change-major-mode-hook
 (lambda ()
   (when (and (null (minibufferp)) (null (derived-mode-p 'special-mode)))
     (repeat-fu-mode))))


;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-load-packages-local: ("use-package")
;; End:
;;; init.el ends here
