;;; Initial buffer
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;;; Package manager. Dependencies
(package-initialize)
(require 'package)
(add-to-list 'package-archives
  '("MELPA" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
  '("MILKBOX" . "http://melpa.milkbox.net/packages/") t)
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

;;; Project management
(use-package projectile :ensure t)
(use-package flx-ido :ensure t)
(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;; Mac OS X
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;; Window mode settings
(when window-system
  (global-display-line-numbers-mode)
  (tool-bar-mode 0))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 150))

;;; Filesystem tree
(use-package neotree :ensure t)
(use-package all-the-icons :ensure t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;(neo-theme 'arrow)
(neotree-show)

;;; Color theme
(set-background-color "gray10")
(set-foreground-color "gray90")
(set-face-attribute 'region nil :background "#0e4686")

;;; Current line highlighting
(global-hl-line-mode 1)
(set-face-attribute 'default nil :background "gray15")
(set-face-attribute 'hl-line nil :inherit nil :background "gray30")

;;; Mac OS specific stuff
(setq ns-right-alternate-modifier nil)
(setq ring-bell-function 'ignore)

;;; Reload files if changed on disk
(global-auto-revert-mode t)

;; Temporal files to /tmp
(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

;; Indentation
(use-package dtrt-indent :ensure t)
(dtrt-indent-global-mode 1)

;; Miscellaneous
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(global-whitespace-mode t)
 '(global-whitespace-newline-mode t)
 '(package-selected-packages (quote (all-the-icons neotree dtrt-indent use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(whitespace-hspace ((t (:foreground "gray90"))))
 '(whitespace-line ((t nil)))
 '(whitespace-newline ((t (:foreground "gray20" :weight normal))))
 '(whitespace-space ((t (:foreground "gray30"))))
 '(default ((t (:inherit nil :stipple nil :background "gray15" :foreground "gray90" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 120 :width normal :foundry "nil" :family "SF Mono")))))
