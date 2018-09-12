
;;; Initial buffer
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;;; Package manager repositories
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("MELPA" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("MELPA-STABLE" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("MILKBOX" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("MARMALADE" . "http://marmalade-repo.org/packages/") t)
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))
(unless package-archive-contents (package-refresh-contents))

;;; Markdown
(use-package markdown-mode :ensure t)

;;; Git
(use-package magit :ensure t)
(global-set-key (kbd "C-x g") 'magit-status)

;;; Environment
(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

;;; Common
(electric-pair-mode 1)
(use-package fill-column-indicator :ensure t)
(setq fci-rule-column 80)
(add-hook 'after-load-functions 'fci-mode)

;;; Flycheck
(use-package flycheck :ensure t)
(global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(add-hook 'js-mode-hook
          (defun my-js2-mode-setup ()
            (flycheck-mode t)
            (when (executable-find "eslint")
              (flycheck-select-checker 'javascript-eslint))))
(add-hook 'js2-mode-hook
          (defun my-js2-mode-setup ()
            (flycheck-mode t)
            (when (executable-find "eslint")
              (flycheck-select-checker 'javascript-eslint))))
;(setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))

;;; JSON
(use-package json-mode :ensure t)

;;; Project management
(use-package projectile :ensure t)
(use-package flx-ido :ensure t)
(projectile-global-mode)
(setq projectile-use-git-grep t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map) ;; CMD+p for project-related stuff
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;; Mac OS X
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;;; My defaults
(setq-default indent-tabs-mode nil)

;;; Coffeescript
(use-package coffee-mode :ensure t)

;;; Javascript
(use-package js2-mode :ensure t)
; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(use-package js2-refactor :ensure t)
(use-package xref-js2 :ensure t)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(use-package eslintd-fix :ensure t)
(add-hook 'js2-mode-hook 'eslintd-fix-mode)
(add-hook 'js-mode-hook 'eslintd-fix-mode)

;;; Autocomplete
(use-package company :ensure t)
(use-package tide :ensure t)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
;; formats the buffer before saving
(add-hook 'js-mode-hook #'setup-tide-mode)
(add-hook 'after-init-hook 'global-company-mode)

;;; Window mode settings
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
(set-face-attribute 'region nil :background "gray30")

;;; Current line highlighting
(global-hl-line-mode 1)
(set-face-attribute 'default nil :background "gray15")
(set-face-attribute 'hl-line nil :inherit nil :background "gray20")

;;; Mac OS specific stuff
(setq ns-right-alternate-modifier nil)
(setq ring-bell-function 'ignore)

;;; Reload files if changed on disk
(global-auto-revert-mode t)

;;; Snippets
(use-package yasnippet :ensure t)
(yas-global-mode 1)

;; Temporal files to /tmp
(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

;; Indentation
(use-package dtrt-indent :ensure t)
(dtrt-indent-global-mode 1)
(setq default-tab-width 2)

;; Miscellaneous
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(electric-pair-mode t)
 '(flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
 '(flycheck-highlighting-mode (quote sexps))
 '(js-indent-level 2)
 '(js2-strict-trailing-comma-warning nil)
 '(neo-window-width 50)
 '(package-selected-packages
   (quote
    (json-mode fill-column-indicator coffee-mode magit markdown-mode all-the-icons neotree dtrt-indent use-package)))
 '(tab-width 3))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray15" :foreground "gray90" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 120 :width normal :foundry "nil" :family "SF Mono"))))
 '(flycheck-error ((t (:background "Red1")))))

