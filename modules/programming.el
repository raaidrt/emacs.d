;;; programming.el --- Programming-related Packages -*- lexical-binding: t -*-

;;; Commentary:
;; Programming utilities and enhancements

;;; Code:

;; Project management
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config (counsel-projectile-mode))

;; Rainbow delimiters - Colored parentheses
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Smartparens - Automatic bracket pairing
(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; Highlight matching parens
(use-package paren
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0))

;; Helpful - Better help buffers
(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; Additional utilities
(use-package restart-emacs
  :ensure t)

(provide 'programming)
;;; programming.el ends here