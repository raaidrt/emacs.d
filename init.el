;;; init.el --- Enhanced Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; A comprehensive Emacs configuration with Evil mode, LSP support,
;; and keybindings similar to Doom Emacs.

;;; Code:

;; Basic UI Configuration
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Font configuration
(set-face-attribute 'default nil :font "Monaco" :height 150)

;; Example for setting variable-pitch font for documentation
(set-face-attribute 'variable-pitch nil :family "Times New Roman" :height 1.1)

;; Load theme
(load-theme 'wombat)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Command logging for debugging
(use-package command-log-mode)

;; Evil mode - Vim keybindings
;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(defvar evil-want-keybinding)
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)

;; Evil Collection - Evil bindings for various modes
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; General.el for keybindings (more flexible than alternatives like hydra)
;; General provides the best flexibility for defining complex keybindings
;; and has excellent documentation
(use-package general
  :ensure t
  :demand t  ; Force immediate loading to avoid issues
  :preface
  (eval-when-compile (require 'general))
  (with-no-warnings
    (general-create-definer my-leader-def
      :states (quote (normal insert visual emacs motion))
      :keymaps (quote override)  ; Ensures bindings work in all modes
      :prefix "SPC"
      :global-prefix "C-SPC")

   (general-create-definer my-local-leader-def
      :states (quote (normal insert visual emacs motion))
      :keymaps (quote override)
      :prefix "SPC m"
      :global-prefix "C-SPC m"))
  :config
  (general-evil-setup t)  ; Set up Evil state bindings

  ;; Global keybindings similar to Doom Emacs
  (my-leader-def
    "SPC" 'execute-extended-command
    ":" 'eval-expression
    
    ;; Files
    "f" '(:ignore t :which-key "files")
    "ff" 'find-file
    "fr" 'recentf-open-files
    "fs" 'save-buffer
    "fS" 'write-file
    "fp" 'project-find-file
    
    ;; Buffers
    "b" '(:ignore t :which-key "buffers")
    "bb" 'switch-to-buffer
    "bd" 'kill-current-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "bR" 'revert-buffer
    "bs" 'save-buffer
    
    ;; Windows
    "w" '(:ignore t :which-key "windows")
    "wl" 'windmove-right
    "wh" 'windmove-left
    "wk" 'windmove-up
    "wj" 'windmove-down
    "w/" 'split-window-right
    "w-" 'split-window-below
    "wd" 'delete-window
    "wD" 'delete-other-windows
    "w=" 'balance-windows
    "w]" 'enlarge-window-horizontally
    "w[" 'shrink-window-horizontally
    
    ;; Help/Documentation
    "h" '(:ignore t :which-key "help")
    "hf" 'helpful-callable
    "hF" 'helpful-function
    "hv" 'helpful-variable
    "hk" 'helpful-key
    "hK" 'describe-keymap
    "hp" 'helpful-at-point
    "hm" 'describe-mode
    "h." 'display-local-help
    
    ;; Search
    "s" '(:ignore t :which-key "search")
    "ss" 'swiper
    "sS" 'swiper-all
    "sg" 'counsel-grep-or-swiper
    "sr" 'counsel-rg
    
    ;; Code/LSP
    "c" '(:ignore t :which-key "code")
    "ca" 'lsp-execute-code-action
    "cf" 'lsp-format-buffer
    "cF" 'lsp-format-region
    "cr" 'lsp-rename
    "cd" 'lsp-find-definition
    "cD" 'lsp-find-declaration
    "ci" 'lsp-find-implementation
    "ct" 'lsp-find-type-definition
    "cR" 'lsp-find-references
    "ce" 'flycheck-list-errors
    
    ;; Git
    "g" '(:ignore t :which-key "git")
    "gs" 'magit-status
    "gb" 'magit-blame
    "gl" 'magit-log
    
    ;; Org mode
    "o" '(:ignore t :which-key "org")
    "oa" 'org-agenda
    "oc" 'org-capture
    "ol" 'org-store-link
    
    ;; Toggle
    "t" '(:ignore t :which-key "toggle")
    "tt" 'treemacs
    "tl" 'display-line-numbers-mode
    "tw" 'whitespace-mode
    "tf" 'flycheck-mode
    
    ;; Quit
    "q" '(:ignore t :which-key "quit")
    "qq" 'save-buffers-kill-terminal
    "qQ" 'kill-emacs))


;; Which-key - Shows available keybindings
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-secondary-delay 0.05))

;; Ivy/Counsel/Swiper - Completion framework
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package ivy-rich
  :ensure t
  :after ivy
  :init
  (ivy-rich-mode 1))

;; Helpful - Better help buffers
(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; Doom Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  ((doom-modeline-height 15)
   (doom-modeline-bar-width 3)
   (doom-modeline-lsp t)
   (doom-modeline-github nil)
   (doom-modeline-minor-modes nil)
   (doom-modeline-persp-name nil)
   (doom-modeline-buffer-file-name-style 'truncate-except-project)))

;; All-the-icons (required for doom-modeline)
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

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

;; Magit - Git integration
(use-package magit
  :ensure t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Company - Completion at point
(use-package company
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-tooltip-align-annotations t))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; Flycheck - Syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; LSP Mode - Language Server Protocol support
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (latex-mode . lsp-deferred)
         (tex-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (javascript-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (when (fboundp 'lsp-enable-which-key-integration)
    (lsp-enable-which-key-integration t))
  (setq lsp-idle-delay 0.6)
  (setq lsp-log-io nil)
  (setq lsp-completion-enable t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-enable-folding t)
  (setq lsp-enable-imenu t)
  (setq lsp-enable-snippet t)
  (setq read-process-output-max (* 1024 1024))) ;; 1MB

;; LSP UI - UI enhancements for LSP
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-peek-enable t)
  :bind (:map lsp-ui-mode-map
         ("C-c l d" . lsp-ui-doc-toggle)
         ("C-c l p" . lsp-ui-peek-find-references)
         ("C-c l P" . lsp-ui-peek-find-definitions)
         ("C-c l i" . lsp-ui-peek-find-implementation)))

;; LSP Ivy integration
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;; LSP Treemacs integration
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

;; Python configuration with Ruff support
(use-package python-mode
  :ensure t
  :mode "\\.py\\'"
  :config
  ;; Avoid byte-compiler warnings: declare variables that are defined by lsp-pylsp
  (defvar lsp-pylsp-plugins-ruff-enabled)
  (defvar lsp-pylsp-plugins-ruff-format-enabled)
  (defvar lsp-pylsp-plugins-ruff-lint-enabled)
  (defvar lsp-pylsp-plugins-black-enabled)
  (defvar lsp-pylsp-plugins-autopep8-enabled)
  (defvar lsp-pylsp-plugins-yapf-enabled)
  (defvar lsp-pylsp-plugins-flake8-enabled)
  (defvar lsp-pylsp-plugins-pycodestyle-enabled)
  (defvar lsp-pylsp-plugins-pyflakes-enabled)
  (defvar lsp-pylsp-plugins-pylint-enabled)
  ;; Configure pylsp options only after lsp-pylsp is available
  (with-eval-after-load 'lsp-pylsp
    ;; Use ruff for formatting and linting
    (setq lsp-pylsp-plugins-ruff-enabled t)
    (setq lsp-pylsp-plugins-ruff-format-enabled t)
    (setq lsp-pylsp-plugins-ruff-lint-enabled t)
    ;; Disable other formatters/linters
    (setq lsp-pylsp-plugins-black-enabled nil)
    (setq lsp-pylsp-plugins-autopep8-enabled nil)
    (setq lsp-pylsp-plugins-yapf-enabled nil)
    (setq lsp-pylsp-plugins-flake8-enabled nil)
    (setq lsp-pylsp-plugins-pycodestyle-enabled nil)
    (setq lsp-pylsp-plugins-pyflakes-enabled nil)
    (setq lsp-pylsp-plugins-pylint-enabled nil)))

;; Alternative: pyright with ruff
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'my/python-format-buffer-with-ruff nil t))))

;; Use ruff for formatting
(defun my/python-format-buffer-with-ruff ()
  "Format Python buffer using ruff."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           "ruff format -"
                           nil t))

;; LaTeX configuration
(use-package auctex
  :ensure t
  :defer t
  :hook (LaTeX-mode . lsp-deferred)
  :config
  ;; Silence byte-compiler and set AUCTeX variables after load
  (defvar TeX-auto-save)
  (defvar TeX-parse-self)
  (defvar TeX-PDF-mode)
  (with-eval-after-load 'tex
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)
    (setq TeX-PDF-mode t)))

;; Org mode configuration (from your Doom config)
(use-package org
  :ensure t
  :config
  ;; LaTeX preview settings
  (setq org-startup-with-latex-preview t)
  (setq org-preview-latex-process-alist
        '((dvisvgm :programs ("latex" "dvisvgm")
           :description "dvi > svg"
           :message "you need to install the programs: latex and dvisvgm."
           :image-input-type "dvi"
           :image-output-type "svg"
           :image-size-adjust (1.7 . 1.5)
           :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
           :image-converter ("dvisvgm %f -n -b min -c %S --currentcolor -o %O"))))
  
  (setq org-latex-packages-alist
        '(("" "tikz" t)
          ("" "tikz-cd" t)))
  
  (setq org-preview-latex-default-process 'dvisvgm)
  
  ;; Enable LaTeX code block evaluation
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)))
  
  (setq org-log-done 'time)
  
  ;; Additional org settings
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t))

;; Org bullets for prettier org mode
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

;; Treemacs - File tree
(use-package treemacs
  :ensure t
  :defer t
  :init
  (defvar winum-keymap)
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay                0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init                t
          treemacs-expand-after-init                t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                      2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                  5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                    nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                     (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                         'left
          treemacs-read-string-input                'from-child-frame
          treemacs-recenter-distance                0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                      nil
          treemacs-show-hidden-files                t
          treemacs-silent-filewatch                 nil
          treemacs-silent-refresh                   nil
          treemacs-sorting                          'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup               t
          treemacs-tag-follow-delay                 1.5
          treemacs-text-scale                       nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width                70
          treemacs-width                            35
          treemacs-width-increment                  1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

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

;; Better minibuffer annotations
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Orderless completion style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark-consult)

;; Embark - Context actions
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Additional utilities
(use-package restart-emacs
  :ensure t)

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; Auto-save and backup settings
(setq auto-save-default t
      make-backup-files t
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Performance check
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons auctex command-log-mode company company-box counsel
		   counsel-projectile doom-modeline embark
		   embark-consult evil evil-collection flycheck
		   general helpful ivy ivy-rich lsp-ivy lsp-mode
		   lsp-pyright lsp-treemacs lsp-ui magit marginalia
		   orderless org-bullets projectile python-mode
		   rainbow-delimiters restart-emacs smartparens
		   treemacs-evil treemacs-magit treemacs-projectile
		   undo-tree)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
