;;; lsp.el --- Language Server Protocol Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; LSP mode configuration and language-specific settings

;;; Code:

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

;; Python LSP configuration
;; Using pyright for type checking and navigation
;; Using ruff server for linting and formatting
;; Install: pip install pyright ruff

;; Pyright - Python type checker and LSP
(use-package lsp-pyright
  :ensure t
  :custom
  ;; Use pyright as the primary Python LSP server
  (lsp-pyright-langserver-command "pyright")
  ;; Disable type checking overlaps if you prefer ruff for linting
  (lsp-pyright-disable-language-services nil)
  (lsp-pyright-disable-organize-imports nil)
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

;; Ruff - Modern Python linter and formatter
;; As of 2024, use 'ruff server' instead of deprecated 'ruff-lsp'
(with-eval-after-load 'lsp-mode
  ;; Disable pylsp to avoid conflicts (we're using pyright instead)
  (setq lsp-disabled-clients '(pylsp))

  ;; Enable ruff server (requires ruff >= 0.5.3)
  ;; This runs alongside pyright as an add-on server
  (setq lsp-ruff-lsp-server-command '("ruff" "server"))

  ;; Configure which servers to use for Python
  (defvar lsp-enabled-clients)
  (add-to-list 'lsp-enabled-clients 'pyright)
  (add-to-list 'lsp-enabled-clients 'ruff))

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

(provide 'lsp)
;;; lsp.el ends here