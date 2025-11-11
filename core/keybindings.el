;;; keybindings.el --- Evil Mode and General Keybindings -*- lexical-binding: t -*-

;;; Commentary:
;; Evil mode configuration and global keybindings

;;; Code:

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

;; Window resizing functions
(defun custom/window-expand-repeatedly ()
  "Expand the window horizontally.  Continue expanding while ] is pressed."
  (interactive)
  (enlarge-window-horizontally 3)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "]")
                 (lambda ()
                   (interactive)
                   (enlarge-window-horizontally 3)
                   (message "Window width: %s" (window-width))))
     (define-key map (kbd "[")
                 (lambda ()
                   (interactive)
                   (shrink-window-horizontally 3)
                   (message "Window width: %s" (window-width))))
     map)
   t
   (lambda () (message "Window resizing done. Width: %s" (window-width)))))

(defun custom/window-shrink-repeatedly ()
  "Shrink the window horizontally.  Continue expanding while [ is pressed."
  (interactive)
  (shrink-window-horizontally 3)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "[")
                 (lambda ()
                   (interactive)
                   (shrink-window-horizontally 3)
                   (message "Window width: %s" (window-width))))
     (define-key map (kbd "[")
                 (lambda ()
                   (interactive)
                   (shrink-window-horizontally 3)
                   (message "Window width: %s" (window-width))))
     map)
   t
   (lambda () (message "Window resizing done. Width: %s" (window-width)))))

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
    "w]" 'custom/window-expand-repeatedly
    "w[" 'custom/window-shrink-repeatedly

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

;; Undo tree for Evil
(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(provide 'keybindings)
;;; keybindings.el ends here