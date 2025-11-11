;;; ui.el --- Basic UI Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Basic UI and visual settings for Emacs

;;; Code:

;; Basic UI Configuration
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(global-auto-revert-mode 1) ; Enable auto-revert-mode for all files

;; Set up the visible bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Load theme
(load-theme 'wombat)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Customize line number colors
(set-face-attribute 'line-number nil :foreground "#6B6B6B")
(set-face-attribute 'line-number-current-line nil :foreground "#FFA500")

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

(provide 'ui)
;;; ui.el ends here