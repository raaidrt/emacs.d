;;; git.el --- Git Integration Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Magit and Git-related packages

;;; Code:

;; Magit - Git integration
(use-package magit
  :ensure t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(provide 'git)
;;; git.el ends here