;;; fonts.el --- Font Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Font configuration for Emacs

;;; Code:

;; Font configuration
(set-face-attribute 'default nil :font "Monaco" :height 150)

;; Example for setting variable-pitch font for documentation
(set-face-attribute 'variable-pitch nil :family "Source Sans 3" :height 1.1)

(provide 'fonts)
;;; fonts.el ends here
