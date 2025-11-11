;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons auctex claude-code command-log-mode company
		   company-box counsel counsel-projectile
		   doom-modeline eat embark embark-consult evil
		   evil-collection flycheck general helpful ivy
		   ivy-rich lsp-ivy lsp-mode lsp-pyright lsp-treemacs
		   lsp-ui magit marginalia mixed-pitch monet orderless
		   org-bullets projectile python-mode
		   rainbow-delimiters restart-emacs smartparens
		   treemacs-evil treemacs-magit treemacs-projectile
		   undo-tree vterm))
 '(package-vc-selected-packages
   '((monet :url "https://github.com/stevemolitor/monet")
     (claude-code :url
		  "https://github.com/stevemolitor/claude-code.el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:inherit fixed-pitch))))
 '(org-block-end-line ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit variable-pitch :weight bold :height 1.3))))
 '(org-drawer ((t (:inherit fixed-pitch))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit variable-pitch :weight bold :height 1.2))))
 '(org-level-2 ((t (:inherit variable-pitch :weight bold :height 1.15))))
 '(org-level-3 ((t (:inherit variable-pitch :weight bold :height 1.1))))
 '(org-level-4 ((t (:inherit variable-pitch :weight bold :height 1.05))))
 '(org-level-5 ((t (:inherit variable-pitch :weight bold))))
 '(org-level-6 ((t (:inherit variable-pitch :weight bold))))
 '(org-level-7 ((t (:inherit variable-pitch :weight bold))))
 '(org-level-8 ((t (:inherit variable-pitch :weight bold))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
