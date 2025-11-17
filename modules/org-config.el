;;; org.el --- Org Mode Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org mode configuration and related packages

;;; Code:

;; Org mode configuration (from your Doom config)
(use-package org
  :ensure t
  :defer t
  :config
  ;; LaTeX preview settings
  (setq org-startup-with-latex-preview t)
  (add-hook 'org-mode-hook
            (lambda ()
              (setq org-format-latex-options 
                    (plist-put org-format-latex-options :scale 2.0))))
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
  (setq org-hide-emphasis-markers t)

  ;; Set up evil-collection for org mode after org is loaded
  (when (fboundp 'evil-collection-org-setup)
    (evil-collection-org-setup)))

;; Org bullets for prettier org mode
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

;; Htmlize for syntax highlighting in HTML exports
(use-package htmlize
  :ensure t
  :after org)

(provide 'org-config)
;;; org.el ends here