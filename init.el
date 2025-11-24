;;; init.el --- Literate Configuration Bootstrap -*- lexical-binding: t -*-

;;; Commentary:
;; This file bootstraps the literate Emacs configuration from init.org.
;; The actual configuration is in init.org - edit that file instead of this one.
;; This file tangles init.org to config.el and loads it.

;;; Code:

;; Bootstrap package.el for org-mode
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize packages
(when (version< emacs-version "27.0")
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; Ensure org is up to date
(unless (package-installed-p 'org)
  (package-install 'org))

;; Load and tangle the literate configuration
(defun load-literate-config ()
  "Load configuration from init.org."
  (let* ((init-org (expand-file-name "init.org" user-emacs-directory))
         (config-el (expand-file-name "config.el" user-emacs-directory)))

    (when (file-exists-p init-org)
      ;; Only tangle if init.org is newer than config.el
      (when (or (not (file-exists-p config-el))
                (file-newer-than-file-p init-org config-el))
        (message "Tangling init.org...")
        (require 'org)
        (org-babel-tangle-file init-org config-el "emacs-lisp"))

      ;; Load the tangled configuration
      (when (file-exists-p config-el)
        (message "Loading configuration from init.org...")
        (load-file config-el)))))

;; Load the configuration
(load-literate-config)

(provide 'init)
;;; init.el ends here
