;;; init.el --- Enhanced Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; A comprehensive Emacs configuration with Evil mode, LSP support,
;; and keybindings similar to Doom Emacs.
;; This configuration is split into modular files for better organization.

;;; Code:

;; Initialize package sources (must be done first)
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Function to recursively load all .el files from directories
(defun internal/load-directory-files (directories)
  "Load all .el files from the specified DIRECTORIES.
DIRECTORIES should be a list of directory names relative to
\='user-emacs-directory'."
  (dolist (dir directories)
    (let ((dir-path (expand-file-name dir user-emacs-directory)))
      (when (file-directory-p dir-path)
        (message "Loading files from directory: %s" dir-path)
        (let ((files (directory-files-recursively dir-path "\\.el$")))
          (dolist (file (sort files 'string<))
            (message "Loading file: %s" file)
            (load-file file)))))))

;; Add core and modules directories to load path
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load all configuration files from specified directories
(internal/load-directory-files '("core" "modules"))

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
