;;; claude-config.el --- Claude Code Integration -*- lexical-binding: t -*-

;;; Commentary:
;; Claude Code integration and monet configuration

;;; Code:

;; Monet for IDE integration
(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

;; Claude Code (already installed via package-vc in custom.el)
(use-package claude-code
  :demand t
  :config
  ;; Optional IDE integration with Monet
  (when (featurep 'monet)
    (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
    (monet-mode 1))

  ;; Enable claude-code-mode globally
  (claude-code-mode 1)

  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(provide 'claude-config)
;;; claude-config.el ends here
