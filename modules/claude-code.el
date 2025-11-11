;;; claude-code.el --- Claude Code Integration -*- lexical-binding: t -*-

;;; Commentary:
;; Claude Code integration and monet configuration

;;; Code:

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

;; install claude-code.el
(use-package claude-code 
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(provide 'claude-code)
;;; claude-code.el ends here