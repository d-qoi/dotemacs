;; -*- lexical-binding: t -*-

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))
  :bind (:map chatgpt-shell-mode-map
         ("C-x C-r" . chatgpt-shell-restore-session-from-transcript))
  :config
  (setq chatgpt-shell-openai-key
        (lambda () (auth-source-pick-first-password :host "api.openai.com"))))


(use-package claude-shell
  :straight (claude-shell :type git :host github :repo "arminfriedl/claude-shell")
  :custom
  (claude-shell-streaming t)
  (claude-shell-model "claude-3-5-sonnet-20240620")
  :config
  (setq claude-shell-api-token
        (lambda () (auth-source-pick-first-password :host "api.anthropic.com"))))

(provide 'init-ai)
