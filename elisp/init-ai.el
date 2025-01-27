;; -*- lexical-binding: t -*-

(use-package shell-maker
  :straight (:host github :repo "xenodium/shell-maker"))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell")
  :bind (:map chatgpt-shell-mode-map
         ("C-x C-r" . chatgpt-shell-restore-session-from-transcript))
  :config
  (setq chatgpt-shell-openai-key
        (lambda () (auth-source-pick-first-password :host "api.openai.com")))
  (setq chatgpt-shell-anthropic-key
        (lambda () (auth-source-pick-first-password :host "api.anthropic.com"))))

(provide 'init-ai)
