;;; init-prog-rust.el --- -*- lexical-bindings: t -*-

(use-package rustic
  :ensure t
  :after lsp-mode
  :if (and *rust-cargo* *rustc* *rust-analyzer*)
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :custom
  ;; "check" is used as default
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (setq rustic-format-on-save t))

(lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection "rust-analyzer")
                     :major-modes '(rust-mode)
                     :remote? t
                     :server-id 'rust-analyzer-remote))

(provide 'init-prog-rust)
