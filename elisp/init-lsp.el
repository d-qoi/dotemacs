;;; init-lsp.el --- -*- lexical-binding: t -*-

(use-package lsp-mode
  :ensure t
  :defer t
  :commands lsp
  :if *lsp*
  :custom
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 20000)
  (read-process-output-max (* 1024 1024))
  :config
  (if *clangd*
      (progn
        (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
        (setq lsp-clients-clangd-executable *clangd*)))
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((python-mode c-mode c++-mode) . lsp))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  (lsp-ui-doc-border "orange")
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if *sys/gui*
      (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(provide 'init-lsp)
