;;; init-editing.el --- -*- lexical-binding: t -*-

(use-package iedit
  :ensure t
  :bind ("C-z ," . iedit-mode)
  :diminish)

(use-package iy-go-to-char
  :load-path (lambda () (expand-file-name "site-elisp" user-emacs-directory))
  :bind (("M-z g f" . iy-go-to-char)
         ("M-z g F" . iy-go-to-char-backward)
         ("M-z g ;" . iy-go-to-or-up-to-continue)
         ("M-z g ," . iy-go-to-or-up-to-continue-backward)))


(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  (:map yas-keymap
        (("TAB" . smarter-yas-expand-next-field)
         ([(tab)] . smarter-yas-expand-next-field)))
  :config
  (setq
   yas-verbosity 1                      ; No need to be so verbose
   yas-wrap-around-region t)
  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))
  (yas-reload-all)
  (yas-global-mode)
  (defun smarter-yas-expand-next-field ()
    "Try to `yas-expand' then `yas-next-field' at current cursor position."
    (interactive)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (yas-expand)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (ignore-errors (yas-next-field))))))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)


(use-package company
  :ensure t
  :diminish company-mode
  :after yasnippet
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :bind
  (:map company-active-map
        ([tab] . smarter-yas-expand-next-field-complete)
        ("TAB" . smarter-yas-expand-next-field-complete))
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-begin-commands '(self-insert-command))
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  :config
  (unless *clangd* (delete 'company-clang company-backends))
  (defun smarter-yas-expand-next-field-complete ()
    "Try to `yas-expand' and `yas-next-field' at current cursor position.

If failed try to complete the common part with `company-complete-common'"
    (interactive)
    (if yas-minor-mode
        (let ((old-point (point))
              (old-tick (buffer-chars-modified-tick)))
          (yas-expand)
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (ignore-errors (yas-next-field))
            (when (and (eq old-point (point))
                       (eq old-tick (buffer-chars-modified-tick)))
              (company-complete-common))))
      (company-complete-common)))
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-lsp
  :ensure t
  :defer t
  :after (:all company lsp-mode)
  :custom (company-lsp-cache-candidates 'auto))

(use-package highlight
  :ensure t)

(provide 'init-editing)
