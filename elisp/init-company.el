;;; init-company.el --- -*- lexical-binding: t -*-

(use-package company
  :ensure t
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode erlang-mode) . company-mode)
  ;; :bind
  ;; (:map company-active-map
  ;;       ([tab] . smarter-yas-expand-next-field-complete)
  ;;       ("TAB" . smarter-yas-expand-next-field-complete))
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
  ;; (defun smarter-yas-expand-next-field-complete ()
  ;;   "Try to `yas-expand' and `yas-next-field' at current cursor position.

  ;;  If failed try to complete the common part with `company-complete-common'"
  ;;   (interactive)
  ;;   (if yas-minor-mode
  ;;       (let ((old-point (point))
  ;;             (old-tick (buffer-chars-modified-tick)))
  ;;         (yas-expand)
  ;;         (when (and (eq old-point (point))
  ;;                    (eq old-tick (buffer-chars-modified-tick)))
  ;;           (ignore-errors (yas-next-field))
  ;;           (when (and (eq old-point (point))
  ;;                      (eq old-tick (buffer-chars-modified-tick)))
  ;;             (company-complete-common))))
  ;;     (company-complete-common)))
  ;;(add-hook 'after-init-hook 'global-company-mode)
  )

(use-package company-distel
  :if *erlang*
  :ensure t
  :after company
  :config
  (add-hook 'erlang-mode-hook
            (lambda ()
              (setq company-backends '(company-distel)))))
;;(require 'company-distel-frontend))

(provide 'init-company)
