(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(use-package company
  :ensure t
  :diminish company-mode
  :bind (("M-C-/" . company-complete)
         :map company-mode-map
         ("M-/" . company-complete)
         :map company-active-map
         ("M-/" . company-other-backend)
         ("M-n" . company-select-next)
         ("M-p" . company-select-previous)))

(use-package company-quickhelp
  :ensure t
  :after (company)
  :config
  (add-hook 'after-init-hook 'company-quickhelp-mode))

(provide 'init-company)
