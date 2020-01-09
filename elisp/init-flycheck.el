;;; init-flycheck.el --- -*- lexical-binding: t -*-


(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  ;(flycheck-add-mode 'javascript-eslint 'js-mode)
  ;(flycheck-add-mode 'typescript-tslint 'rjsx-mode)
  )

(provide 'init-flycheck)
