;;; init-flycheck.el --- -*- lexical-binding: t -*-


(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (if (not *gcc*)
      (add-to-list flycheck-disabled-checkers 'c/c++-gcc))
  (if (not *clang*)
      (add-to-list flycheck-disabled-checkers 'c/c++-clang))
  (if (not *cppcheck*)
      (add-to-list flycheck-disabled-checkers 'c/c++-cppcheck))
  :config
  ;(flycheck-add-mode 'javascript-eslint 'js-mode)
  ;(flycheck-add-mode 'typescript-tslint 'rjsx-mode)
  )

(provide 'init-flycheck)
