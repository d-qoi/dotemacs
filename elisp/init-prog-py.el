;;; init-prog-py.el --- -*- lexical-binding: t -*-

(require 'constants)

(use-package python-mode
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))


(use-package pyenv-mode
  :ensure t
  :after python)


;; (use-package lsp-python-ms
;;   :ensure t
;;   :if *python-use-lsp*
;;   :hook (python-mode . (lambda ()
;;                               (require 'lsp-python-ms)
;;                               (lsp)))
;;   :custom
;;   (lsp-python-executable-cmd "python3")
;;   (lsp-python-ms-executable (concat user-emacs-directory
;;                                     "site-elisp/python-language-server/output/bin/Release/Microsoft.Python.LanguageServer")))



(use-package anaconda-mode
  :ensure t
  :after pyvenv-mode
  :diminish
  :if (not *python-use-lsp*)
  :hook ((python-mode-hook . anaconda-mode)
         (python-mode-hook . anaconda-eldoc-mode)))

(provide 'init-prog-py)
