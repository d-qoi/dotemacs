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


(use-package lsp-python-ms
  :ensure t
  :after lsp-mode python pyvenv-mode
  :if (and (or *python3* *python*) *python-use-lsp*)
  :custom
  (lsp-python-executable-cmd "python3"))

(use-package anaconda-mode
  :ensure t
  :after pyvenv-mode
  :diminish
  :if (not *python-use-lsp*)
  :hook ((python-mode-hook . anaconda-mode)
         (python-mode-hook . anaconda-eldoc-mode)))

(provide 'init-prog-py)
