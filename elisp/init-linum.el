;;; init-linum.el --- -*- lexical-binding: t -*-

(require 'linum)

(global-linum-mode t)

(setq column-number-mode t)
(setq linum-format "%d|")

(defvar linum-disabled-modes
  '(term-mode help-mode image-mode))

(defun d-qoi/linum-disable-function ()
  "Function for the hook for after major mode change."
  (defun recloop (val lst)
    ;;(message "recloop %s %s" val (car lst))
    (cond
     ((equal '() lst) 1)
     ((equal val (car lst)) 0)
     (t (recloop val (cdr lst)))))
  (linum-mode (recloop major-mode linum-disabled-modes)))

(add-hook 'after-change-major-mode-hook 'd-qoi/linum-disable-function)

(provide 'init-linum)
