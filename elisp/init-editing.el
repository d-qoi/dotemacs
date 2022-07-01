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


;; (use-package yasnippet
;;   :ensure t
;;   :diminish yas-minor-mode
;;   :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
;;   :bind
;;   (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
;;   (:map yas-keymap
;;         (("TAB" . smarter-yas-expand-next-field)
;;          ([(tab)] . smarter-yas-expand-next-field)))
;;   :config
;;   (setq
;;    yas-verbosity 1                      ; No need to be so verbose
;;    yas-wrap-around-region t)
;;   (with-eval-after-load 'yasnippet
;;     (setq yas-snippet-dirs '(yasnippet-snippets-dir)))
;;   (yas-reload-all)
;;   (yas-global-mode)
;;   (defun smarter-yas-expand-next-field ()
;;     "Try to `yas-expand' then `yas-next-field' at current cursor position."
;;     (interactive)
;;     (let ((old-point (point))
;;           (old-tick (buffer-chars-modified-tick)))
;;       (yas-expand)
;;       (when (and (eq old-point (point))
;;                  (eq old-tick (buffer-chars-modified-tick)))
;;         (ignore-errors (yas-next-field))))))

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :after yasnippet)

(use-package highlight
  :ensure t)


(provide 'init-editing)
