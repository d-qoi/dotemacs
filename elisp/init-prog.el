;; -*- lexical-binding: t -*-

(require 'use-package)

(unless (require 'treesit nil t)
  (defun treesit-available-p () nil))

(use-package eglot
  :straight (:type built-in))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package yasnippet
  :straight (yasnippet :host github :repo "joaotavora/yasnippet")
  :demand t
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :straight (yasnippet-snippets :host github :repo "AndreaCrotti/yasnippet-snippets")
  :demand t
  :after yasnippet)

(use-package company
  :straight t
  :demand t
  :after orderless
  :bind
  (("C-c C-/" . company-other-backend)
   ("C-c Y" . company-yasnippet))
  :config
  (setq company-tooltip-flip-when-above t)
  (setq company-backends '((company-capf
                            company-semantic
                            company-yasnippet
                            company-dabbrev-code
                            company-files
                            company-keywords)))
  (global-company-mode 1))

(require 'semantic)
(require 'semantic/db)
(require 'project)
(defun d-qoi/project-semantic-project-root-function (dir)
  (if-let ((root (project-current nil dir)))
      (project-root root)))
(add-to-list 'semanticdb-project-root-functions 'd-qoi/project-semantic-project-root-function)
(semantic-mode 1)

;; We follow a suggestion by company maintainer u/hvis:
;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
;; (defun company-completion-styles (capf-fn &rest args)
;;   (let ((completion-styles '(basic partial-completion)))
;;     (apply capf-fn args))

;; (advice-add 'company-capf :around #'company-completion-styles)

(use-package consult-eglot
  :straight t
  :demand t
  :after (:all consult))


(use-package idle-highlight-mode
  :disabled
  :straight t
  :hook (prog-mode-hook . idle-highlight-mode))

(defconst *tree-sit-dist-dir* "third-party/tree-sitter-module/dist"
  "Destination directory of tree-sitter shared objects")

(defun d-qoi/get-tree-sitter-languages (&optional language)
  "Get languages built already by third-party/tree-sitter-module"
  (let* ((default-directory user-emacs-directory)
         (dist (expand-file-name *tree-sit-dist-dir*)))
    (when (file-exists-p dist)
      (let* ((file-names (directory-files dist nil "libtree-sitter"))
             (names (mapcar (lambda (file-name)
                             (string-match "libtree-sitter-\\(.*\\)\\.so" file-name)
                             (match-string 1 file-name)) file-names)))
        (if language
            (member language names)
          names)))))

(unless (treesit-available-p)
  (warn "Tree sitter is not available."))
(when (treesit-available-p)
  (setq treesit-extra-load-path (list (expand-file-name *tree-sit-dist-dir* user-emacs-directory))))

(use-package treesit-auto
  :straight t
  :if (treesit-available-p)
  :demand t
  :config
  (global-treesit-auto-mode))

(provide 'init-prog)
