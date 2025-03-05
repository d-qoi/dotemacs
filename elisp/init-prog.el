;; -*- lexical-binding: t -*-

(require 'use-package)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

(unless (require 'treesit nil t)
  (defun treesit-available-p () nil))

(use-package eglot
  :straight (:type built-in)
  :custom
  (eglot-report-progress nil))

(defcustom d-qoi/eglot-disabled-projects nil
  "List of project paths where eglot should be disabled"
  :type '(repeat string)
  :group 'eglot)

(defun d-qoi/disable-eglot-advice (&rest _)
  (when-let ((project (project-current)))
    (member (project-root project) d-qoi/eglot-disabled-projects)))

(defun d-qoi/toggle-eglot-for-project ()
  (interactive)
  (when-let* ((project (project-current))
              (root (project-root project)))
    (if (member root d-qoi/eglot-disabled-projects)
        (progn
          (setq d-qoi/eglot-disabled-projects
                (delete root d-qoi/eglot-disabled-projects))
          (message "Eglot enabled for %s" root))
      (push root d-qoi/eglot-disabled-projects)
      (message "Eglot disabled for %s" root))))

(advice-add 'eglot-ensure :before-until #'d-qoi/disable-eglot-advice)

(advice-add 'eglot-format :before-until #'d-qoi/disable-eglot-advice)

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook 'eglot-format-buffer -10 t))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package yasnippet
  :straight (yasnippet :host github :repo "joaotavora/yasnippet")
  :demand t
  :diminish
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :straight (yasnippet-snippets :host github :repo "AndreaCrotti/yasnippet-snippets")
  :demand t
  :diminish
  :after yasnippet)

(use-package company
  :straight t
  :demand t
  :diminish
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

(use-package treesit-fold
  :if (treesit-available-p)
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :bind (:map treesit-fold-mode-map
        ("<tab>" . d-qoi/treesit-toggle-fold-or-tab))
  :init
  (defun d-qoi/treesit-toggle-fold-or-tab ()
    "either call `treesit-toggle-fold` or `indent-for-tab-command`"
    (interactive)
    (if (and (bolp)
             (not current-prefix-arg)
             (not (use-region-p))
             (treesit-fold-ready-p))
        (treesit-fold-toggle)
      (indent-for-tab-command)))
  :config
  (global-treesit-fold-mode))

(use-package treesit-fold-indicators
  :after treesit-fold
  :straight (treesit-fold-indicators :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :config
  (global-treesit-fold-indicators-mode))

(use-package move-text
  :straight t
  :bind (:map prog-mode-map
              ("M-<down>" . move-text-down)
              ("M-<up>" . move-text-up)))

(provide 'init-prog)
