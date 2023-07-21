;;; init.el --- -*- lexical-binding: t -*-

(require 'find-lisp)
(require 'autoload)

(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-elisp" user-emacs-directory))

;; Autoload from site-elisp, unsure where to do this, so doing it now
(setq site-elisp-autoload-file (expand-file-name "site-elisp/autoloads.el" user-emacs-directory))

(defun d-qoi/generate-site-elisp-autoload ()
  "Generate autoloads for site-elisp directory."
  (interactive)
  (let ((site-elisp-dir (expand-file-name "site-elisp" user-emacs-directory)))
    (if (version< emacs-version "29")
	    (update-directory-autoloads site-elisp-dir)
      (loaddefs-generate site-elisp-dir site-elisp-autoload-file))))

(unless (file-exists-p site-elisp-autoload-file)
  (d-qoi/generate-site-elisp-autoload))

(load site-elisp-autoload-file nil t)

(defconst system-emacs-dirs
  '("/usr/share/emacs/"
    "/usr/local/share/emacs/"))

(defun d-qoi/find-lisp-in-dirs (dirs name)
  (when dirs
    (let* ((current-dir (car dirs))
          (next-dirs (cdr dirs))
          (result (find-lisp-find-files current-dir name)))
      (if result
          result
        (d-qoi/find-lisp-in-dirs next-dirs name)))))

;; package setup
(defvar bootstrap-version)
(setq straight-repository-branch "develop")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(if (require 'use-package nil t)
    (straight-use-package '(use-package :type built-in))
  (straight-use-package 'use-package))
(setq use-package-verbose t)

(if (require 'eglot nil t)
    (straight-use-package '(eglot :type built-in))
  (straight-use-package 'eglot))

;; Require All the things
(require 'init-defaults)
(require 'init-global-packages)
(require 'init-eshell)
(require 'init-nav)
(require 'init-org)
(require 'init-projects)
(require 'init-editing)

(require 'init-prog)
(require 'init-prog-markup)
(require 'init-prog-c)
(require 'init-prog-cmake)
(require 'init-prog-go)

(require 'init-helpers)

;; One of the last things to do.
(if (file-exists-p custom-file)
    (load custom-file))

;; and a prompt to save all unsaved customizations
(add-hook 'kill-emacs-query-functions
	  'custom-prompt-customize-unsaved-options)
(put 'narrow-to-region 'disabled nil)
