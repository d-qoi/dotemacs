;;; init.el --- -*- lexical-binding: t -*-

(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-elisp" user-emacs-directory))

;; Autoload from site-elisp, unsure where to do this, so doing it now
(setq generated-autoload-file (expand-file-name "site-elisp/autoloads.el" user-emacs-directory))
(let ((site-elisp-dir (expand-file-name "site-elisp" user-emacs-directory)))
  (if (version< emacs-version "29")
	(update-directory-autoloads site-elisp-dir)
    (loaddefs-generate site-elisp-dir generated-autoload-file))
  (load generated-autoload-file nil t))


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

(unless (require 'use-package nil t)
  (straight-use-package 'use-package)
  (setq use-package-verbose t))

(unless (require 'eglot nil t)
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
