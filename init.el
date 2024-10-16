;;; init.el --- -*- lexical-binding: t -*-

(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-elisp" user-emacs-directory))

(require 'init-helpers)

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
(require 'init-nav)
(require 'init-eshell)
(require 'init-org)
(require 'init-projects)
(require 'init-editing)

(require 'init-prog)
(require 'init-prog-markup)
(require 'init-prog-c)
(require 'init-prog-rust)
(require 'init-prog-cmake)
(require 'init-prog-go)
(require 'init-prog-py)
(require 'init-prog-web)
(require 'init-prog-just)

;; Other things
(require 'init-ai)

;; Custom file should be located somewhere in the load path,
;; probably in site-elisp
(require 'site-custom nil t)

;; One of the last things to do.
(if (file-exists-p custom-file)
    (load custom-file))

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
