;;; init.el --- -*- lexical-binding: t -*-

(require 'use-package)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(add-to-list 'load-path "~/.emacs.d/elisp")

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


;; Require All the things
(require 'init-defaults)

(require 'init-global-packages)
(require 'init-projects)
(require 'init-prog)
(require 'init-prog-c)
(require 'init-prog-cmake)
(require 'init-editing)
(require 'init-nav)

;; One of the last things to do.
(if (file-exists-p custom-file)
    (load custom-file))

;; and a prompt to save all unsaved customizations
(add-hook 'kill-emacs-query-functions
	  'custom-prompt-customize-unsaved-options)
