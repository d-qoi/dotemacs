(setq debug-on-error t)

(setq custom-file "~/.emacs.d/emacs-custom.el")

(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;;(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/elisp")

;; Emacs customizations that do not require installations
(require 'init-defaults)
(require 'init-default-packages)


;; One of the last things to do.
(load custom-file)

;; and a prompt to save all unsaved customizations
(add-hook 'kill-emacs-query-functions
	  'custom-prompt-customize-unsaved-options)
