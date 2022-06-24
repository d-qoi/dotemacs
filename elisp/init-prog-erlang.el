;;; init-prog-erlang.el --- -*- lexical-binding: t -*-

(require 'constants)

(when *erlang*
  (let* ((erlang-root-dir
          (file-name-directory (directory-file-name (file-name-directory *erlang*))))
         (erlang-emacs-dir
          (file-expand-wildcards
           (concat (file-name-directory erlang-root-dir) "lib/tools-*/emacs"))))
    (push (car erlang-emacs-dir) load-path)
    (require 'erlang-start)
    (setq exec-path (cons (concat erlang-root-dir "bin") exec-path))
    (setq erlang-man-root-dir (concat erlang-root-dir "man"))))

(use-package distel
  :if *erlang*
  :load-path (lambda () (expand-file-name "site-elisp/distel/elisp" user-emacs-directory))
  :config
  (distel-setup)
  ;; taken from https://www.lambdacat.com/post-modern-emacs-setup-for-erlang/
  ;; prevent annoying hang-on-compile
  (defvar inferior-erlang-prompt-timeout t)
  ;; default node name to emacs@localhost
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; tell distel to default to that node
  (setq erl-nodename-cache
        (make-symbol
         (concat
          "emacs@"
          ;; Mac OS X uses "name.local" instead of "name", this should work
          ;; pretty much anywhere without having to muck with NetInfo
          ;; ... but I only tested it on Mac OS X.
          (car (split-string (shell-command-to-string "hostname")))))))

(provide 'init-prog-erlang)
