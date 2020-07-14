;;; init-flycheck.el --- -*- lexical-binding: t -*-

(require 'ansi-color)

(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (if (not *gcc*)
      (add-to-list flycheck-disabled-checkers 'c/c++-gcc))
  (if (not *clang*)
      (add-to-list flycheck-disabled-checkers 'c/c++-clang))
  (if (not *cppcheck*)
      (add-to-list flycheck-disabled-checkers 'c/c++-cppcheck))

  (when *erlang*
    (defun flycheck-rebar3-project-root (&optional _checker)
      "Return directory where =rebar.config= is located."
      (let* ((rebarconfig (locate-dominating-file buffer-file-name "rebar.config"))
             (project-dir (if rebarconfig
                              (file-name-as-directory
                               (substring rebarconfig
                                          0
                                          (string-match
                                           "\\(/_build/[^/]+/lib\\|/_checkouts\\|/apps\\)"
                                           rebarconfig)))
                            rebarconfig)))
        project-dir))

    (flycheck-define-checker erlang-otp
      "An Erlang syntax checker using the Erlang interpreter."
      :command ("erlc" "-o" temporary-directory "-Wall"
                "-I" "../include" "-I" "../../include"
                "-I" "../../../include" source)
      :error-patterns
      ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
       (error line-start (file-name) ":" line ": " (message) line-end)))

    (flycheck-define-checker erlang-rebar3
      "An Erlang syntax checker using the rebar3 build tool."
      :command ("rebar3" "compile")
      :error-parser
      (lambda (output checker buffer)
        (flycheck-parse-with-patterns (ansi-color-filter-apply output)
                                      checker
                                      buffer))
      :error-patterns
      ((warning line-start
                (file-name) ":" line ": Warning:" (message) line-end)
       (error line-start
              (file-name) ":" line ": " (message) line-end))
      :modes erlang-mode
      :enabled (lambda () (flycheck-rebar3-project-root))
      :predicate (lambda () (flycheck-buffer-saved-p))
      :working-directory flycheck-rebar3-project-root)

    (flycheck-add-next-checker 'erlang-otp 'erlang-rebar3)


    (add-hook 'erlang-mode-hook
              (lambda ()
                (flycheck-select-checker 'erlang-otp)
                (flycheck-mode))))
  ; :config
  ; (flycheck-add-mode 'javascript-eslint 'js-mode)
  ; (flycheck-add-mode 'typescript-tslint 'rjsx-mode)
  )

(use-package flycheck-tip
  :ensure t
  :after flycheck
  :custom
  (flycheck-tip-use-timer 'verbose))

(provide 'init-flycheck)
