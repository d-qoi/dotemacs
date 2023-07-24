;; -*- lexical-binding: t -*-

(require 'eglot)
(require 'use-package)

(defcustom *clangd*
  (executable-find "clangd")
  "Do we have clangd?"
  :group 'c)

(defcustom *clang-format*
  (executable-find "clang-format")
  "Do we have Clang-format?"
  :group 'c)

(defcustom *clang-format-elisp-file*
  (and *clang-format* (d-qoi/find-lisp-in-dirs system-emacs-dirs "clang-format"))
  "Where is the elisp file for clang-format?"
  :group 'c)

(setq c-basic-offset 4
      c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

(defun d-qoi/c-initialization-hook ()
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (c-toggle-electric-state 1))
;;(add-hook 'c-initialization-hook 'd-qoi/c-initialization-hook)


(when (treesit-available-p)
  (when (treesit-language-available-p 'c)
    (add-hook 'c-ts-mode-hook (lambda () (run-hooks 'c-mode-hook)))
    (add-to-list 'semantic-new-buffer-setup-functions
                 '(c-ts-mode . semantic-default-c-setup)))

  (when (treesit-language-available-p 'cpp)
    (add-hook 'c++-ts-mode-hook (lambda () (run-hooks 'c++-mode-hook)))
    (add-to-list 'semantic-new-buffer-setup-functions
                 '(c++-ts-mode . semantic-default-c-setup))))

(defun d-qoi/init-prog-c-after-init ()
  (when *clangd*
    (add-to-list 'eglot-server-programs
                 '((c-mode c++-mode)
                   . ("clangd"
                      "-j=8"
                      "--pretty"
                      "--log=verbose"
                      "--background-index"
                      "--clang-tidy"
                      "--completion-style=detailed"
                      "--pch-storage=memory"
                      "--header-insertion=never"
                      "--header-insertion-decorators=0")))
    (add-hook 'c++-mode-hook 'eglot-ensure)
    (add-hook 'c-mode-hook 'eglot-ensure))

  ;; Pulled into after-init function so *clang-format* can be set in customise
  ;; pull from melpa if needed
  (use-package clang-format
    :if (and *clang-format* (not *clang-format-elisp-file*))
    :straight t
    :bind (:map c-mode-base-map
                ("C-M-TAB" . clang-format-region))
    :init
    (message "Clang-format pulled from Melpa"))

  ;; pull from melpa if needed
  (use-package clang-format
    :if (and *clang-format* *clang-format-elisp-file*)
    :straight (:type built-in)
    :bind (:map c-mode-base-map
                ("C-M-TAB" . clang-format-region))
    :init
    (push (f-dirname (car *clang-format-elisp-file*)) load-path)
    (message "Clang-format found locally")))

(add-hook 'after-init-hook 'd-qoi/init-prog-c-after-init)

;; TODO Pull cpplint into its own file.

(defvar-local cpplint--flymake-proc nil)

(defgroup cpplint nil
  "cpplint config"
  :group 'c)

(defcustom cpplint-executable
  (executable-find "cpplint")
  "Do we have cpplint?"
  :group 'cpplint)

(defcustom cpplint-verbosity 3
  "Specify a number 0-5 to restrict errors to certain verbosity levels.
Errors with lower verbosity levels have lower confidence and are more
likely to be false positives. 0 is least confident, 5 is most confident."
  :group 'cpplint
  :type '(number :tag "0-5"))

(defcustom cpplint-filter nil
  "Specify a comma-separated list of category-filters to apply: only
error messages whose category names pass the filters will be printed.
(Category names are printed with the message and look like
\"[whitespace/indent]\".)  Filters are evaluated left to right.
\"-FOO\" means \"do not print categories that start with FOO\".
\"+FOO\" means \"do print categories that start with FOO\".

Examples: --filter=-whitespace,+whitespace/braces
          --filter=-whitespace,-runtime/printf,+runtime/printf_format
          --filter=-,+build/include_what_you_use

To see a list of all the categories used in cpplint, pass no arg:
   --filter="
  :group 'cpplint)

(defcustom cpplint-linelength 120
  "This is the allowed line length for the project. The default value is
80 characters.

Examples:
  --linelength=120"
  :type '(number :tag "80 or higher")
  :group 'cpplint)

(defcustom cpplint-counting 'total
  "The total number of errors found is always printed. If
'toplevel' is provided, then the count of errors in each of
the top-level categories like 'build' and 'whitespace' will
also be printed. If 'detailed' is provided, then a count
is provided for each category like 'build/class'."
  :type '(choice
          (const :tag "Total" "total")
          (const :tag "Top Level" "toplevel")
          (const :tag "Detailed" "detailed"))
  :group 'cpplint)

(defun cpplint-create-command (filename)
  "Construct a command that flymake can use to check C/C++ source."
  (remove nil
          (list
           cpplint-executable
           "--output=emacs"
           (format "--verbose=%d" cpplint-verbosity)
           (if cpplint-filter (format "--filter=%s" cpplint-filter))
           (format "--counting=%s" cpplint-counting)
           (format "--linelength=%d" cpplint-linelength)
           filename)))

(defun cpplint-flymake (report-fn &rest _args)
  "Flymake CPPLine backend.
Copied mostly from https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html"
  (unless cpplint-executable
    (error "cpplint not found, define cpplint-executable"))

  (when (process-live-p cpplint--flymake-proc)
    (kill-process cpplint--flymake-proc))

  (let* ((source (current-buffer))
         (filename (buffer-file-name))
         (command (cpplint-create-command filename)))
    (save-restriction
      ;; Use whole buffer, save restrictions will revert this when done.
      (widen)
      (setq
       cpplint--flymake-proc
       (make-process
        :name "cpplint-flymake"
        :noquery t
        :connection-type 'pipe
        ;; Collect output in new temp buffer.
        :buffer (generate-new-buffer "*cpplint-flymake*")
        :command command
        :sentinel (lambda (proc _event)
                    ;; Check that the process has indeed exited, as it might
                    ;; be simply suspended.
                    ;;
                    (when (memq (process-status proc) '(exit signal))
                      (unwind-protect
                          ;; Only proceed if `proc' is the same as
                          ;; `cpplint--flymake-proc', which indicates that
                          ;; `proc' is not an obsolete process.
                          ;;
                          (if (with-current-buffer source (eq proc cpplint--flymake-proc))
                              (with-current-buffer (process-buffer proc)
                                (goto-char (point-min))
                                ;; Parse the output buffer for diagnostic's
                                ;; messages and locations, collect them in a list
                                ;; of objects, and call `report-fn'.
                                ;;
                                (cl-loop
                                 ;; while doesn't return nil
                                 while (search-forward-regexp "^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" nil t)
                                 ;; for is basically let* vars being set per iteration
                                 for file = (match-string 1)
                                 for linum = (string-to-number (match-string 2))
                                 for msg = (concat "cpplint:" (match-string 3))
                                 for (beg . end) = (flymake-diag-region source linum)
                                 ;; can use all for (let variables) for this final thing.
                                 collect (flymake-make-diagnostic source beg end :warning msg)
                                 ;; cons output of collect into diags for later use.
                                 into diags
                                 ;; Call with diags before it gets deallocated.
                                 finally (funcall report-fn diags)))
                            (flymake-log :warning "Canceling obsolete check %s" proc))
                        ;; Cleanup the temporary buffer used to hold the
                        ;; check's output.
                        ;;
                        (kill-buffer (process-buffer proc))
                        )))) ;; end of make-process
       ) ;; end of setq
      ) ;; end of save-restrction
    ) ;; end of let*
  ) ;; end of defun.

(defun cpplint-setup-flymake-backend ()
  (when (and (eglot-managed-p)
             (memq major-mode '(c-mode c++-mode c-ts-mode c++-ts-mode)))
    (message "cpplint added")
    (add-hook 'flymake-diagnostic-functions 'cpplint-flymake)))

(defun cpplint-flymake-after-init ()
  (when cpplint-executable
    (add-hook 'eglot-managed-mode-hook 'cpplint-setup-flymake-backend)))

(add-hook 'after-init-hook 'cpplint-flymake-after-init)

(provide 'init-prog-c)
