;;; init-eshell.el --- -*- lexical-binding: t -*-

;;; A lot of this will be copied from the wizard Howard Abrams
;;; https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org#last-results

(use-package eshell
  :straight (:type built-in)
  :after esh-mode
  :hook (eshell-load . #'eat-eshell-visual-command-mode)
  :init
  (setq eshell-error-if-no-glob t
        ;; This jumps back to the prompt:
        eshell-scroll-to-bottom-on-input 'all
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t

        ;; Since eshell starts fast, let's dismiss it on exit:
        eshell-kill-on-exit t
        eshell-destroy-buffer-when-process-dies t

        ;; Can you remember the parameter differences between the
        ;; executables `chmod' and `find' and their Emacs counterpart?
        ;; Me neither, so this makes it act a bit more shell-like:
        eshell-prefer-lisp-functions nil)
  :bind (:map eshell-mode-map
         ("M-R" . eshell-insert-history))
  :config
  (defvar eshell-variable-aliases-list nil "Autoloading this eshell-defined variable")
  (add-to-list 'eshell-variable-aliases-list '("$"  ha-eshell-output-text))
  (add-to-list 'eshell-variable-aliases-list '("_"  ha-eshell-output-list))
  (add-to-list 'eshell-variable-aliases-list '("OUTPUT" ha-eshell-output-file)))

;;; Useful packages
(use-package pcmpl-args
  :straight t
  :after eshell)

(use-package pcre2el
  :straight (:host github :repo "joddie/pcre2el")
  :config
  (defmacro prx (&rest expressions)
    "Convert the rx-compatible regular EXPRESSIONS to PCRE.
  Most shell applications accept Perl Compatible Regular Expressions."
    `(rx-let ((integer (1+ digit))
              (float   (seq integer "." integer))
              (b256    (seq (optional (or "1" "2"))
                            (regexp "[0-9]\\{1,2\\}")))
              (ipaddr  (seq b256 "." b256 "." b256 "." b256))
              (time    (seq digit (optional digit) ":" (= 2 digit) (optional ":" (= 2 digit))))
              (email   (seq (1+ (regexp "[^,< ]")) "@" (1+ (seq (1+ (any alnum "-"))) ".") (1+ alnum)))
              (date    (seq (= 2 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 4 digit)))
              (ymd     (seq (= 4 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 2 digit)))
              (uuid    (seq (= 8 hex) "-" (= 3 (seq (= 4 hex) "-")) (= 12 hex)))
              (guid    (seq uuid)))
       (rxt-elisp-to-pcre (rx ,@expressions)))))

;;; let's make opts work
(defun eshell-getopts (defargs args)
  "Return hash table of ARGS parsed against DEFARGS.
Where DEFARGS is an argument definition, a list of plists.
For instance:
   '((:name number :short \"n\"                 :parameter integer :default 0)
     (:name title  :short \"t\" :long \"title\" :parameter string)
     (:name debug  :short \"d\" :long \"debug\"))

If ARGS, a list of _command line parameters_ is something like:

    '(\"-d\" \"-n\" \"4\" \"--title\" \"How are that\" \"this\" \"is\" \"extra\")

The hashtable return would contain these entries:

    debug t
    number 4  ; as a number
    title \"How are that\" ; as a string
    parameters (\"this\" \"is\" \"extra\") ; as a list of strings "
  (let ((retmap    (make-hash-table))
        (short-arg (rx string-start "-" (group alnum)))
        (long-arg  (rx string-start "--" (group (1+ any)))))

    ;; Let's not pollute the Emacs name space with tiny functions, as
    ;; well as we want these functions to have access to the "somewhat
    ;; global variables", `retmap' and `defargs', we use the magical
    ;; `cl-labels' macro to define small functions:

    (cl-labels ((match-short (str defarg)
                  ;; Return t if STR matches against DEFARG's short label:
                  (and (string-match short-arg str)
                       (string= (match-string 1 str)
                                (plist-get defarg :short))))

                (match-long (str defarg)
                  ;; Return t if STR matches against DEFARG's long label:
                  (and (string-match long-arg str)
                       (string= (match-string 1 str)
                                (plist-get defarg :long))))

                (match-arg (str defarg)
                  ;; Return DEFARG if STR matches its definition (and it's a string):
                  (when (and (stringp str)
                             (or (match-short str defarg)
                                 (match-long str defarg)))
                    defarg))

                (find-argdef (str)
                  ;; Return entry in DEFARGS that matches STR:
                  (car (--filter (match-arg str it) defargs)))

                (process-args (arg parm rest)
                  (when arg
                    (let* ((defarg (find-argdef arg))
                           (key    (plist-get defarg :name)))
                      (cond
                       ;; If ARG doesn't match any definition, add
                       ;; everything else to PARAMETERS key:
                       ((null defarg)
                        (puthash 'parameters (cons arg rest) retmap))

                       ((plist-get defarg :help)
                        (error (documentation (plist-get defarg :help))))

                       ;; If argument definition has a integer parameter,
                       ;; convert next entry as a number and process rest:
                       ((eq (plist-get defarg :parameter) 'integer)
                        (puthash key (string-to-number parm) retmap)
                        (process-args (cadr rest) (caddr rest) (cddr rest)))

                       ;; If argument definition has a parameter, use
                       ;; the next entry as the value and process rest:
                       ((plist-get defarg :parameter)
                        (puthash key parm retmap)
                        (process-args (cadr rest) (caddr rest) (cddr rest)))

                       ;; No parameter? Store true for its key:
                       (t
                        (puthash key t retmap)
                        (process-args (car rest) (cadr rest) (cdr rest))))))))

      (process-args (car args) (cadr args) (cdr args))
      retmap)))

;;; Don't let things pause terminal output
(setenv "PAGER" "cat")

;;; Better history, will bind later
(defun eshell-insert-history ()
  "Displays the eshell history to select and insert back into your eshell."
  (interactive)
  (insert (completing-read "Eshell history: "
                               (delete-dups
                                (ring-elements eshell-history-ring)))))


;; supporting function for better mapping
(defun eshell-fn-on-files (fun1 fun2 args)
  "Call FUN1 on the first element in list, ARGS.
Call FUN2 on all the rest of the elements in ARGS."
  (unless (null args)
    (let ((filenames (flatten-list args)))
      (funcall fun1 (car filenames))
      (when (cdr filenames)
        (mapcar fun2 (cdr filenames))))
    ;; Return an empty string, as the return value from `fun1'
    ;; probably isn't helpful to display in the `eshell' window.
    ""))


(defun eshell/set (&rest args)
  "Creates a buffer local variables."
  (dolist (arg-pair (seq-partition args 2))
    (seq-let (var val) arg-pair
      (let ((var-sym (make-symbol var)))
        (set (make-local-variable var-sym) val)))))


(defun eshell/e (&rest files)
  "Essentially an alias to the `find-file' function."
  (eshell-fn-on-files 'find-file 'find-file-other-window files))

(defun eshell/ee (&rest files)
  "Edit one or more files in another window."
  (eshell-fn-on-files 'find-file-other-window 'find-file-other-window files))

(defun eshell/less (&rest files)
  "Essentially an alias to the `view-file' function."
  (eshell-fn-on-files 'view-file 'view-file-other-window files))

;;; Ha, imagine calling these...
(defalias 'eshell/emacs 'eshell/e)
(defalias 'eshell/vi 'eshell/e)
(defalias 'eshell/vim 'eshell/e)
(defalias 'eshell/more 'eshell/less)
(defalias 'eshell/view 'eshell/less)


;;; Ebb and Flow

(defvar ha-eshell-ebbflow-buffername "*eshell-edit*"
  "The name of the buffer that eshell can use to store temporary input/output.")

(defun ha-eshell-ebbflow-return ()
  "Close the ebb-flow window and return to Eshell session."
  (interactive)
  (when (boundp 'ha-eshell-ebbflow-close-window)
    (bury-buffer))
  (when (boundp 'ha-eshell-ebbflow-return-buffer)
    (pop-to-buffer ha-eshell-ebbflow-return-buffer)))

(define-minor-mode ebbflow-mode
  "Get your foos in the right places."
  :lighter " ebb"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-q") 'ha-eshell-ebbflow-return)
            (define-key map (kbd "C-c C-c") 'ha-eshell-ebbflow-return)
            map))

(defun eshell-flow-buffer-contents (buffer-name)
  "Return the contents of BUFFER as a string."
  (when buffer-name
    (save-window-excursion
      (switch-to-buffer (get-buffer buffer-name))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun eshell/flow (&rest args)
  "Output the contents of one or more buffers as a string.
Usage: flow [OPTION] [BUFFER ...]
    -h, --help           show this usage screen
    -l, --lines          output contents as a list of lines
    -w, --words          output contents as a list of space-separated elements "
  (let* ((options (eshell-getopts '((:name words  :short "w" :long "words")
                                    (:name lines  :short "l" :long "lines")
                                    (:name string :short "s" :long "string")
                                    (:name help   :short "h" :long "help"
                                           :help eshell/flow))
                                  args))
         (buffers (gethash 'parameters options))
         (content (thread-last buffers
                               (-map 'eshell-flow-buffer-contents)
                               (s-join "\n"))))
    (if (gethash 'help options)
        (error (documentation 'eshell/flow))

      ;; No buffer specified? Use the default buffer's contents:
      (unless buffers
        (setq content
              (eshell-flow-buffer-contents ha-eshell-ebbflow-buffername)))

      ;; Do we need to convert the output to lines or split on words?
      (cond
       ((gethash 'words options) (split-string content))
       ((gethash 'lines options) (split-string content "\n"))
       (t                        content)))))

(defun eshell-flow-buffers (buffers)
  "Convert the list, BUFFERS, to actual buffers if given buffer names."
  (if buffers
      (--map (cond
              ((bufferp it) it)
              ((stringp it) (get-buffer it))
              (t            (error (format "Illegal argument of type %s: %s\n%s"
                                           (type-of arg) it
                                           (documentation 'eshell/flow)))))
             buffers)
    ;; No buffers given? Use the default buffer:
    (list (get-buffer ha-eshell-ebbflow-buffername))))


(defalias 'eshell/bcat 'eshell/flow)

(defun ha-eshell-ebb-switch-to-buffer (insert-location)
  "Switch to `ha-eshell-ebbflow-buffername' and get the buffer ready for new data."
  (let ((return-buffer (current-buffer)))

    (if-let ((ebbwindow (get-buffer-window ha-eshell-ebbflow-buffername)))
        (select-window ebbwindow)
      (switch-to-buffer ha-eshell-ebbflow-buffername)
      (setq-local ha-eshell-ebbflow-close-window t))

    (setq-local ha-eshell-ebbflow-return-buffer return-buffer)
    (ebbflow-mode)

    (cl-case insert-location
      (:append  (goto-char (point-max)))
      (:prepend (goto-char (point-min)))
      (:insert   nil)
      (:replace (delete-region (point-min) (point-max))))))

(defun ha-eshell-ebb-command (insert-location command-parts)
  "Call `eshell-command' with the COMMAND-PARTS.
Inserts the output into `ha-eshell-ebbflow-buffername'"
  (let ((command-string (string-join command-parts " ")))
    (ha-eshell-ebb-switch-to-buffer insert-location)
    (eshell-command command-string t)))

(defun ha-eshell-ebb-files (insert-location files)
  "Insert the FILES at the INSERT-LOCATION tin `ha-eshell-ebbflow-buffername'."
  (ha-eshell-ebb-switch-to-buffer insert-location)
  (dolist (file files)
    (insert-file file)
    (insert "\n")))

(defun ha-eshell-ebb-output (insert-location)
  "Grab output from previous eshell command, inserting it into our buffer.
Gives the INSERT-LOCATION to `ha-eshell-ebb-switch-to-buffer'."
  (let* ((start  (save-excursion
                   (goto-char eshell-last-output-start)
                   (re-search-backward eshell-prompt-regexp)
                   (next-line)
                   (line-beginning-position)))
         (end    eshell-last-output-start)
         (contents (buffer-substring-no-properties start end)))
    (ha-eshell-ebb-switch-to-buffer insert-location)
    (insert contents)))

(defun eshell/ebb (&rest args)
  "Run command with output into a buffer, or output of last command.
Usage: ebb [OPTION] [COMMAND] [FILE ...]
    -h, --help           show this usage screen
    -a, --append         add command output to the *eshell-edit* buffer
    -p, --prepend        add command output to the end of *eshell-edit* buffer
    -i, --insert         add command output to *eshell-edit* at point"
  (let* ((options  (eshell-getopts '((:name insert  :short "i" :long "insert")
                                     (:name append  :short "a" :long "append")
                                     (:name prepend :short "p" :long "prepend")
                                     (:name help    :short "h" :long "help"
                                            :help eshell/ebb))
                                   args))
         (location (cond
                    ((gethash 'insert  options) :insert)
                    ((gethash 'append  options) :append)
                    ((gethash 'prepend options) :prepend)
                    (t                          :replace)))
         (params   (gethash 'parameters options)))
    (cond
     ((seq-empty-p params)         (ha-eshell-ebb-output  location))
     ((file-exists-p (car params)) (ha-eshell-ebb-files   location params))
     (t                            (ha-eshell-ebb-command location params))))

  ;; At this point, we are in the `ha-eshell-ebbflow-buffername', and
  ;; the buffer contains the inserted data, so:
  (goto-char (point-min))

  nil) ; Return `nil' so that it doesn't print anything in `eshell'.


;;; The fun $$ tricks and hacks
(defvar ha-eshell-output (make-ring 10)
  "A ring (looped list) storing history of eshell command output.")

(defun ha-eshell-store-last-output ()
  "Store the output from the last eshell command.
Called after every command by connecting to the `eshell-post-command-hook'."
  (let ((output
         (buffer-substring-no-properties eshell-last-input-end eshell-last-output-start)))
    (ring-insert ha-eshell-output output)))

(add-hook 'eshell-post-command-hook 'ha-eshell-store-last-output)

(defun eshell/output (&rest args)
  "Return an eshell command output from its history.

The first argument is the index into the historical past, where
`0' is the most recent, `1' is the next oldest, etc.

The second argument represents the returned output:
 * `text' :: as a string
 * `list' :: as a list of elements separated by whitespace
 * `file' :: as a filename that contains the output

If the first argument is not a number, it assumes the format
to be `:text'.
"
  (let (frmt element)
    (cond
     ((> (length args) 1)  (setq frmt (cadr args)
                                 element (car args)))
     ((= (length args) 0)  (setq frmt "text"
                                 element 0))
     ((numberp (car args)) (setq frmt "text"
                                 element (car args)))
     ((= (length args) 1)  (setq frmt (car args)
                                 element 0)))

    (if-let ((results (ring-ref ha-eshell-output (or element 0))))
        (cl-case (string-to-char frmt)
          (?l     (split-string results))
          (?f     (ha-eshell-store-file-output results))
          (otherwise (s-trim results)))
      "")))

(defun ha-eshell-store-file-output (results)
  "Writes the string, RESULTS, to a temporary file and returns that file name."
  (let ((filename (make-temp-file "ha-eshell-")))
    (with-temp-file filename
      (insert results))
    filename))

(defun ha-eshell-output (format-type indices)
  "Wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
  (if indices
      (eshell/output (string-to-number (caar indices)) format-type)
    (eshell/output 0 format-type)))

(defun ha-eshell-output-text (&optional indices &rest ignored)
  "A _text_ wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
  (ha-eshell-output "text" indices))

(defun ha-eshell-output-list (&optional indices &rest ignored)
  "A _list_ wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
  (ha-eshell-output "list" indices))

(defun ha-eshell-output-file (&optional indices &rest ignored)
  "A _file_ wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
  (ha-eshell-output "file" indices))


(provide 'init-eshell)
