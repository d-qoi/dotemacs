;;; constants.el --- -*- lexical-binding: t -*-


(defconst *sys/gui*
  (display-graphic-p)
  "Are we running on a GUI Emacs?")

(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/root*
  (string-equal "root" (getenv "USER"))
  "Are you a ROOT user?")

(defconst *rg*
  (executable-find "rg")
  "Do we have ripgrep?")

(defconst *ag*
  (executable-find "ag")
  "Do we have the silver searcher")

(defconst *python*
  (executable-find "python")
  "Do we have python?")

(defconst *python3*
  (executable-find "python3")
  "Do we have python3?")

(defconst *cscope*
  (executable-find "cscope")
  "Do we have cscope")

(defconst *global*
  (executable-find "gtags")
  "Do we have Gnu Globals")

(defconst *ccls*
  (executable-find "ccls")
  "Do we have ccls")

(defconst *clangd*
  (or (executable-find "clangd")  ;; usually
      (executable-find "/usr/local/opt/llvm/bin/clangd"))  ;; macOS
  "Do we have clangd?")

(defconst *gcc*
  (executable-find "gcc")
  "Do we have gcc?")

(defconst *clang*
  (executable-find "clang")
  "Do we have clang")

(defconst *cppcheck*
  (executable-find "cppcheck")
  "Do we have cppcheck")

(defconst *cflow*
  (executable-find "cflow")
  "Do we have cflow")

(defconst *git*
  (executable-find "git")
  "Do we have git?")

(defconst *pdflatex*
  (executable-find "pdflatex")
  "Do we have pdflatex?")

(defconst *eaf-env*
  (and *sys/linux* *sys/gui* *python3*
       (executable-find "pip")
       (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") "")))
  "Check basic requirements for EAF to run.")

(defconst *python-use-lsp* t
  "Do we use LSP or Anaconda")

(provide 'constants)
