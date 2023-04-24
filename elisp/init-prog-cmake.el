;; -*- lexical-binding -*-

(defconst *cmake*
  (executable-find "cmake")
  "Do we have cmake?")

(defvar *cmake-mode-path*
  (and *cmake* (car (file-expand-wildcards "/usr/share/emacs/site-lisp/*/cmake*")))
  "The path to cmake mode, if cmake is installed.")

(if (and *cmake* *cmake-mode-path*)
    (progn
      (push *cmake-mode-path* load-path)
      (require 'cmake-mode)))

(if (and *cmake* (not *cmake-mode-path*))
    (warn "Cmake found, cmake-mode-path not"))

(provide 'init-prog-cmake)
