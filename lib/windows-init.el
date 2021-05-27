;;; windows-init --- OS specific parameters for Windows.
;;; Commentary:
;;; All the parameters and code specific to Windows should be defined here.
;;; Code:
(defvar *hyperspec-dir*
  (concat "file://"
          (file-name-as-directory (getenv "HOME"))
          (file-name-as-directory "hyperspec/HyperSpec"))
  "The Common Lisp ANSI-standard Hyperspec.")

(defvar *browser-exec-name* "C:/Program Files/Mozilla Firefox/firefox.exe"
  "Executable name of the default browser used.")

(defvar *ccl-exec* (concat (file-name-as-directory (getenv "HOME"))
                           (file-name-as-directory "bin/ccl")
                           "wx86cl64")
  "Executable of the CCL compiler.")

(defvar *ocaml-merlin* "c:/OCaml64/home/pc/.opam/4.12.0+mingw64c/bin/ocamlmerlin.exe"
  "The path to merlin.")

(provide 'windows-init)
;;; windows-init ends here
