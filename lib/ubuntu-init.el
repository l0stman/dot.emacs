;;; ubuntu-init --- OS specific parameters for Ubuntu.
;;; Commentary:
;;; All the parameters and code specific to Ubuntu should be defined here.
;;; Code:
(defvar *hyperspec-dir* "/usr/share/doc/hyperspec/"
  "The Common Lisp ANSI-standard Hyperspec.")

(defvar *sbcl-home* "/usr/lib/sbcl/"
  "Home of the SBCL compiler.")

(defvar *sbcl-exec* "/usr/bin/sbcl"
  "Executable of the SBCL compiler.")

(defvar *browser-exec-name* "firefox"
  "Executable name of the default browser used.")

(defvar *ccl-exec* "ccl"
  "Executable of the CCL compiler.")

(require 'python)
(setq python-shell-interpreter "python3")

(provide 'ubuntu-init)
;;; ubuntu-init ends here
