;;; freebsd-init --- OS specific init file for FreeBSD.
;;; Commentary:
;;; All the parameters and code specific to FreeBSD should be defined here.
;;; Code:
(defvar *hyperspec-dir* "/usr/local/share/doc/clisp-hyperspec/HyperSpec/"
  "The Common Lisp ANSI-standard Hyperspec.")

(defvar *sbcl-home* "/usr/local/lib/sbcl/"
  "Home of the SBCL compiler.")

(defvar *sbcl-exec* "/usr/local/bin/sbcl"
  "Executable of the SBCL compiler.")

(defvar *browser-exec-name* "chrome"
  "Executable name of the default browser used.")

(defvar *tmp-dir* (getenv "TMPDIR")
  "Directory for temporary files.")

(provide 'freebsd-init)
;;; freebsd-init ends here
