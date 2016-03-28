;;; OS specific parameters for FreeBSD.
(defvar *hyperspec-dir* "/usr/local/share/doc/clisp-hyperspec/HyperSpec/"
  "The Common Lisp ANSI-standard Hyperspec.")

(defvar *sbcl-home* "/usr/local/lib/sbcl/"
  "Home of the SBCL compiler.")

(defvar *sbcl-exec* "/usr/local/bin/sbcl"
  "Executable of the SBCL compiler.")

(defvar *browser-exec-name* "chrome"
  "Executable name of the default browser used.")
(provide 'os-params)
