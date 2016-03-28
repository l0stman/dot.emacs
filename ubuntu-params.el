;;; OS specific parameters for Ubuntu.
(defvar *hyperspec-dir* "/usr/share/doc/hyperspec/"
  "The Common Lisp ANSI-standard Hyperspec.")

(defvar *sbcl-home* "/usr/lib/sbcl/"
  "Home of the SBCL compiler.")

(defvar *sbcl-exec* "/usr/bin/sbcl"
  "Executable of the SBCL compiler.")

(defvar *browser-exec-name* "chromium-browser"
  "Executable name of the default browser used.")
(provide 'os-params)
