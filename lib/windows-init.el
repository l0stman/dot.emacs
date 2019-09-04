;;; windows-init --- OS specific parameters for Windows.
;;; Commentary:
;;; All the parameters and code specific to Windows should be defined here.
;;; Code:
(defvar *hyperspec-dir*
  (concat (file-name-as-directory (getenv "HOME"))
          (file-name-as-directory "HyperSpec-7-0/HyperSpec"))
  "The Common Lisp ANSI-standard Hyperspec.")

(provide 'windows-init)
;;; windows-init ends here
