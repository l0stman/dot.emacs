;;;; -*-Emacs-Lisp-*-

(defvar *emacs-dir* "/home/l0stman/.emacs.d"
  "Emacs personal root directory.")

(add-to-list 'load-path (concat *emacs-dir* "/lib"))

(require 'cl)	; Common Lisp library

(defmacro defkeys (map &rest bindings)
  "Define the bindings represented as property list of a key and the
corresponding function in the key mapping (the global one if null)."
  (eval-after-load "cl"
    '`(progn
	,@(loop for plist on bindings by #'cddr
		collect (let ((key (car plist)) (fn (cadr plist)))
			  (if map
			      `(define-key ,map (kbd ,key) ',fn)
			    `(global-set-key (kbd ,key) ',fn)))))))

;;;; Global key bindings
(defkeys nil
  "C-m" reindent-then-newline-and-indent
  "C-w" backward-kill-word
  "C-x C-k" kill-region
  "C-h" backward-delete-char-untabify
  "H-h" help-command
  "C-c s" slime-selector)

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)

;;;; Color theme
(load-library "color-theme-djcb-dark")

(eval-after-load "color-theme-djcb-dark"
  '(progn
     (color-theme-initialize)
     (color-theme-djcb-dark)))

;;;; SLIME configuration
(defvar *slime-dir* "/usr/local/share/emacs/23.0.95/site-lisp/slime")

;;; Common Lisp
(defvar *sbcl-core* (concat *emacs-dir* "/sbcl.core-with-swank"))
(defvar *hyperspec-dir* "/usr/local/share/doc/clisp-hyperspec/HyperSpec/")

(setq slime-net-coding-system 'utf-8-unix
      common-lisp-hyperspec-root *hyperspec-dir*
      slime-lisp-implementations
      `((sbcl ("sbcl" "--core" ,*sbcl-core*)
	      :init (lambda (port-file _)
		      (format "(swank:start-server %S)\n" port-file)))))

(add-to-list 'load-path *slime-dir* (concat *slime-dir* "/contrib"))
(require 'slime-autoloads)
(slime-setup '(slime-repl))

;;;; Paredit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(defun symb (&rest args)
  "Produce a symbol from the lisp objects."
  (intern
   (with-output-to-string
     (dolist (o args) (princ o)))))

(defvar *paredit-mode-list*
  '(lisp scheme emacs-lisp lisp-interaction slime-repl)
  "List of major modes using paredit.")

(dolist (name *paredit-mode-list*)
  (add-hook (symb name '-mode-hook) '(lambda () (paredit-mode +1))))

(eval-after-load "paredit"
  '(defkeys paredit-mode-map
     "C-t" transpose-sexps
     "C-M-t" transpose-chars
     "C-f" paredit-forward
     "C-M-f" forward-char
     "C-b" paredit-backward
     "C-M-b" backward-char
     "C-k" kill-sexp
     "C-M-k" paredit-kill
     "C-<backspace>" backward-kill-sexp))

;;;; Misc
(when (eq window-system 'x)
  (setq browse-url-browser-function 'browse-url-firefox
	browse-url-firefox-program "firefox3"))