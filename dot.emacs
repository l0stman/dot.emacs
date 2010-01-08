;;;; -*-Emacs-Lisp-*-

(defvar *emacs-dir* "/home/l0stman/.emacs.d"
  "Emacs personal root directory.")

(add-to-list 'load-path (concat *emacs-dir* "/lib"))

(defun symb (&rest args)
  "Produce a symbol from the lisp objects."
  (intern
   (with-output-to-string
     (dolist (o args) (princ o)))))

;;;; Key bindings
(global-set-key "\C-m" 'reindent-then-newline-and-indent)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-h" 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key [(hyper h)] 'help-command)

;;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)

;;;; Color theme
(load-library "color-theme-djcb-dark")

(eval-after-load "color-theme-djcb-dark"
  (progn
    (color-theme-initialize)
    (color-theme-djcb-dark)))

;;;; SLIME configuration
(defvar *slime-dir* "/usr/local/share/emacs/23.0.95/site-lisp/slime")

;;; Common Lisp
(defvar *sbcl-core* (concat *emacs-dir* "/sbcl.core-with-swank"))

(setq slime-net-coding-system 'utf-8-unix
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

(defvar *paredit-mode-list*
  '(lisp scheme emacs-lisp lisp-interaction slime-repl)
  "List of major mode using paredit.")

(dolist (name *paredit-mode-list*)
  (add-hook (symb name '-mode-hook) (lambda () (paredit-mode +1))))
