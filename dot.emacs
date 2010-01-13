;;;; -*-Emacs-Lisp-*-

(defvar *emacs-dir* "/home/l0stman/.emacs.d"
  "Emacs personal root directory.")

(add-to-list 'load-path *emacs-dir*)
(add-to-list 'load-path (concat *emacs-dir* "/lib"))

(require 'cl)				; Common Lisp library

(defmacro defkeys (map &rest bindings)
  "Define the bindings represented as property list of keys and
functions in the key mapping map (the global one if null)."
  (eval-after-load "cl"
    '`(progn
	,@(loop for plist on bindings by #'cddr
		collect (let ((key (car plist)) (fn (cadr plist)))
			  (if map
			      `(define-key ,map (kbd ,key) ',fn)
			    `(global-set-key (kbd ,key) ',fn)))))))

(defun join-next-line ()
  "Join the current line with the next one."
  (interactive)
  (join-line 1))

;;;; Global key bindings
(defkeys nil
  ;; Function keys
  "<f1>" info
  "<f2>" help-command
  "<f3>" compile
  "<f4>" repeat-complex-command
  "<f5>" slime-selector
  
  ;; Meta
  "M-g" goto-line
  "M-n" next-error
  "M-p" previous-error

  ;; Hyper
  "H-h" help-command
  "H-s" slime-selector
  "H-r" query-replace

  ;; Misc
  "C-m" reindent-then-newline-and-indent
  "C-w" backward-kill-word
  "C-x C-k" kill-region
  "C-h" backward-delete-char-untabify
  "C-c k" join-line
  "C-c j" join-next-line)

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)

;;;; Color theme
(load-library "color-theme-djcb-dark")

(eval-after-load "color-theme-djcb-dark"
  '(progn
     (color-theme-initialize)
     (color-theme-djcb-dark)))

;;;; SLIME configuration for Common Lisp
(defvar *slime-dir* "/usr/local/share/emacs/23.0.95/site-lisp/slime")
(defvar *sbcl-core* (concat *emacs-dir* "/sbcl.core-with-swank"))
(defvar *hyperspec-dir* "/usr/local/share/doc/clisp-hyperspec/HyperSpec/")

(setq slime-net-coding-system 'utf-8-unix
      common-lisp-hyperspec-root *hyperspec-dir*
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-lisp-implementations
      `((sbcl ("sbcl" "--core" ,*sbcl-core*)
	      :init (lambda (port-file _)
		      (format "(swank:start-server %S)\n" port-file)))))

(add-to-list 'load-path *slime-dir* (concat *slime-dir* "/contrib"))
(require 'slime-autoloads)
(slime-setup '(slime-repl slime-autodoc slime-fuzzy))

;;;; For editing scheme code
(require 'quack)

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
  '(lisp scheme emacs-lisp lisp-interaction slime-repl inferior-scheme)
  "List of major modes using paredit.")

;;; Toggle paredit mode and bind <tab> to symbol completion.
(dolist (name *paredit-mode-list*)
  (add-hook (symb name '-mode-hook) 
	    `(lambda ()
	       (paredit-mode +1)
	       (define-key ,(symb name '-mode-map) (kbd "<tab>")
		 (case ',name
		   ((emacs-lisp lisp-interaction) 'lisp-complete-symbol)
		   (lisp 'slime-complete-symbol)
		   (slime-repl 'slime-indent-and-complete-symbol))))))

;;;; Adding some hooks.
(setq-default indent-tabs-mode nil)	; Use spaces only for indentation.

;;; Make it easy to navigate by expression for programming mode.
(defvar *prog-mode-list* '(paredit c sh awk)
  "List of programming mode names.")

(dolist (name *prog-mode-list*)
  (add-hook (symb name '-mode-hook)
	    `(lambda ()
	       (defkeys ,(symb name '-mode-map)
		 "C-t" transpose-sexps
		 "C-M-t" transpose-chars
		 "C-f" paredit-forward
		 "C-M-f" forward-char
		 "C-b" paredit-backward
		 "C-M-b" backward-char
		 "C-k" kill-sexp
		 "C-M-k" paredit-kill
		 "C-<backspace>" backward-kill-sexp))))

;;; Text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; C mode
(add-hook 'c-mode-hook
	  '(lambda ()
	     (c-set-style "bsd")
	     (c-toggle-auto-newline)
             (define-key c-mode-map "\C-m" 'c-context-line-break)))

;;; Interaction Lisp mode
(add-hook 'lisp-interaction-mode-hook
	  '(lambda ()
	     (defkeys lisp-interaction-mode-map "C-m" eval-print-last-sexp)))

;;; Make SLIME connect to lisp when opening a lisp file
(add-hook 'slime-mode-hook
	  '(lambda ()
	     (unless (slime-connected-p)
	       (save-excursion (slime)))))

;;; SLIME repl mode
(add-hook 'slime-repl-mode-hook
	  '(lambda ()
	     (defkeys slime-repl-mode-map
	       "C-c s" slime-repl-next-matching-input
	       "C-c r" slime-repl-previous-matching-input)))

;;;; Misc 
(when (eq window-system 'x)
  (setq browse-url-browser-function 'browse-url-firefox
	browse-url-firefox-program "firefox3")
  ;; start an emacs server
  (server-start))

(setq show-trailing-whitespace t
      custom-file (concat *emacs-dir* "/emacs-custom.el"))
(load custom-file)

(require 'keywiz)		    ; game to learn emacs key-bindings