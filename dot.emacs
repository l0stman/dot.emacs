;;;; -*-Emacs-Lisp-*-

(defvar *emacs-dir* (expand-file-name "~/.emacs.d")
  "Emacs personal root directory.")

(defsubst full-path (file)
  "Expand the FILE name into the root directory."
  (expand-file-name file *emacs-dir*))

(defsubst add-to-list* (lst &rest args)
  "Add to the list LST the elements in ARGS."
  (dolist (fn args) (add-to-list lst fn)))

(add-to-list* 'load-path
	      (full-path "lib")
	      (expand-file-name "~/hacks/slime"))

;;; OS specific parameters.
(pcase system-type
  ('gnu/linux (require 'ubuntu-init))
  ('berkeley-unix (require 'freebsd-init))
  ('windows-nt (require 'windows-init)))

(require 'utils)                   ; some utility macros and functions

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
  "H-v" scroll-other-window-down

  ;; Super
  "s-k" kmacro-keymap

  ;; Misc
  "C-m" reindent-then-newline-and-indent
  "C-w" backward-kill-word
  "C-x C-k" kill-region
  "C-x g" magit-status
  "C-x a" org-agenda
  "C-h" backward-delete-char-untabify
  "C-c k" join-line
  "C-c j" join-next-line
  "C-c f" find-library
  "C-c c" compose-french)

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)

;;;; Custom theme
(load-theme 'dream t)

;;; Configure package.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; Company.
(use-package company
  :config
  (require 'color)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip
       ((t (:inherit default :background "#121733"))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
  (global-company-mode)
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("M-." . company-show-location)))

;;; Ido.
(require 'ido)
(ido-mode t)

;;;; SLIME configuration for Common Lisp.
(defvar *sbcl-core* (full-path "sbcl.core-with-swank"))
(defvar *sbcl-dev-core* (full-path "sbcl-devel.core-with-swank"))
(defvar *fasls-dir* (concat temporary-file-directory "fasls-dir"))

(make-directory *fasls-dir* t)

(defsubst sbcl-implementations ()
  "Return the list of SBCL implementations."
  (unless (eq system-type 'windows-nt)
    `((sbcl (,*sbcl-exec* "--noinform" "--core" ,*sbcl-core*)
            :init (lambda (port-file enc)
                    (format "(swank:start-server %S)\n" port-file))
            :env ((concat "SBCL_HOME=" *sbcl-home*)))
      (sbcl-devel (,(expand-file-name "~/bin/sbcl") "--noinform" "--core"
                   ,*sbcl-dev-core*)
                  :init (lambda (port-file enc)
                          (format "(swank:start-server %S)\n" port-file))
                  :env (,(concat "SBCL_HOME="
                                 (expand-file-name "~/lib/sbcl")))))))

(use-package slime-company)
(use-package slime
  :hook (slime-mode . (lambda ()
                        ;; Make SLIME connect to lisp when opening a lisp file.
	                (unless (slime-connected-p)
	                  (save-excursion (slime)))
                        (common-lisp-set-style 'modern)))
  
  :bind (:map slime-mode-map
              ("C-c M-i" . slime-inspect-definition)
              :map slime-repl-mode-map
              ("C-c s" . slime-repl-next-matching-input)
	      ("C-c r" . slime-repl-previous-matching-input)
              ("C-c M-i" . slime-inspect-definition))
  :init (require 'slime-autoloads)
  :config
  (slime-setup '(slime-repl slime-autodoc slime-fuzzy slime-fancy-inspector
                            slime-indentation slime-presentations
                            slime-fancy slime-company))
  (let ((slime-dir (file-name-directory (locate-library "slime"))))
    (add-to-list* 'load-path
                  slime-dir
                  (expand-file-name "contrib" slime-dir)))
  (setq slime-net-coding-system        'utf-8-unix
        common-lisp-hyperspec-root     *hyperspec-dir*
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-compile-file-options     `(:fasl-directory ,*fasls-dir*)
        slime-lisp-implementations
        `(,@(sbcl-implementations)
          (ccl (,*ccl-exec* "--terminal-encoding" "utf-8")))))

;;; Flycheck.
(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (unless (eq system-type 'windows-nt)
    (use-package flycheck-clang-analyzer
      :config
      (flycheck-clang-analyzer-setup))))

;;;; For editing scheme code.
(use-package quack)

;;;; Paredit
(use-package paredit
  :commands enable-paredit-mode
  :init (require 'lisp-indent)
  :bind (:map paredit-mode-map
         ("H-)" . paredit-lisp-indent-assign)))

(defvar *paredit-mode-list*
  '(lisp scheme emacs-lisp lisp-interaction slime-repl inferior-scheme)
  "List of major modes using paredit.")

;;; Toggle paredit mode and bind <tab> to symbol completion.
(dolist (name *paredit-mode-list*)
  (add-hook (symb name '-mode-hook)
	    `(lambda ()
	       (enable-paredit-mode)
               (defkeys ,(symb name '-mode-map) "C-h" paredit-backward-delete)
               ,(let ((map (symb name '-mode-map)))
                  (cl-case name
                    ((emacs-lisp lisp-interaction)
                     `(defkeys ,map "<tab>" completion-at-point))
                    (lisp
                     `(defkeys ,map "<tab>" slime-complete-symbol)))))))

;;; Yaml
(use-package yaml-mode
  :mode (("\\.yaml$" . yaml-mode) ("\\.yml$" . yaml-mode)))

;;;; Adding some hooks.
;;; Make it easy to navigate by expression for programming mode
;;; and turn on flyspell.
(defvar *prog-mode-list*
  '(paredit c c++ sh awk mixal octave inferior-octave python inferior-python
            java js2)
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
		 "C-<backspace>" backward-kill-sexp)
               (flyspell-prog-mode))))

;;; Text mode
(add-hook 'text-mode-hook
          '(lambda ()
             (refill-mode 1)
             (flyspell-mode 1)))

;;; Latex and HTML mode
(dolist (name '(latex html))
  (add-hook (symb name '-mode-hook)
            '(lambda ()
               (refill-mode -1)
               (auto-fill-mode 1))))

;;; Org mode
(setq org-startup-indented               t
      org-bullets-bullet-list            '(" ")
      org-ellipsis                       "⤵"
      org-pretty-entities                t
      org-hide-emphasis-markers          t
      org-agenda-block-separator         ""
      org-fontify-whole-heading-line     t
      org-fontify-done-headline          t
      org-fontify-quote-and-verse-blocks t)



(let ((bg-white           "#fbf8ef")
      (bg-dark            "#1c1e1f")
      (fg-white           "#ffffff")
      (dark-cyan          "#008b8b")
      (dark-red           "#8c3400")
      (slate              "#8FA1B3")
      (keyword            "#f92672")
      (comment            "#525254")
      (builtin            "#fd971f")
      (doc                "#727280")
      (gray               "#bbb")
      (et-font            "EtBembo")
      (sans-mono-font     "Souce Code Pro")
      (serif-mono-font    "Verily Serif Mono"))
  (add-hook 'org-mode-hook
            `(lambda ()
               (refill-mode -1)
               (auto-fill-mode 1)
               (variable-pitch-mode 1)
               (setq org-todo-keyword-faces
                     `(("⮞" . org-warning)
                       ("✔" . (:foreground ,,dark-cyan :weight bold))
                       ("✘" . (:foreground ,,dark-red :weight bold))
                       ("⏲" . (:foreground "steel blue":weight bold))))))
  (custom-theme-set-faces
   'user
   `(variable-pitch
     ((t (:family ,et-font
                  :background nil
                  :height 1.1))))
   `(org-document-title
     ((t (:inherit nil
                   :family ,et-font
                   :height 1.4
                   :foreground ,gray
                   :underline nil))))
   `(org-document-info
     ((t (:height 0.9
                  :slant italic))))
   `(org-level-1
     ((t (:inherit nil
                   :family ,et-font
                   :height 1.2
                   :weight normal
                   :slant normal
                   :foreground ,keyword))))
   `(org-level-2
     ((t (:inherit nil
                   :family ,et-font
                   :weight normal
                   :height 1.1
                   :slant italic
                   :foreground ,gray))))
   `(org-level-3
     ((t (:inherit nil
                   :family ,et-font
                   :weight normal
                   :slant italic
                   :height 1.0
                   :foreground ,gray))))
   `(org-level-4
     ((t (:inherit nil
                   :family ,et-font
                   :weight normal
                   :slant italic
                   :height 1.0
                   :foreground ,gray))))
   `(org-level-5
     ((t nil)))
   `(org-level-6
     ((t nil)))
   `(org-level-7
     ((t nil)))
   `(org-level-8
     ((t nil)))
   `(org-headline-done
     ((t (:family ,et-font
                  :strike-through t))))
   `(org-quote
     ((t nil)))
   `(org-block
     ((t (:background ,bg-dark))))
   `(org-block-begin-line
     ((t (:background nil
                      :height 0.8
                      :family ,sans-mono-font
                      :foreground ,slate))))
   `(org-block-end-line
     ((t (:background nil
                      :height 0.8
                      :family ,sans-mono-font
                      :foreground ,slate))))
   `(org-document-info-keyword
     ((t (:height 0.8
                  :foreground ,comment))))
   `(org-link
     ((t (:underline t
                     :weight normal
                     :foreground ,slate))))
   `(org-special-keyword
     ((t (:family ,sans-mono-font
                  :height 0.8))))
   `(org-todo
     ((t (:foreground ,builtin
                      :background ,bg-dark))))
   `(org-done
     ((t (:inherit variable-pitch
                   :foreground ,dark-cyan
                   :background ,bg-dark))))
   `(org-agenda-current-time
     ((t (:foreground ,slate))))
   `(org-hide
     ((t (:foreground ,bg-white))))
   `(org-indent
     ((t (:inherit (org-hide fixed-pitch)))))
   `(org-time-grid
     ((t (:foreground ,comment))))
   `(org-warning
     ((t (:foreground ,builtin))))
   `(org-date
     ((t (:family ,sans-mono-font
                  :height 0.8))))
   `(org-agenda-structure
     ((t (:height 1.3
                  :foreground ,doc
                  :weight normal
                  :inherit variable-pitch))))
   `(org-agenda-date
     ((t (:inherit variable-pitch
                   :height 1.1))))
   `(org-agenda-date-today
     ((t (:height 1.5
                  :foreground ,keyword
                  :inherit variable-pitch))))
   `(org-agenda-date-weekend
     ((t (:inherit org-agenda-date))))
   `(org-scheduled
     ((t (:foreground ,gray))))
   `(org-upcoming-deadline
     ((t (:foreground ,keyword))))
   `(org-scheduled-today
     ((t (:foreground ,fg-white))))
   `(org-scheduled-previously
     ((t (:foreground ,slate))))
   `(org-agenda-done
     ((t (:inherit nil
                   :strike-through t
                   :foreground ,doc))))
   `(org-ellipsis
     ((t (:underline nil
                     :foreground ,comment))))
   `(org-tag
     ((t (:foreground ,doc))))
   `(org-table
     ((t (:family ,serif-mono-font
                  :height 0.9
                  :background ,bg-white))))
   `(org-code
     ((t (:inherit nil
                   :family ,serif-mono-font
                   :foreground ,comment
                   :height 0.9))))))

(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; CC mode
(autoload 'plunder-mode "plunder"
  "Minor mode for structurally editing C code."
  t)

(add-hook 'c-initialization-hook
          '(lambda ()
             (defkeys c-mode-base-map
               "C-m" c-context-line-break
               "C-c RET" c-macro-expand)))

(defun new-c-snug-do-while (syntax pos)
  "This function is a modified version of `c-snug-do-while' that
works with macros."
  (save-excursion
    (if (and (eq syntax 'block-close)
             (progn (backward-up-list)
                    (c-forward-sexp -1)
                    (looking-at "\\<do\\>[^_]")))
        '(before)
      '(before after))))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "bsd")
             (c-toggle-auto-newline)
             (plunder-mode +1)
             (add-to-list* 'c-cleanup-list
                           'comment-close-slash
                           'brace-else-brace
                           'brace-elseif-brace)
             (add-to-alist c-hanging-braces-alist
                           (class-open after)
                           (class-close before)
                           (brace-list-close before)
                           (block-close . new-c-snug-do-while))))

;;; C++ mode.
(add-hook 'c++-mode-hook
          '(lambda ()
             (add-to-alist c-offsets-alist (innamespace . 0))))

;;; PHP mode.
(c-add-style "php"
             `("linux" (c-basic-offset . 4)))
(add-hook 'php-mode-hook
          '(lambda ()
             (c-set-style "php")
             (setq flycheck-phpcs-standard "PSR2")
             (c-set-offset 'arglist-intro '+)
             (c-set-offset 'arglist-close '0)))

;;; Interaction Lisp mode.
(add-hook 'lisp-interaction-mode-hook
	  '(lambda ()
	     (defkeys lisp-interaction-mode-map "C-m" eval-print-last-sexp)))

;;; MIXAL mode
(require 'mixvm)

(add-hook 'mixal-mode-hook
          '(lambda ()
             (defkeys mixal-mode-map
               "C-h" backward-delete-char-untabify
               "C-m" newline-and-indent
               "C-j" newline
               "H-h o" mixal-describe-operation-code)))

;;; awk mode
(add-hook 'awk-mode-hook
          '(lambda ()
             (add-to-alist c-hanging-braces-alist
                           (defun-open after))))

;;; Octave mode
(use-package octave
  :mode ("\\.m$" . octave-mode)
  :bind (:map octave-mode-map ("<tab>" . octave-complete-symbol)))

;;; CSS mode.
(use-package rainbow-mode
  :hook css-mode)

;;; Javascript mode.
(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . electric-pair-mode))

;;; Python mode.
(use-package python
  :hook (python-mode . (lambda ()
                         (add-to-list 'company-backends
                                      '(company-jedi company-files))
                         (py-autopep8-enable-on-save)
                         (jedi:setup)
                         (electric-pair-mode +1)
                         (save-selected-window
                           (switch-to-buffer-other-window
                            (process-buffer
                             (python-shell-get-or-create-process
                              (python-shell-parse-command)))))))
  :init
  (use-package py-autopep8
    :config (setq py-autopep8-options '("-aa")))
  (use-package company-jedi
    :config
    (setq jedi:setup-keys      t
          jedi:complete-on-dot t))
  (use-package pyvenv
    :init
    (add-hook 'pyvenv-post-activate-hooks #'pyvenv-restart-python)))

;;; Web mode
(use-package web-mode
  :mode ("\\.twig\\'" "\\.jinja\\'")
  :hook (web-mode . (lambda ()
                      (setq web-mode-markup-indent-offset 2
                            web-mode-code-indent-offset   2))))

;;; Objective-J mode.
(require 'objj-mode)

;;; Go mode.
(use-package company-go
  :config (setq gofmt-command "goimports")
  :hook ((before-save . gofmt-before-save)
         (go-mode . (lambda ()
                      (electric-pair-mode)
                      (flyspell-prog-mode))))
  :bind (:map go-mode-map
         ("C-t" . transpose-sexps)
         ("C-M-t" . transpose-chars)
         ("C-f" . forward-sexp)
         ("C-M-f" . forward-char)
         ("C-b" . backward-sexp)
         ("C-M-b" . backward-char)
         ("C-k" . kill-sexp)
         ("C-M-k" . paredit-kill)
         ("C-<backspace>" . backward-kill-sexp)))

;;; Magit.
(use-package magit)

;;; SSH agent.
(use-package ssh-agency)

;;;; Misc
(when window-system
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program *browser-exec-name*)
  ;; start an emacs server
  (server-start))

(setq-default indent-tabs-mode         nil ; use spaces only for indentation
              show-trailing-whitespace t)

(setq default-major-mode     'text-mode
      custom-file            (full-path "emacs-custom.el")
      backup-by-copying      t
      backup-directory-alist `((".*" . ,temporary-file-directory)))

(set-default-coding-systems 'utf-8)
(load custom-file)

(require 'keywiz)		    ; game to learn emacs key-bindings

