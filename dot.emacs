;; -*-Emacs-Lisp-*-

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-h" 'backward-delete-char)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key [(hyper h)] 'help-command)

(add-to-list 'load-path "/home/l0stman/.emacs.d")

(load-library "color-theme-djcb-dark")

(eval-after-load "color-theme-djcb-dark"
  (progn
    (color-theme-initialize)
    (color-theme-djcb-dark)))