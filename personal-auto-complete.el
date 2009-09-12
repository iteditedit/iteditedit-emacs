;;(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")

;; (require 'python)

;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;; Require
(require 'auto-complete)
(require 'auto-complete-yasnippet)
(require 'auto-complete-semantic)
(require 'auto-complete-python)
(require 'auto-complete-emacs-lisp)
(global-auto-complete-mode t)
;(ac-ropemacs-setup)

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

(setq-default ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-all-buffer))
