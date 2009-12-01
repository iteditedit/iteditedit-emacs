
(if (fboundp #'which-func-mode)
    (add-hook 'semantic-init-hooks (lambda ()
 				     (which-func-mode 1))))
(global-semantic-show-parser-state-mode 1)

(setq semanticdb-default-system-save-directory
      (setq semanticdb-default-save-directory "~/.emacs.d/semantic.cache/"))
;;(setq semanticdb-default-save-directory "~/.emacs.d/semantic.cache/")
(semantic-add-system-include "/usr/lib/python2.5" 'python-mode)

(require 'semantic-gcc)
(require 'semantic-ia)
(require 'eassist)

;; (semantic-load-enable-guady-code-helpers)
;; (semantic-load-enable-minimum-features)
(defun semantic-custom-load-helper ()
  (interactive)
  (semantic-load-enable-minimum-features)
  (condition-case nil
      (global-semantic-idle-completions-mode 1)
    (error nil))

  (global-semantic-highlight-func-mode 1)
  (global-semantic-idle-summary-mode 1)

  (global-semantic-mru-bookmark-mode 1)

  (semantic-load-enable-code-helpers)
)
(semantic-custom-load-helper)
;;(require 'semanticdb-global)
;;(semanticdb-enable-gnu-global-databases 'c-mode)
;;(semanticdb-enable-gnu-global-databases 'c++-mode)

(semantic-load-enable-all-exuberent-ctags-support)
(semanticdb-enable-exuberent-ctags 'c-mode)
(semanticdb-enable-exuberent-ctags 'c++-mode)

(semantic-add-system-include "/usr/include/" 'c-mode)
(semantic-add-system-include
 "/usr/lib/gcc/i486-linux-gnu/4.3.3/include"
 'c-mode)
(semantic-add-system-include "/usr/include/" 'c++-mode)
(semantic-add-system-include
 "/usr/lib/gcc/i486-linux-gnu/4.3.3/include/"
 'c++-mode)
(semantic-add-system-include
 "/usr/lib/gcc/i486-linux-gnu/4.3.3/include/"
 'c++-mode)


(add-hook 'speedbar-load-hook
	  (lambda ()
	    (require 'semantic-sb)))

(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c." 'senator-complete-symbol)
  
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit))
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'lisp-mode-hook 'my-cedet-hook)

(defun my-c-mode-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-cm" 'eassist-list-methods))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

(defun my-semantic-hook ()
  (semantic-tag-folding-mode 1))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(when (require 'ede nil t)
  (global-ede-mode t))

(proivde 'semantic-module)
