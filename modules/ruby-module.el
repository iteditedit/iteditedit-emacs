;; svn co http://svn.ruby-lang.org/repos/ruby/trunk/misc/ ~/.emacs.d/elisp/ruby-mode
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)

;; inf-ruby
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".erb$" . html-mode) auto-mode-alist))

(require 'ruby-electric)

(add-hook 'ruby-mode-hook '(lambda ()
                             (inf-ruby-keys)
                             (ruby-electric-mode t)
                             (set (make-local-variable 'indent-tabs-mode) 'nill)
                             (setq tab-width 2)
                             ))

;; rinari
;; http://github.com/eschulte/rinari
(require 'rinari)

;; we are going to debugging
;; svn checkout svn://rubyforge.org/var/svn/ruby-debug
(autoload 'rdebug "rdebug" "Ruby debugging support." t)

(defun rinari-generate-tags()
  (interactive)
  (let ((my-tags-file (concat (rinari-root) "TAGS"))
	(root (rinari-root)))
    (message "Regenerating TAGS file: %s" my-tags-file)
    (if (file-exists-p my-tags-file)
	(delete-file my-tags-file))
    (shell-command
     (format "find %s -regex \".+rb$\" | xargs ctags-exuberant -a -e -f %s"
	     root my-tags-file))
    (if (get-file-buffer my-tags-file)
	 (kill-buffer (get-file-buffer my-tags-file)))
    (visit-tags-table my-tags-file)))
;; (add-hook 'rinari-minor-mode-hook 'rinari-generate-tags)

(require 'ruby-block)
(ruby-block-mode t)

;; anything-rcodetools
;;(add-to-list 'load-path "~/.emacs.d/vendor/rcodetools")
;(require 'rcodetools)
;(require 'icicles-rcodetools)
;(require 'anything)
;;       ;; Command to get all RI entries.
(setq rct-get-all-methods-command "PAGER=cat fri -l -L")
;;(define-key anything-map "\C-z" 'anything-execute-persistent-action)

;; yasnippet rails
(load (concat config-dir "vendor/yasnippets-rails/setup.el"))
(provide 'ruby-module)
