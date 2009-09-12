;; (setq semantic-load-turn-everything-on t)

;; (require 'semantic-load)

;; Load CEDET
(load-file "~/.emacs.d/plugins/cedet-1.0/common/cedet.elc")

(global-ede-mode 1)                      ; Enable the Project management system
;;(semantic-load-enable-minimum-features)

;; ;;make all the 'semantic.cache' files go somewhere sane
;; (setq semanticdb-default-save-directory "~/.emacs.d/semantic.cache/")

;; ;; * This enables even more coding tools such as the nascent intellisense mode
;; ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-guady-code-helpers)
;; (require 'semantic-ia)
;; (require 'semantic-gcc)

;; (semantic-add-system-include "/usr/lib/python2.5/" 'python-mode)

;; (defun my-cedet-hook ()
;;   (local-set-key [(control return)] 'semantic-ia-complete-symbol)
;;   (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
;;   (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
;;   (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))

