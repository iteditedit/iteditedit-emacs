;; This file assumes auto-complete and cedet installed
(require 'gpycomplete)
;(require 'pycomplete)
;;(require 'pysmell)


;;(require 'personal-semantic)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
;; (semantic-add-system-include "/usr/lib/python2.5" 'python-mode)
;; (semantic-add-system-include "/usr/lib/python2.5/" 'python-mode)
(set-variable 'py-python-command "/usr/bin/python2.5")
;; (setq semantic-python-dependency-system-include-path
;;       '("/usr/lib/python2.5/"))

(add-hook 'python-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")
            (gpycomplete-mode)
            ;; (add-to-list 'ac-omni-completion-sources
            ;;              (cons "\\." '(ac-source-semantic)))
            ;; (my-cedet-hook)  
            
       ;;     (pysmell-mode 1)
            ))


;; Not provided by flymake itself, curiously
(defun flymake-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-temp-")))

;;Auto Syntax Error Hightlight
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
            'flymake-create-temp-in-system-tempdir))
    (local-file (file-relative-name
         temp-file
         (file-name-directory buffer-file-name))))
         (list "pyflakes" (list local-file))
      ))
  (add-to-list 'flymake-allowed-file-name-masks
        '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)
