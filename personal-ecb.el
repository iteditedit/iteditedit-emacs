
(add-to-list 'load-path "~/.emacs.d/plugins/ecb-snap")

(setq ecb-tip-of-the-day nil)
(require 'ecb-autoloads)
(ecb-activate)
(ecb-toggle-ecb-windows)
;; Filter unwanted source file
(setq ecb-source-file-regexps (quote ((".*" ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(pyc\\|elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\)$\\)\\)") ("^\\.\\(emacs\\|gnus\\)$")))))
;; Set the left-click mouse button to collapse directory in directory buffer
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1)) 