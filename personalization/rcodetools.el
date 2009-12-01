(require 'anything-rcodetools)
(require 'auto-complete-ruby)
(add-hook 'ruby-mode-hook
  (lambda ()
    (setq ac-omni-completion-sources '(("\\.\\=" ac-source-rcodetools)))))

