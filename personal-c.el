(add-hook 'c-mode-common-hook '(lambda ()
      ;; ac-omni-completion-sources is made buffer local so
      ;; you need to add it to a mode hook to activate on 
      ;; whatever buffer you want to use it with.  This
      ;; example uses C mode (as you probably surmised).
      ;; auto-complete.el expects ac-omni-completion-sources to be
      ;; a list of cons cells where each cell's car is a regex
      ;; that describes the syntactical bits you want AutoComplete
      ;; to be aware of. The cdr of each cell is the source that will
      ;; supply the completion data.  The following tells autocomplete
      ;; to begin completion when you type in a . or a ->
      (add-to-list 'ac-omni-completion-sources
                   (cons "\\." '(ac-source-semantic)))
      (add-to-list 'ac-omni-completion-sources
                   (cons "->" '(ac-source-semantic)))
      ;; ac-sources was also made buffer local in new versions of
      ;; autocomplete.  In my case, I want AutoComplete to use 
      ;; semantic and yasnippet (order matters, if reversed snippets
      ;; will appear before semantic tag completions).
      (setq ac-sources '(ac-source-semantic ac-source-yasnippet))
      (setq system-base "/usr/include")
      (semantic-add-system-include system-base 'c-mode)
      (add-to-list 'auto-mode-alist (cons system-base 'c-mode))
      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat system-base "/mysql/mysql.h"))
      (my-cedet-hook)
      ))