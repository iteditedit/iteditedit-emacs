(add-to-list 'load-path "~/.emacs.d/plugins/icicles")
;;; Require
(require 'icicles)

;;; Code:


;;; ### Icicles ###
;;; --- Minibuffer的智能补全
(setq icicle-top-level-key-bindings nil) ;禁止icicles的按键生效
(setq icicle-key-complete-keys nil)      ;禁止icicles的补全按键加载
(setq icicle-download-dir "~/.emacs.d/plugins/icicles/") ;设置icicles的下载目录, 运行 'icicle-download-wizard' 即可更新
(setq icicle-incremental-completion-flag 'always)   ;; always show completion buffer..
(setq icicle-highlight-input-completion-failure-delay 0.3)         ;输入补全失败延迟高亮
(setq icicle-incremental-completion-delay 0.3)                     ;增量补全延迟
(setq icicle-default-value nil)                                    ;不显示默认的值
(setq icicle-highlight-lighter-flag nil)                           ;不显示 Icicles 标志
(setq icicle-unpropertize-completion-result-flag t)                ;解决Gnus附件产生文本属性的bug
(setq icicle-redefine-standard-commands-flag nil)                  ;不要重新定义标准按键
; turn off stupid icicles line stuff
(let ((maps (list 
             minibuffer-local-map minibuffer-local-ns-map minibuffer-local-isearch-map
             completion-list-mode-map minibuffer-local-completion-map minibuffer-local-must-match-map)))
  (mapcar '(lambda (map) (progn
                           (define-key map [(control ?a)] 'move-beginning-of-line)
                           (define-key map [(control ?e)] 'move-end-of-line)))
          maps))

(icicle-mode 1) 