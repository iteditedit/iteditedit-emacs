(require 'yasnippet)


;; Initialize Yasnippet                                                                                        
;Don't map TAB to yasnippet                                                                                    
;In fact, set it to something we'll never use because                                                          
;we'll only ever trigger it indirectly.                                                                        
(setq yas/trigger-key (kbd "C-c C-u"))
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
