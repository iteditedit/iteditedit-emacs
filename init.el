 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Richard Steckroth Emacs Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library Paths
;; Note: I like to keep every emacs library underneath
;;   ~/.emacs.d and 3rd party apps under ~/.emacs.d/plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d")
;Add all top-level subdirectories of .emacs.d to the load path
(progn (cd "~/.emacs.d")
       (normal-top-level-add-subdirs-to-load-path))
;Add all ./plugins sub dirs
(add-to-list 'load-path "~/.emacs.d/plugins")
(progn (cd "~/.emacs.d/plugins")
       (normal-top-level-add-subdirs-to-load-path))

(setenv "PYTHONPATH" "/home/lunatic/.emacs.d/plugins/Pymacs/:/home/lunatic/.emacs.d/plugins/gpycomplete/:/home/lunatic/.emacs.d/plugins/pycomplete/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load default packages that won't interfere
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'winring)
(require 'pabbrev)

;; Minibuffer or Icicles support
;;(load-file "~/.emacs.d/plugins/minibuffer-complete.el")
(load-library "personal-icicles")


(load-library "personal-pabbrev")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle theme colors and personal interface options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-interface")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Ad-HOC or completely misc options.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-behavior")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load all my personal keybindings first to give non 3rd party
;; packages precedence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-keybindings")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET Plugin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-cedet")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SemanticDB Parser and Senator
;; Pythonic includes and system cmds are put with personal-python.
;; Currently includes C/C++ for completness
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-semantic")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Code Browsers -- Awsome... C-b Toggles display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-ecb.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet integration. Must be loaded before auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-yasnippet")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; International Spelling Plugin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-ispell")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocompletion library - Rope, Pymacs, Yasnippet, Autocomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-auto-complete")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python Autocompletion, syntax checking, etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-python")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ Related autocompletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-c")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FlyMake latest. Includes syntax checking and minibuffer echo area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-flymake")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal Switching - Keybindings are F12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-terminal")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Superier Lisp Interpretor Evaluator and Debugger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "personal-slime")

;; Ispell -- Currently uses /usr/bin/aspell
(setq-default ispell-program-name "aspell")

;; Fullscreen on startup
(toggle-fullscreen)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-fix-window-size (quote width))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("/home/lunatic/.emacs.d" "config") ("/home/lunatic" "home") ("/home/lunatic/Projects" "projects"))))
 '(ecb-windows-width 0.15)
 '(icicle-change-region-background-flag t)
 '(icicle-default-value t)
 '(icicle-hide-common-match-in-Completions-flag t)
 '(icicle-populate-interactive-history-flag t)
 '(icicle-region-background "#566566")
 '(icicle-search-highlight-context-levels-flag nil)
 '(use-dialog-box nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-analyse-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face :background "lightblue" :foreground "black" :weight bold))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "lightblue" :foreground "black" :weight bold))))
 '(ecb-directory-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face :background "lightblue" :foreground "black" :weight bold))))
 '(ecb-history-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face :background "lightblue" :foreground "black" :weight bold))))
 '(ecb-tag-header-face ((((class color) (background dark)) (:background "SeaGreen1" :foreground "black"))))
 '(icicle-search-context-level-1 ((((background dark)) (:background "#FA6CC847FFFF" :foreground "black"))))
 '(icicle-search-context-level-2 ((((background dark)) (:background "#C847FFFFE423" :foreground "black"))))
 '(icicle-search-context-level-3 ((((background dark)) (:background "#C847D8FEFFFF" :foreground "black"))))
 '(icicle-search-context-level-4 ((((background dark)) (:background "#EF46FFFFC847" :foreground "black"))))
 '(icicle-search-context-level-5 ((((background dark)) (:background "#FCFCE1E1FFFF" :foreground "black"))))
 '(icicle-search-context-level-6 ((((background dark)) (:background "#E1E1FFFFF0F0" :foreground "black"))))
 '(icicle-search-context-level-7 ((((background dark)) (:background "#E1E1EAEAFFFF" :foreground "black"))))
 '(icicle-search-context-level-8 ((((background dark)) (:background "#F6F5FFFFE1E1" :foreground "black")))))
