;;; define-key-wise.el - Analogues for `define-key' and friends
;;; to understand "natural syntax"

;; Copyright (C) 1995  Ilya Zakharevich

;; Author: Ilya Zakharevich <ilya@math.mps.ohio-state.edu>

;; This file is not a part of GNU Emacs, but is made available under
;; the same conditions.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary ============================================================

;;; To use this package add 

;;; (require 'define-key-wise)

;;; to your .emacs file. After this you can define keys in a "natural"
;;; manner, i.e., can put multiple command in the definition, can put
;;; docstring, can put forms to evaluate, have direct assess to both
;;; numeric and raw argument. Just append -wise to the name of the
;;; command. Example:

;;; (global-set-key-wise [M-return]
;;;     "Revert the direction of undo."
;;;     '(setq last-command 'undo-toggle)    ; a hack.
;;;     'advertised-undo
;;;     '(message "Undo Toggle"))

;;; A helper function `point-to-mouse-maybe' is provided that makes it
;;; possible to bind a command both to a keyboard and mouse event.

;;; I think the behavior of key-binding commands should be changed on the
;;; level of C code, and what is here is just a test engine for such a change.

;;; Changes:

;; Does not advice definitions any more. It slowed down the loading a lot.

(defvar list)

(or (fboundp 'point-to-mouse-maybe)	; Is defsubst fboundp-ing?
(defsubst point-to-mouse-maybe ()
  "Moves point to the position of the click if the last event is click.
Intended for use in commands that can be bound both to mouse and keyboard."
  (and (listp last-input-event) (mouse-set-point last-input-event)))
)

(defun define-key-subst-args (name)
  "Used for `define-key-wise' and friends. NAME should be the
symbol name of the \"action\" argument of the adviced function."
  (let (doc (defn name))
    (if (or list
	    (and defn (listp defn) (symbolp (car defn))
		 (not (memq (car defn) '(lambda keymap))))) ; Extended syntax
	(progn
	  (if (and list (stringp defn))
	      (setq doc (list defn))	; Process docstring
	    (setq list (cons defn list))) ; Now list contains all the actions
	  (setq list
		(mapcar (lambda (elt) (if (symbolp elt)
					  (list 'call-interactively
						(list 'quote elt))
					elt))
			list))
	  (append '(lambda (n raw))
				    doc '((interactive "p\nP")) list))
      defn)))

(defun define-key-wise (keymap key def &rest list)
  "Variant of `define-key' that allows \"natural\" keybindings.
Allow also DEF to be a list to be evaled when key is pressed. Free
variables `n' and `raw' can be used as scratch. Initially they contain 
numerical and raw repeat counts. Also supports syntax 
\(define-key KEYMAP KEY DOC DEF1 DEF2 ...)
and
\(define-key KEYMAP KEY DEF1 DEF2 ...).
All the DEFi are evaluated, if lists, and called interactively, if symbols, 
when the key is pressed. DOC can contain a documentation string. 
Use \"(point-to-mouse-maybe)\" to move the point to the position of click."
  (define-key keymap key (define-key-subst-args def)))

(defun global-set-key-wise (keys function &rest list)
  "Variant of `global-set-key' that allows \"natural\" keybindings.
Allow also FUNCTION to be a list to be evaled when key is pressed. Free
variables `n' and `raw' can be used as scratch. Initially they contain 
numerical and raw repeat counts. Also supports syntax 
\(define-key KEYMAP KEY DOC DEF1 DEF2 ...)
and
\(define-key KEYMAP KEY DEF1 DEF2 ...).
All the DEFi are evaluated, if lists, and called interactively, if symbols, 
when the key is pressed. DOC can contain a documentation string. 
Use \"(point-to-mouse-maybe)\" to move the point to the position of click."
  (global-set-key keys (define-key-subst-args function)))

(defun local-set-key-wise (keys function &rest list)
  "Variant of `local-set-key' that allows \"natural\" keybindings.
Allow also FUNCTION to be a list to be evaled when key is pressed. Free
variables `n' and `raw' can be used as scratch. Initially they contain 
numerical and raw repeat counts. Also supports syntax 
\(define-key KEYMAP KEY DOC DEF1 DEF2 ...)
and
\(define-key KEYMAP KEY DEF1 DEF2 ...).
All the DEFi are evaluated, if lists, and called interactively, if symbols, 
when the key is pressed. DOC can contain a documentation string. 
Use \"(point-to-mouse-maybe)\" to move the point to the position of click."
  (local-set-key keys (define-key-subst-args function)))

(provide 'define-key-wise)
