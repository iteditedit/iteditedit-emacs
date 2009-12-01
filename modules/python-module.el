;; Insert Python debug statements
(defun set-py-breakpoint (&optional no-other-breakpoints)
  "Sets the necessary breakpoint py code."
;;  (interactive)
  (cond
   (no-other-breakpoints
    (insert "if not globals().get( 'PDB_ACTIVE', 0 ):
       globals()['PDB_ACTIVE'] = 1
       import pdb; pdb.set_trace()")
    (indent-relative))
   (t
    (insert "import pdb; pdb.set_trace()"))
  )
)

(provide 'python-module)
