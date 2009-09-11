;;F12 switch to ansi-term
(defun switch-to-ansi-term ()
  "If ansi-term is not existed, start a new one, otherwise switch
to it"
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term "/bin/bash")))
