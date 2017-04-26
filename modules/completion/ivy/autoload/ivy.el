;;; completion/ivy/autoload/ivy.el

;;;###autoload
(defun +ivy/counsel-ag-occur ()
  "Invoke the search+replace wgrep buffer on the current ag search results."
  (interactive)
  (require 'wgrep)
  (call-interactively 'ivy-occur))
