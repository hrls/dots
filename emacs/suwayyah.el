(defun set-in-alist (list-var values)
  (if-let (entry (assoc (car values) (symbol-value list-var)))
      (setcdr entry (cdr values))
    (set list-var (cons values (symbol-value list-var))))) ;; TODO: bind list once

(provide 'suwayyah)
