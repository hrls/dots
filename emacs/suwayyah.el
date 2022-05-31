;;; Funcs

(defun set-in-alist (list-var values)
  (if-let (entry (assoc (car values) (symbol-value list-var)))
      (setcdr entry (cdr values))
    (set list-var (cons values (symbol-value list-var))))) ;; TODO: bind list once

;;; Arrows

(defun hrls/ctrl-a-move-beginning-of (arg)
  (interactive "^d")
  (back-to-indentation)
  (if (= arg (point))
      (move-beginning-of-line nil)))

(defun hrls/ctrl-e-move-end-of (arg)
  (interactive "^d")
  (move-end-of-line nil))
  ;; (if (= arg (point))
  ;;     (forward-paragraph)))

;;; Daggers

(defun hrls/ctrl-w-kill ()
  (interactive)
  (call-interactively (if (use-region-p) #'kill-region #'backward-kill-word)))

(defun hrls/indent-and-return ()
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command))

(provide 'suwayyah)
