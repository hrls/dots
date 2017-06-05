;; suwayyah.el
(defun suwayyah-init ()
  (interactive)
  (message "suwayyah"))

;; M-x: aj: dir => cd obsolete; echo
;; obviously should work in eshell
(defun aj (dir)
  "jump into @dir from history"
  (interactive)
  ;; scan eshell lastdir file
  ;; make sorted index
  ;; grep first occurrence
  ;; if not found ask prompt
  ;; jump into..
  (error "not implemented"))
