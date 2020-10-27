(defun omni-func ()
  (interactive)

  (cond
   ((use-region-p) (indent-for-tab-command))
   (t
    ;; TODO:
    ;; try indent if line is empty (indent-according-to-mode)
    ;; then => complete-at-point-function
    (when (company-manual-begin)
      (if (= company-candidates-length 1)
          (company-complete-number 1))))))

(defun company-instant-or-cycle ()
  (interactive)
  (if (and (company-tooltip-visible-p)
           (= company-candidates-length 1))
      (company-complete-number 1)
    ;; TODO: endless route '' 'Foo' 'Bar' 'Foo' ..., w/o empty lines
    (company-select-next)))


(provide 'omnitab)
