;; https://www.emacswiki.org/emacs/TabCompletion#SmartTab

;; indent if line is empty (indent-according-to-mode)
;; (complete-at-point-function) if line is already indented
(defun omni-func ()
  (interactive)

  (cond
   ((use-region-p) (indent-for-tab-command))
   (t
    (if (looking-at "\\>") ; end of symbol
        (when (company-manual-begin)
          (if (= company-candidates-length 1)
              (company-complete-number 1)))
      (indent-for-tab-command)))))

(provide 'omnitab)
