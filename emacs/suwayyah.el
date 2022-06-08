;;; Funcs

(defun set-in-alist (list-var values)
  (if-let (entry (assoc (car values) (symbol-value list-var)))
      (setcdr entry (cdr values))
    (set list-var (cons values (symbol-value list-var))))) ;; TODO: bind list once

;; DWIM addons

(defun my/ctrl-a-move-beginning-of (_arg)
  (interactive "^d")
  (back-to-indentation)
  (if (= _arg (point))
      (move-beginning-of-line nil)))

(defun my/ctrl-e-move-end-of (_arg)
  (interactive "^d")
  (move-end-of-line nil))
  ;; (if (= _arg (point))
  ;;     (forward-paragraph)))

(defun my/ctrl-w-kill ()
  (interactive)
  (call-interactively
   (if (use-region-p) #'kill-region #'backward-kill-word)))


(defun my/indent-and-return ()
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command))


(defun my/highlight (_rbegin _rend)
  "Highlight active region / symbol at point"
  ;; TODO: rotate colors for phrases and for symbols (currently each one has own stack)
  (interactive "r")
  (if (use-region-p)
      (let ((_region (buffer-substring-no-properties _rbegin _rend)))
        (highlight-phrase (rx (literal _region)))
        (deactivate-mark))
    (call-interactively #'highlight-symbol-at-point)))

(defun my/unhighlight ()
  "Unhighlight regexp (instant when only one is active)"
  (interactive)
  ;; TODO: no prompt if there is only one lighter
  ;; (read-answer "prompt : "
  ;;              '(("1" ?1 "first")
  ;;                ("2" ?2 "second")
  ;;                ("*" ?* "all")))
  (call-interactively #'unhighlight-regexp))


(defun my/ctrl-t (_rbegin _rend)
  (interactive "r")
  (if (use-region-p)
      (message "region is active")
    (message "region isn't active"))
  (message "begin %s" _rbegin)
  (message "end %s" _rend))



;; *-mode: navigate vi-like, edit emacs way
;;; Any mode C-g when edit active evil mode or mapped to current mode vi-like movements or keymap

(provide 'suwayyah)
