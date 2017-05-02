(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\C-i" 'hippie-expand)
(global-set-key (kbd "<C-return>") 'new-next-line)
(global-set-key (kbd "<S-return>") 'new-prev-line)

(defun new-next-line ()
  "vim: o"
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-for-tab-command))

(defun new-prev-line ()
  "vim: O"
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1)
  (indent-for-tab-command))
