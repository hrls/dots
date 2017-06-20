;; C-a should jump as M-m for the first time, next as origin C-a
(global-set-key "\C-x\C-b" 'bs-show)	; todo: bs-show show any top level dirs
;; todo C-x b -> just pre buffer like alt-tab
;; todo C-x B -> ido-switch-buffer
; (global-set-key "\C-i" 'hippie-expand)
(global-set-key (kbd "<C-return>") 'new-next-line)
(global-set-key (kbd "<S-return>") 'new-prev-line)
(global-set-key (kbd "s-w") 'kill-this-buffer) ; todo: move point focus to previous selected buffer

;; dired hook bind
;; S-return dired-find-file-other-window

;; bind M-1 to M-! (shell-command ...)

(global-set-key (kbd "C-;") 'comment-line)

;; todo: scan major mode
(global-set-key (kbd "<f9>") 'eval-buffer)

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
