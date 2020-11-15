(unless (package-installed-p 'counsel)
  ;; (use-package ido :quelpa)
  (require 'ido)
  (ido-mode 1)
  (setq ido-everywhere t
        ido-enable-flex-matching t
        ido-confirm-unique-completion nil
        ido-use-virtual-buffers t
        ido-create-new-buffer 'always
        ido-completion-buffer "*Ido Completions*"
        ido-completion-buffer-all-completions nil
        ido-max-window-height 1
        ido-use-faces t
        ido-buffer-disable-smart-matches nil
        ido-enable-last-directory-history t
        ido-enable-tramp-completion t)

  (global-set-key (kbd "s-b") #'ido-switch-buffer)
  (global-set-key (kbd "s-B") #'ido-switch-buffer-other-frame)
  (global-set-key (kbd "s-f") #'ido-find-file)
  (global-set-key (kbd "s-F") #'ido-find-file-other-frame)
  (global-set-key (kbd "s-d") #'ido-dired)
  (global-set-key (kbd "s-D") #'ido-dired-other-frame))


;; Kind of inspiration
(defun tron-theme ()
  (interactive)
  (use-package tron-legacy-theme
    :config
    (setq tron-legacy-theme-vivid-cursor t
          tron-legacy-theme-dark-fg-bright-comments t
          tron-legacy-theme-softer-bg nil)
    (load-theme 'tron-legacy t)))

(defun flatland-theme ()
  (use-package flatland-theme
    :config
    (custom-theme-set-faces
     'flatland
     '(show-paren-match ((t (:background "dark gray" :foreground "black" :weight bold))))
     '(show-paren-mismatch ((t (:background "firebrick" :foreground "orange" :weight bold)))))
    (load-theme 'flatland t)))

(provide 'fallback)
