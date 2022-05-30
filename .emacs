(set-language-environment 'UTF-8)

(setq user-full-name "hrls")
(setq user-mail-address "viktor.kharitonovich@gmail.com")


;;; Init / Inhibit
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "hrls")

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "autosaves"))))


;;; GUI
;; (setq redisplay-dont-pause t) ; >_<
(tool-bar-mode -1)
(fringe-mode '(0 . 13))
(set-default 'cursor-type 'hbar)
;; (setq cursor-type 'hbar
;;       custom-enabled-themes '(wombat))
(scroll-bar-mode -1)
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 1001
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount  '(1 ((shift) . 0.1)))


(setq use-dialog-box nil)
(defalias 'yes-or-no-p 'y-or-n-p)


;;; Text
(setq-default standard-indent 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq search-highlight t
      query-replace-highlight t)

(setq kill-whole-line t)

(column-number-mode t)
(show-paren-mode t)
(delete-selection-mode t)


(global-auto-revert-mode 1)

(require 'saveplace)
(setq-default save-place t)


;;; prog-mode
(defun prog-mode-tweaks ()
  ;; TODO:
  ;; [ ] - highlight variable/tokens under cursor (C-8/C-*)
  ;; [ ] - C-w (backward-kill-word) respects current-prog-lang-mode delimiters
  ;; [ ] - C-a more like vi-like [I]nsert, first press go to first visible char, second '^'

  (setq display-line-numbers-width 3
        show-trailing-whitespace t)
  ;; TODO: hide or dim on inactive frames or windows
  (display-line-numbers-mode))

(add-hook 'prog-mode-hook 'prog-mode-tweaks)

;;; Buffers
(require 'ibuffer)
;; TODO: Filename/Process column: prefix [project] for projectile buffers
(defalias 'list-buffers 'ibuffer)
(require 'uniquify)
(setq uniquify-separator "/"
      uniquify-buffer-name-style 'forward)
;; (require 'bs)
;; (global-set-key (kbd "<f2>") 'bs-show)


;;; Dired
(require 'dired)
(setq dired-find-subdir t)
(setq dired-recursive-deletes 'top)

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)
(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)


;;; Windows
(windmove-default-keybindings) ; Shift+Arrows when get bored w/ C-x o


;; ~/.config/emacs/custom.el or XDG
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file) (load custom-file))


(require 'server)
(unless (server-running-p) (server-start))


(add-to-list 'load-path "~/src/dots/emacs")
(require 'suwayyah)
(require 'wm)
(require 'my-packages)
(require 'omnitab)
(require 'fallback)

(global-set-key [?\t] #'omni-func)
;; (setq tab-always-indent 'complete) ; one day w/ CAPF support

(global-set-key (kbd "s-e") #'forward-sentence)
(global-set-key (kbd "s-a") #'backward-sentence)
(global-set-key (kbd "s-x") #'execute-extended-command)

(global-set-key "\C-a" #'hrls/ctrl-a-move-beginning-of)
(global-set-key "\C-e" #'hrls/ctrl-e-move-end-of)
(global-set-key "\C-w" #'hrls/ctrl-w-kill)

(global-set-key (kbd "C-8") #'highlight-symbol-at-point)
(global-set-key (kbd "C-*") #'unhighlight-regexp)
(global-set-key (kbd "s-/") #'comment-or-uncomment-region)
(global-set-key (kbd "C-x i") #'ielm) ; TODO: in new selected frame

(global-set-key (kbd "s-i") #'xref-find-definitions-other-window)

(define-key prog-mode-map [s-mouse-1] #'xref-find-definitions-at-mouse)
(define-key prog-mode-map [C-return] #'hrls/indent-and-return)

(put 'upcase-region 'disabled nil)
