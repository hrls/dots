(set-language-environment 'UTF-8)

(setq user-full-name "hrls")
(setq user-mail-address "viktor.kharitonovich@gmail.com")


;;; Init / Inhibit
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "hrls")

(setq auto-save-default nil)
(setq make-backup-files nil)


;;; GUI
(tool-bar-mode -1)
(fringe-mode '(12 . 0))
;; (setq cursor-type 'hbar
;;       custom-enabled-themes '(wombat))
(scroll-bar-mode -1)
(setq scroll-step 1
      scroll-margin 8
      scroll-conservatively 1001
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount  '(1 ((shift) . 0.1)))


;; (setq redisplay-dont-pause t) ; >_<

(setq use-dialog-box nil)
(defalias 'yes-or-no-p 'y-or-n-p)


;;; Text
(setq-default standard-indent 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq search-highlight t)
(setq query-replace-highlight t)

(setq kill-whole-line t)

(column-number-mode t)
(show-paren-mode t)
(delete-selection-mode t)

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;;; Buffers
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer)
;; (require 'bs)
;; (global-set-key (kbd "<f2>") 'bs-show)


(require 'dired)
(setq dired-find-subdir t)
(setq dired-recursive-deletes 'top)

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)
(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)


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

(global-set-key [?\t] 'omni-func)
;; (setq tab-always-indent 'complete) ; one day w/ CAPF support

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x i") 'ielm) ; TODO: in new selected frame
(define-key global-map "\C-x\C-k" 'kill-region) ;; elisp ko kmacro

(global-set-key (kbd "s-i") 'xref-find-definitions-other-window)
(define-key prog-mode-map [s-mouse-1] 'xref-find-definitions-at-mouse)
(define-key prog-mode-map [S-return]
  (lambda ()(interactive)
    (progn
      (beginning-of-line)
      (newline-and-indent)
      (previous-line)
      (indent-for-tab-command))))
;; (global-set-key (kbd "s-/") 'comment-or-uncomment-region)
