(set-language-environment 'UTF-8)

(setq user-full-name "hrls")

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
;;; TODO: make window scrollable by touchpad on horizontal axis
(set-default 'cursor-type 'hbar)
(set-face-font 'variable-pitch "Georgia:size=15") ;; Helvetica Neue:size=15 Verdana:size=14
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode '(0 . 13))
;; (setq redisplay-dont-pause t) ; >_<
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 1001
      mouse-wheel-progressive-speed nil
      mouse-wheel-flip-direction t
      mouse-wheel-tilt-scroll t
      ;; mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount  '(1 ((shift) . 0.1)))

(setq use-dialog-box nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; NeXTSTEP
(when (string-equal system-type "darwin")
  (setq
   ns-use-native-fullscreen nil
   ns-alternate-modifier 'meta
   ns-right-alternate-modifier 'meta))


;;; Text
(setq-default standard-indent 4
              tab-width 4
              c-basic-offset 4
              sgml-basic-offset 4
              css-indent-offset 2
              indent-tabs-mode nil
              truncate-lines t)

(setq search-highlight t
      query-replace-highlight t
      kill-whole-line t)

(toggle-word-wrap) ;; when (toggle-truncate-lines)
(column-number-mode t)
(show-paren-mode t)
(delete-selection-mode t)

;;; TRAMP mode

;;; Dired
(require 'dired)
(setq dired-find-subdir t
      dired-recursive-deletes 'top)

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)
(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)
(define-key dired-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file)

;;; Windows
;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Windows.html
(windmove-default-keybindings) ; Shift+Arrows when get bored w/ C-x o


;;; Buffers
(require 'ibuffer)
;; TODO: Filename/Process column: prefix [project] for projectile buffers
(defalias 'list-buffers 'ibuffer)
(require 'uniquify)
(setq uniquify-separator "/"
      uniquify-buffer-name-style 'forward)


;; ~/.config/emacs/custom.el or XDG
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file) (load custom-file))


;;; prog-mode
(defun prog-mode-tweaks ()
  (setq display-line-numbers-width 3
        show-trailing-whitespace t)
  (display-line-numbers-mode)
  (toggle-truncate-lines 1))

(add-hook 'prog-mode-hook 'prog-mode-tweaks)

;;; Daggers
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'my-packages)
(require 'suwayyah)
(require 'wm)
(require 'omnitab)
(require 'fallback)
(load-theme 'alh t)

;;; Bindings
(global-set-key [?\t] #'omni-func)
;; (setq tab-always-indent 'complete) ; one day w/ CAPF support

(global-set-key (kbd "C-4") #'delete-other-windows) ; TODO: make hydra for windows commands
(global-set-key (kbd "C-a") #'my/ctrl-a-move-beginning-of)
(global-set-key (kbd "C-e") #'my/ctrl-e-move-end-of)
(global-set-key (kbd "C-w") #'my/ctrl-w-kill)
(global-set-key (kbd "C-t") #'my/ctrl-t)
(global-set-key (kbd "C-8") #'my/highlight)
(global-set-key (kbd "C-*") #'my/unhighlight)
(global-set-key (kbd "C-x i") #'ielm) ; TODO: in new selected frame
(global-set-key (kbd "C-x e") #'eshell)

;; Command is new Meta
(global-set-key (kbd "s-/") #'comment-or-uncomment-region) ; comment-dwim
(global-set-key (kbd "s-i") #'xref-find-definitions)
(global-set-key (kbd "s-I") #'xref-find-definitions-other-frame)
;; https://www.emacswiki.org/emacs/NavigatingParentheses
(global-set-key (kbd "C-s-n") #'forward-list)
(global-set-key (kbd "C-s-p") #'backward-list)


(define-key prog-mode-map [s-mouse-1] #'xref-find-definitions-at-mouse)
(define-key prog-mode-map [C-return] #'my/indent-and-return)

(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))

;; Free bindings
;; C-t
;; C-j
;; C-x f
;; s-b for build
;; s-{+/0/-} already duplicated as C-x C-{+/0/-}


;;; Etc tweaks
(global-auto-revert-mode 1)

(require 'saveplace)
(save-place-mode 1)

(require 'server)
(unless (server-running-p) (server-start))
