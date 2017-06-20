;; (setq os (if (memq window-system '(ns mac)) 'macos 'unix))
;; (require 'package)
;; (add-to-list 'package-archives
;;  	     '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)

(add-to-list 'load-path "~/.emacs.d/use-package")
(require 'use-package)

(use-package markdown-mode
  :load-path "markdown-mode"
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :load-path "yaml-mode"
  :mode (("\\.yml\\'" . yaml-mode)
		 ("\\.yaml\\'" . yaml-mode)))

;;; erlang section
(use-package erlang-start
  :load-path "/usr/local/opt/erlang/lib/erlang/lib/tools-2.9.1/emacs"
  :init
  (setq erlang-root-dir "/usr/local/opt/erlang/lib/erlang")
  (add-to-list 'exec-path (concat erlang-root-dir "/bin"))
  (setq erlang-man-root-dir (concat erlang-root-dir "/man"))
  ;; :bind (("RET" . newline-and-indent))
  )

;;; haskell section
(use-package haskell-mode-autoloads
  :load-path "haskell-mode"
  :init
  (add-to-list 'Info-default-directory-list "~/.emacs.d/haskell-mode")
  :config
  (setq haskell-indent-offset 4
		haskell-indent-spaces 4
		haskell-indentation-left-offset 4
		haskell-indentation-layout-offset 4
		haskell-indentation-starter-offset 4
		haskell-indentation-where-pre-offset 4
		haskell-indentation-where-post-offset 4))

(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;;; GUI
(setq ring-bell-function 'ignore)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq line-number-mode t)
(setq column-number-mode t)

(setq default-tab-width 4)
(setq-default truncate-lines t)

(blink-cursor-mode -1)
(set-default 'cursor-type 'hbar)
(set-default 'cursor-in-non-selected-windows 'bar)

;; http://emacs-fu.blogspot.com.by/2009/01/balancing-your-parentheses.html
(show-paren-mode t)
(setq show-paren-style 'mixed)

(use-package ido
  :config
  (ido-mode t))
(use-package url)

(add-to-list 'load-path "~/.etc")
(load "binds")
(load "suwayyah")

(load-theme 'leuven)

(server-start)

;; (defun set-frame-startup-size-and-position ()
;;   (interactive)
;;   (if window-system
;; 	  (progn
;; 		(if (= (x-display-pixel-width) 1280)
;; 			(progn
;; 			  (add-to-list 'default-frame-alist (cons 'width 100))
;; 			  (add-to-list 'default-frame-alist (cons 'height 49))
;; 			  (add-to-list 'default-frame-alist (cons 'top 20))
;; 			  (add-to-list 'default-frame-alist (cons 'left 300)))))))

;; (set-frame-startup-size-and-position)
;; (add-hook 'emacs-startup-hook 'delete-other-windows) ; fixme
