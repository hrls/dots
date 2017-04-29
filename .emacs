;; (require 'package)
;; (add-to-list 'package-archives
;; 	     '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)

(add-to-list 'load-path "~/.emacs.d/use-package")

(require 'use-package)
;; (use-package markdown-mode
;;   :ensure t)
;; (use-package haskell-mode
;;   :ensure t)

(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;;; GUI
(setq ring-bell-function 'ignore)

(tool-bar-mode -1)
(menu-bar-mode 0)
(scroll-bar-mode -1)

(blink-cursor-mode -1)

(require 'ido)
(ido-mode t)

;;; srv-mode
(server-start)
