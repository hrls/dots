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

;;; erlang section
(add-to-list 'load-path "/usr/local/opt/erlang/lib/erlang/lib/tools-2.9.1/emacs")
(use-package erlang-start
  :init
  (setq erlang-root-dir "/usr/local/opt/erlang/lib/erlang")
  (add-to-list 'exec-path (concat erlang-root-dir "/bin")))

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
