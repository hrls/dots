;;; GUI
(setq ring-bell-function 'ignore)

(tool-bar-mode -1)
(blink-cursor-mode 0)

;;; srv-mode
(server-start)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

