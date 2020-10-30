(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package delight)
(use-package diminish)


(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package ace-window
  :bind ("s-j" . 'ace-window))


(use-package company
  :init (company-tng-configure-default)
  :hook (after-init . global-company-mode)
  :delight '(:propertize " \\_" face shadow)
  :bind (:map company-active-map
              ("C-n" . #'company-select-next)
              ("C-p" . #'company-select-previous)
              ([tab] . #'company-instant-or-cycle))
  :config
  (setq company-abort-manual-when-too-short t
        company-selection-wrap-around t
        company-tooltip-minimum-width 17
        company-tooltip-width-grow-only t
        company-tooltip-align-annotations t))


(use-package rust-mode
  :config
  (setq flycheck-checker 'rust-clippy))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck
  ;; :init (global-flycheck-mode)
  :hook (rust-mode . flycheck-mode))

(use-package projectile
  :init
  (projectile-mode +1)
  :delight
  '(:eval
    (if (projectile-project-p)
        (propertize
         (format " [%s:%s]" (projectile-project-name) (projectile-project-type))
         'face 'shadow)
      (propertize " -" 'face 'shadow)))
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))) ; "C-c p"

(use-package hydra)
(use-package eldoc :delight) ; TODO: useful with tweaks

;; (use-package ido :quelpa)
(require 'ido)
(ido-mode 1)
(setq ido-everywhere t
      ido-enable-flex-matching t
      ido-confirm-unique-completion nil
      ido-use-virtual-buffers t
      ido-create-new-buffer nil
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
(global-set-key (kbd "s-D") #'ido-dired-other-frame)

;; (use-package counsel
;;   :init
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t
;;         ivy-count-format "(%d/%d) "
;;         projectile-completion-system 'ivy))

;; LSP wars

;; (setq lsp-keymap-prefix "s-r")
;; (use-package lsp-mode
;;   :hook (rust-mode . lsp))

;; (use-package lsp-ui)

;; (use-package eglot
;;   :hook (rust-mode . eglot-ensure)))

(use-package haskell-mode :defer t)
(use-package swift-mode :defer t)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package helpful
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-h C" . #'help-command)
         ("C-h F" . #'helpful-function)
         ("C-c C-d" . #'helpful-at-point)))


(provide 'my-packages)
