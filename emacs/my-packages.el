(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package ace-window
  :bind ("s-j" . 'ace-window))


(use-package company
  :init (company-tng-configure-default)
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("C-n" . 'company-select-next)
              ("C-p" . 'company-select-previous)
              ([tab] . 'company-instant-or-cycle))
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
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))) ; "C-c p"

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

(provide 'my-packages)
