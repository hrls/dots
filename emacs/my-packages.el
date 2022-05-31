(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; (setq use-package-always-ensure t)
;; https://github.com/jwiegley/use-package/issues/494#issuecomment-322181026

(use-package delight :ensure t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t
  :delight
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.7))

(use-package ace-window
  :ensure t
  :bind ("s-j" . ace-window))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package counsel
  :ensure t
  :init
  (setq ivy-on-del-error-function 'ignore
        ivy-use-virtual-buffers t
        ivy-count-format ""
        ivy-display-style 'fancy
        ivy-magic-tilde t
        projectile-completion-system 'ivy)
  :bind (("C-s" . swiper)
         ("C-r" . swiper-isearch-thing-at-point)
         ("C-x C-f" . counsel-find-file)
         ("s-b" . counsel-buffer-or-recentf)
         ("s-f" . counsel-fzf)
         ("s-r" . hrls/counsel-rg))
  :config
  (defun hrls/counsel-rg ()
    ;; TODO:
    ;; - $PWD or project root in prompt
    (interactive)
    (let ((initial-input
           (if (region-active-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (if-let (symbol (symbol-at-point)) (symbol-name symbol)))))
      (counsel-rg initial-input nil "--hidden" nil))))


(use-package projectile
  :ensure t
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


(use-package company
  :ensure t
  :init (company-tng-configure-default)
  :hook (after-init . global-company-mode)
  :delight '(:propertize " t* " face shadow)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ([tab] . hrls/company-instant-or-cycle))
  :config
  (setq company-abort-manual-when-too-short t
        company-selection-wrap-around t
        company-tooltip-minimum-width 27
        company-tooltip-width-grow-only t
        company-tooltip-align-annotations t)
  (defun hrls/company-instant-or-cycle ()
    (interactive)
    (if (and (company-tooltip-visible-p)
             (= company-candidates-length 1))
        (company-complete-number 1)
      ;; TODO: endless route '' 'Foo' 'Bar' 'Foo' ..., w/o empty lines
      (company-select-next))))


;; TODO:
;;  - [ ] rust etags
;;  - [ ] tweak abbrev completions for company (SS expands into S.*S.*)
(use-package rust-mode
  :ensure t
  :config
  ;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
  (require 'misc)
  (modify-syntax-entry ?_ "w" rust-mode-syntax-table)
  (setq flycheck-checker 'rust-clippy))


(use-package cargo
  :ensure t
  ;; :mode ("Cargo.toml" . cargo-minor-mode) ; TODO: fix conflict w/ conf-toml-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck
  :ensure t)
;; :init (global-flycheck-mode)
;; :hook (rust-mode . flycheck-mode)


;; LSP wars

;; (setq lsp-keymap-prefix "s-r")
;; (use-package lsp-mode
;;   :hook (rust-mode . lsp))

;; (use-package lsp-ui)

;; (use-package eglot
;;   :hook (rust-mode . eglot-ensure)))

;; (use-package deadgrep) ; nice w/ dangled frame
;; (use-package rg) ; plain M-x rg

(use-package haskell-mode :ensure t)

(use-package swift-mode :ensure t)
(use-package dockerfile-mode :ensure t)

;; (use-package hydra :ensure t)
(use-package eldoc :delight) ; TODO: useful with tweaks

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h C" . help-command)
         ("C-h F" . helpful-function)
         ("C-c C-d" . helpful-at-point)))

;; https://github.com/coldnew/eshell-autojump/issues/3
;; (use-package eshell-autojump)

(provide 'my-packages)
