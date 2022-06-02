(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; (setq use-package-always-ensure t)
;; https://github.com/jwiegley/use-package/issues/494#issuecomment-322181026

(use-package delight :ensure)
(use-package autothemer :ensure)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure
  :config (exec-path-from-shell-initialize))

(use-package which-key
  :ensure
  :delight
  :config
  (which-key-mode)
  (which-key-setup-minibuffer)
  (setq which-key-idle-delay 0.7
        which-key-popup-type 'minibuffer))


(use-package ace-window
  :ensure
  :bind ("s-j" . ace-window))

(use-package expand-region
  :ensure
  :bind ("C-=" . er/expand-region))


(use-package magit
  :ensure
  :bind ("C-x g" . magit-status))

(use-package projectile
  :ensure
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

(use-package counsel
  :ensure
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


(use-package company
  :ensure
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

(use-package flycheck :ensure)
;; :init (global-flycheck-mode)
;; :hook (rust-mode . flycheck-mode)


;; TODO:
;;  - [ ] tweak abbrev completions for company (SS expands into S.*S.*)
(use-package rust-mode
  :ensure
  :config
  ;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
  (require 'misc)
  (modify-syntax-entry ?_ "w" rust-mode-syntax-table)
  (setq flycheck-checker 'rust-clippy))

(use-package cargo
  :ensure
  :hook (rust-mode . cargo-minor-mode))


(use-package haskell-mode
  :ensure)

;; LSP wars
;; ;; https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
;;
;; https://robert.kra.hn/posts/rust-emacs-setup/
;; https://rust-analyzer.github.io/manual.html#emacs
;;
;; https://github.com/emacs-lsp/lsp-ivy
;; https://github.com/emacs-lsp/lsp-haskell

(use-package eldoc :delight) ; TODO: useful with tweaks


(use-package yaml-mode :ensure)
(use-package dockerfile-mode :ensure)
(use-package swift-mode :ensure)
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; https://github.com/coldnew/eshell-autojump/issues/3
;; (use-package eshell-autojump)

;; (use-package helpful)
;; (use-package hydra)
;; (use-package deadgrep) ; nice w/ dangled frame


(provide 'my-packages)
