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
  :init (exec-path-from-shell-initialize))

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
         ("C-x C-d" . counsel-dired)
         ("s-e" . counsel-buffer-or-recentf)
         ("s-f" . counsel-fzf) ;; TODO: sync with .zshrc or ignore .git
         ("s-r" . my/counsel/ripgrep)
         (:map dired-mode-map
               ("C-x j" . counsel-dired-jump)))
  :config
  (defun my/counsel/ripgrep ()
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
  :delight
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-w" . my/ctrl-w-kill)
              ([tab] . my/company/instant-or-cycle))
  :config
  (setq company-abort-manual-when-too-short t
        company-selection-wrap-around t
        company-tooltip-minimum-width 42
        company-tooltip-width-grow-only t
        company-tooltip-align-annotations t)
  (defun my/company/instant-or-cycle ()
    (interactive)
    (if (and (company-tooltip-visible-p)
             (= company-candidates-length 1))
        (company-complete-number 1)
      ;; TODO: endless route '' 'Foo' 'Bar' 'Foo' ..., w/o empty lines
      (company-select-next))))


(use-package flycheck
  :ensure
  :hook (after-init . global-flycheck-mode)
  :bind (("C-0" . flycheck-next-error)      ; C-)
         ("C-9" . flycheck-previous-error)) ; C-(
  :config
  ;; TODO: limit of flycheck-help-echo-function output to one line
  (setq
   flycheck-check-syntax-automatically '(save mode-enabled idle-change)
   flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
   flycheck-indication-mode 'right-fringe)

  ;; https://www.flycheck.org/en/latest/user/error-reports.html#fringe-and-margin-icons
  (when (fboundp 'define-fringe-bitmap)
    ;; https://github.com/flycheck/flycheck/pull/1950
    (defconst flycheck-fringe-bitmap-double-arrow-left
      [27 54 108 216 108 54 27])
    (defconst flycheck-fringe-bitmap-double-arrow-left-hi-res
      [975 1950 3900 7800 15600 31200 31200 15600 7800 3900 1950 975])
    (define-fringe-bitmap
      'flycheck-fringe-bitmap-double-arrow-left
      flycheck-fringe-bitmap-double-arrow-left)
    (define-fringe-bitmap
      'flycheck-fringe-bitmap-double-arrow-left-hi-res
      flycheck-fringe-bitmap-double-arrow-left-hi-res
      nil 16)
    (flycheck-redefine-standard-error-levels
     nil
     (cons 'flycheck-fringe-bitmap-double-arrow-left
           'flycheck-fringe-bitmap-double-arrow-left-hi-res)))

  ;; https://www.flycheck.org/en/latest/user/error-list.html#tune-error-list-display
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.33)))

  ;; https://github.com/flycheck/flycheck-haskell
  ;; https://github.com/flycheck/flycheck-rust
  ;; TODO: modify 'rust checker for support checking buffers w/o saving to file
  (setq flycheck-global-modes '(rust-mode haskell-mode))
  (setq-default flycheck-disabled-checkers '(rust rust-cargo)))


;; TODO:
;;  - [ ] tweak abbrev completions for company (SS expands into S.*S.*)
(use-package rust-mode
  :ensure
  :init
  (defun my/rust-mode/hook ()
    ;; https://rust-lang.github.io/rustfmt/?version=master&search=#max_width
    (set-fill-column 100)
    (display-fill-column-indicator-mode))
  :hook (rust-mode . my/rust-mode/hook)
  :bind (:map rust-mode-map
              ("C-c c" . flycheck-buffer))
  :config
  ;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
  (modify-syntax-entry ?_ "w" rust-mode-syntax-table))

(use-package cargo
  :ensure
  :hook (rust-mode . cargo-minor-mode))

(use-package haskell-mode
  :ensure
  :bind (:map haskell-mode-map
              ("C-c c" . flycheck-buffer) ; TODO: what about prog-mode
              ("C-c C-c" . haskell-check)
              ("C-c C-o" . haskell-navigate-imports))
  :config
  (defun haskell-check-current (arg)
    (interactive "f")
    (user-error "TODO: haskell-check-current")))

;; LSP wars
;; https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
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

;; (use-package delsel
;;   :bind
;;   (:map mode-specific-map
;;         ("C-g" . minibuffer-keyboard-quit)))

(provide 'my-packages)
