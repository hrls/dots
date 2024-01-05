(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(if (version< emacs-version "29")
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

;; (setq use-package-always-ensure t)
;; https://github.com/jwiegley/use-package/issues/494#issuecomment-322181026

(use-package delight :ensure)
(use-package autothemer :ensure)
(use-package hydra :ensure)

(use-package exec-path-from-shell :ensure
  :if (memq window-system '(mac ns))
  :init (exec-path-from-shell-initialize))

(use-package which-key :ensure
  :delight
  :config
  (which-key-mode)
  (which-key-setup-minibuffer)
  (setq which-key-idle-delay 0.7
        which-key-popup-type 'minibuffer))


(use-package ace-window :ensure
  :bind ("s-j" . ace-window))

(use-package expand-region :ensure
  :bind ("C-=" . er/expand-region))


(use-package magit :ensure :defer
  :bind ("C-x g" . magit-status))

(use-package projectile :ensure
  :init
  (projectile-mode +1)
  ;; :delight
  ;; '(:eval
  ;;   (if (projectile-project-p)
  ;;       (propertize
  ;;        (format " [%s:%s]" (projectile-project-name) (projectile-project-type))
  ;;        'face 'shadow)
  ;;     (propertize " -" 'face 'shadow)))
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))) ; "C-c p"

(use-package counsel :ensure
  :init
  (setq ivy-on-del-error-function 'ignore
        ivy-use-virtual-buffers t
        ivy-count-format ""
        ivy-display-style 'fancy
        ivy-magic-tilde t
        projectile-completion-system 'ivy)
  :bind (("C-s"   . swiper)
         ("C-r"   . swiper-isearch-thing-at-point)
         ("C-x b" . counsel-switch-buffer)
         ("C-x f" . counsel-find-file)
         ("C-x d" . counsel-dired)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("s-x"   . counsel-M-x)
         ("s-e"   . counsel-buffer-or-recentf)
         ("s-f"   . counsel-fzf) ;; TODO: sync with .zshrc or ignore .git
         ("s-r"   . my/counsel/ripgrep)
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


(use-package company :ensure
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html#index-initials_002c-completion-style
  ;; TODO: merge candidates with priority (keyword > word)
  :init
  (company-tng-configure-default)
  ;; (custom-set-variables '(company-quick-access-modifier 'control))
  :custom
  (company-quick-access-modifier 'control)
  :hook (after-init . global-company-mode)
  ;; :delight
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-w" . my/ctrl-w-kill)
              ("s-i" . company-show-location)
              ([tab] . my/company/instant-or-cycle))
  :config
  (setq company-minimum-prefix-length 3
        company-idle-delay 0.11
        company-abort-manual-when-too-short t
        company-selection-wrap-around t
        ;; interface part
        company-tooltip-minimum-width 42
        company-tooltip-width-grow-only t
        company-tooltip-limit 16
        company-tooltip-align-annotations t ; TODO: doesnt works
        company-tooltip-flip-when-above nil ; TODO: C-p must select first element when flipped
        company-show-quick-access 'left
        company-format-margin-function 'company-text-icons-margin
        company-text-face-extra-attributes '(:slant italic :weight bold)
        company-text-icons-add-background nil)
  (defun my/company/instant-or-cycle ()
    (interactive)
    (if (and (company-tooltip-visible-p)
             (= company-candidates-length 1))
        (company-complete-tooltip-row 1)
      ;; TODO:
      ;; - [ ] comlete common before selecting first candidate
      ;; - [ ] rotate candidates '' 'Foo' 'Bar' 'Foo' ...
      ;;       and skip selecting of empty line on the end of candidates list
      (company-select-next))))



;; TODO: 'rust-ts-mode uses 'treesit package, not this one
;;   https://github.com/emacs-tree-sitter/tree-sitter-langs/issues/163
;;   https://archive.casouri.cc/note/2023/tree-sitter-starter-guide/
;; (use-package tree-sitter :ensure
;;   :init (use-package tree-sitter-langs :ensure)
;;   :config (global-tree-sitter-mode)
;;   :hook (tree-sitter-after-on . tree-sitter-hl-mode))


;; - [ ] delay syntax check to prevent hl errors on incomplete lines
(use-package eglot :ensure
  :config
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer"
                                             :initializationOptions (:check (:command "clippy")))))
  :bind
  (:map prog-mode-map
        ("C-c f" . eglot-format)
        ("C-c s e" . eglot)
        ("C-c s s" . eglot-shutdown)))

;; TODO:
;;  - [ ] tweak abbrev completions (SS expands into \\S[a-z]*S[a-z]*), like `initial` completion style
;; rust-ts-mode uses 'treesit package not 'tree-si
(use-package rust-mode :ensure
  :init
  (defun my/rust-mode/hook ()
    ;; https://rust-lang.github.io/rustfmt/?version=master&search=#max_width
    (set-fill-column 100)
    (display-fill-column-indicator-mode))
  :hook
  ((rust-mode rust-ts-mode) . my/rust-mode/hook)
  :bind
  (:map rust-mode-map
        ("s-b" . cargo-process-build)
        ("C-c c" . flycheck-buffer))
  :config
  ;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
  (modify-syntax-entry ?_ "w" rust-mode-syntax-table))


(use-package cargo :ensure
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))

(use-package haskell-mode :ensure :defer
  :bind (:map haskell-mode-map
              ("C-c c" . flycheck-buffer) ; TODO: what about prog-mode
              ("C-c C-c" . haskell-check)
              ("C-c C-o" . haskell-navigate-imports))
  :custom
  ;; https://github.com/haskell/haskell-mode/blob/master/haskell-indentation.el
  (haskell-indentation-layout-offset 4)
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

(use-package eldoc
  :delight
  :config
  (setq eldoc-idle-delay 0.5
        eldoc-echo-area-prefer-doc-buffer t))

(use-package yaml-mode :ensure)
(use-package dockerfile-mode :ensure)
(use-package swift-mode :ensure)
(use-package purescript-mode :ensure :defer :disabled
  :hook (purescript-mode . my/purescript-mode/config)
  :init
  ;; https://github.com/purescript-emacs/purescript-mode/blob/master/examples/init.el
  (defun my/purescript-mode/config ()
    (turn-on-purescript-indentation)))
(use-package dhall-mode :ensure :defer)
(use-package solidity-mode :ensure :defer)
(use-package protobuf-mode :ensure :defer)


(use-package markdown-mode
  :ensure
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package help-mode
  :bind (:map help-mode-map
              ("b" . help-go-back)
              ("f" . help-go-forward)
              ("p" . previous-line)
              ("n" . next-line)
              ("s-i" . help-view-source)))


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
