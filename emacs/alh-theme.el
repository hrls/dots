(require 'autothemer)

;; https://coolors.co/contrast-checker
;; https://encycolorpedia.com

;; list-colors-display
;; describe-face
;; describe-theme
;; list-faces-display

;; TODO:
;; [ ] - M-x mininuffer-prompt
;; [ ] - eshell
;; [ ] - dim numbers for display-line-number-mode on inactive frames or windows
;; [ ] - Dark background

(autothemer-deftheme
 alh "Particles and yellow pages. Purple and violet on antiquewhite"

 ((((class color) (min-colors #xFFFFFF) (background light)))
  ;; (load-theme 'alh t)

  (pal/bg   "#faebd7") ; antiquewhite
  (pal/kw   "#872657") ; was 8f3d66 desaturated websafe #936, RAL 4006 Traffic purple
  (pal/type "#162f65") ; current gh literal, was 484063 unknown color

  (pal/constant     "#5b4799")
  (pal/builtin      "#324b80")
  (pal/preprocessor "#be3536")

  (pal/string  "#1313b0") ; #1313b0 4169e1
  (pal/comment "#666666") ; 7011 Iron grey #52595d
  (pal/docline "#2b3f98") ; unknown color

  (pal/shadow "#666666")
  (pal/phantom "#8b7765")

  (pal/gh/str-literal     "#162f65")
  (pal/gh/button-link     "#2e68d3") ; also used for some kws
  (pal/gh/tritanopiad/red "#be3536")

  ;; like 800080
  (pal/grey        "#666666")
  (pal/dark-sienna "#804224")
  (pal/dark-orange "#cc7000")

  (pal/okay "#6495ed")

  (pal/red   "#ff3b30") ; system red
  (pal/blue  "#0000cd")
  (pal/amber "#ffbf00")
  (pal/black "#040404"))

 ((default (:foreground pal/black :background pal/bg))
  (button  (:foreground pal/gh/button-link :underline t :weight 'bold))
  (success (:foreground pal/okay :weight 'bold))
  ;; (warning) is DarkOrange currently
  (error   (:foreground pal/red :weight 'bold))

  (fringe (:inherit 'default))

  (font-lock-keyword-face       (:foreground pal/kw :weight 'bold))
  (font-lock-type-face          (:foreground pal/type :weight 'bold))
  (font-lock-function-name-face (:foreground pal/black))
  (font-lock-variable-name-face (:foreground pal/black))

  (font-lock-string-face        (:foreground pal/string))
  (font-lock-constant-face      (:foreground pal/constant :weight 'bold))
  (font-lock-builtin-face       (:foreground pal/builtin :italic t))
  (font-lock-preprocessor-face  (:foreground pal/preprocessor :italic t))

  (font-lock-comment-face       (:foreground pal/comment))
  (font-lock-comment-delimiter-face (:inherit 'font-lock-comment-face :italic t))
  (font-lock-doc-face           (:foreground pal/docline :italic t))

  ;; (compilation-warning (:foreground pal/dark-orange :weight 'bold))
  (compilation-line-number (:inherit 'button :foreground pal/black))
  (compilation-column-number (:inherit 'button :foreground pal/shadow))
  (compilation-mode-line-exit (:inherit 'compilation-info :foreground pal/okay :weight 'normal))

  (line-number-current-line (:inherit 'line-number :foreground pal/black :weight 'bold)) ; background reserved for dbg-mode
  (show-paren-match (:background pal/amber))
  (trailing-whitespace (:foreground pal/red :underline t))

  (dired-directory (:inherit 'font-lock-type-face))
  (dired-symlink (:inherit 'font-lock-keyword-face :italic t :bold nil))
  (which-key-key-face (:inherit 'font-lock-constant-face :weight 'bold))

  (company-preview (:background pal/bg :foreground pal/phantom))

  (rust-ampersand-face (:foreground pal/shadow))
  ))

;; https://github.com/jasonm23/autothemer#re-using-the-color-palette
;; (define-advice autothemer-deftheme (:before (_ _ palette &rest _) defcolors)
;;   (mapcar
;;    (lambda (e) (setf (symbol-value (car e)) (cadr e)))
;;    (cdr palette)))

(provide-theme 'alh)
