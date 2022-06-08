(require 'autothemer)

;; describe-face
;; describe-theme
;; list-faces-display


;; TODO:
;; [ ] - emphasize keywords more with color (try light blue)
;; [ ] - dim numbers for display-line-number-mode on inactive frames or windows
;; [ ] - hl-line-mode affects only line numbers
;; [ ] - tweak trailing whitespace face

(autothemer-deftheme
 alh "Particles and yellow pages"

 (
  (((class color) (min-colors #xFFFFFF) (background light)))

  ;; background colors candidates:
  ;; RAL 9010 Pure white #f1ece1
  ;; RAL 1013 Oyster white #e3d9c6 <- little saturation would help
  ;; RAL 140-5 #f8e4c8 ; desaturated f3e3cd
  ;; Yellow pages #f2e0c9 <- nice one, but pinkish
  ;; Attempt n2 #e8dbbb

  ;; (load-theme 'alh t)

  (pal/background "#f3e3cd")

  ;; function name is grey bold
  ;; str literal are plain blue, as current function name

  (pal/var "#903373")
  (pal/const "#6e6387") ; 794d3e

  (pal/docline "#792423")

  (pal/black "#000000")
  (pal/red   "#ff0000")
  (pal/blue  "#0000ff")

  ;; https://www.ralcolorchart.com/ral-classic
  (pal/cobalt-blue      "#193153") ; RAL 5013 Cobalt blue
  (pal/traffic-purple   "#903373") ; RAL 4006 Traffic purple
  (pal/iron-grey        "#52595d") ; RAL 7011 Iron grey
  (pal/basalt-grey      "#575d5e") ; RAL 7012 Basalt grey
  (pal/anthracite-grey  "#383e42") ; RAL 7016 Anthracite grey

  (pal/pearl-violet "#6e6387") ; RAL 4011 Pearl violet (almost good for bold italic kwords)

  (pal/night-blue  "#222d5a") ; RAL 5022 Night blue (nice type face when just italic)
  (pal/khaki-grey  "#745e3d") ; RAL 7008 Khaki grey (italic for text, bold for onewords)
  (pal/pebble-grey "#b5b0a1") ; RAL 7032 Pebble grey (almost invisible)
  )

 (
  (default (:foreground pal/black :background pal/background))
  (button  (:foreground pal/black :underline t :weight 'bold))
  (error   (:foreground pal/red))

  ;; :weight 'bold


  (font-lock-constant-face (:foreground pal/const))

  (font-lock-variable-name-face (:foreground pal/black))
  (font-lock-doc-face     (:foreground pal/docline :italic t))
  (font-lock-string-face (:foreground pal/blue)) ; TODO: relax it


  (font-lock-keyword-face (:foreground pal/traffic-purple :italic t :weight 'bold))
  (font-lock-type-face    (:foreground pal/cobalt-blue)) ; TODO: make accent, like blue-ish
  (font-lock-function-name-face (:foreground pal/cobalt-blue :weight 'bold))


  (font-lock-comment-face (:foreground pal/iron-grey :italic t))

  (dired-directory (:inherit 'font-lock-type-face))
  (which-key-key-face (:inherit 'font-lock-constant-face :weight 'bold))
  ))

;; https://github.com/jasonm23/autothemer#re-using-the-color-palette
;; (define-advice autothemer-deftheme (:before (_ _ palette &rest _) defcolors)
;;   (mapcar
;;    (lambda (e) (setf (symbol-value (car e)) (cadr e)))
;;    (cdr palette)))

(provide-theme 'alh)
