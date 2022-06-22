;; https://www.emacswiki.org/emacs/TabCompletion#SmartTab

;; indent if line is empty (indent-according-to-mode)
;; (complete-at-point-function) if line is already indented
(defun omni-func ()
  (interactive)
  ;; TODO:
  ;;  disable company autocompletion when *Completions* buffer is already active
  ;;   ielm autocompletion shows default completions as well as company for the same string
  ;;
  (cond
   ((use-region-p) (indent-for-tab-command))
   (t
    (if (looking-at "\\>") ; end of symbol
        (when (company-manual-begin)
          (if (= company-candidates-length 1)
              (company-complete-number 1)))
      (indent-for-tab-command)))))


;; var completion-styles
;; https://github.com/company-mode/company-mode/wiki/Tips-&-tricks#enable-bindings-conditionally-for-a-shown-tooltip

;; (require 'company)
;; (require 'cl-lib)

;; foo[bar] - one tab
;; foo[bar*] - tab to insert common and expand tooltip
;;             tooltip rotates candidates in tng way
;; fooerr
;; foobar

;; TODO: try to re-use `company-preview-show-at-point`
;;       look into ptf-workaround


;; Some corner cases for inlining
;; command
;;     post-command
;;     prefix ’make-local-var’
;;     common ’make-local-variable’
;;     selected nil
;;     inlined t
;;     tooltip :visible

;; command
;;     update
;;     prefix ’mak-local-variab’
;;     common ’make-local-variable’
;;     selected nil
;;     inlined nil
;;     tooltip :visible


(defun company-ttt-mode ()
  (interactive)

  (require 'company)
  (require 'cl-lib)

  (cl-defstruct company-variables
    is-active command prefix common selected inline-p tooltip)

  (defun company-capture (command)
    (make-company-variables
     :is-active (if (company--active-p) t nil)
     :command command
     :prefix company-prefix
     :common company-common
     :selected company-selection
     :inline-p (company--show-inline-p)
     :tooltip (if (company-tooltip-visible-p) :visible)))


  (defun print-company-variables (vars)
    ;; TODO: cl-destructuring-bind
    (message "is-active %s, command %s, prefix '%s', common '%s', selected %s, inlined %s, tooltip %s"
             (company-variables-is-active vars)
             (company-variables-command  vars)
             (company-variables-prefix   vars)
             (company-variables-common   vars)
             (company-variables-selected vars)
             (company-variables-inline-p vars)
             (company-variables-tooltip  vars)))

  ;;; Future ttt-mode

  (defvar-local company-ttt--overlay nil)

  (defvar company-ttt-map
    (let ((keymap (make-sparse-keymap)))
      (set-keymap-parent keymap company-active-map)
      (define-key keymap [return] nil)
      (define-key keymap (kbd "RET") nil)

      (define-key keymap [tab] 'company-ttt-complete-or-next)
      (define-key keymap (kbd "TAB") 'company-ttt-complete-or-next)
      ;; ===========================================================
      (define-key keymap (kbd "C-t") 'company-dump-vars)

      (define-key keymap [backtab] 'company-select-previous)
      (define-key keymap (kbd "S-TAB") 'company-select-previous)
      keymap))

  (defun company-dump-vars ()
    (interactive)
    (print-company-variables (company-capture :manual-when-active)))

  (defun company-ttt-complete-or-next ()
    (interactive)
    ;; (company-select-next-if-tooltip-visible-or-complete-selection)
    ;; (company-select-next) ; what if the only one candidate is available
    (my/company/instant-or-cycle)
    )

  (defun comp-ttt-frontend (command)
    (print-company-variables (company-capture command))

    (cl-case command
      (show
       (let ((ov (make-overlay (point) (point))))
         (setq company-ttt--overlay ov)
         (overlay-put ov 'priority 2)))

      (update
       (let ((ov company-ttt--overlay))
         (cond
          ;; Exact candidate is selected in tooltip
          (company-selection
           (let* ((selected (nth company-selection company-candidates))
                  (prefix (length company-prefix))
                  (tprop (if (= prefix 0) 'after-string 'display)))
             ;; (message "show as %s" tprop)
             (move-overlay ov (- (point) prefix) (point))
             (overlay-put ov tprop selected)))

          ;; The only one candidate that matches prefix
          ((and (not (cdr company-candidates))
                company-common
                (not (eq t (compare-strings company-prefix nil nil
                                            (car company-candidates) nil nil
                                            t))))
           (let ((keep-prefix (eq (company-call-backend 'ignore-case) 'keep-prefix)))
             (when-let (((or keep-prefix
                             (string-prefix-p company-prefix company-common)))
                        (common (company-strip-prefix company-common)))
               ;; (message "keep prefix is: %s" keep-prefix)
               (add-face-text-property 0 (length common) 'company-preview nil common)
               (add-text-properties 0 1 '(cursor 1) common) ; cursor before completing
               (move-overlay ov (point) (point))
               (overlay-put ov 'after-string common))))

          (t ; otherwise
           ;; (message "otw case")
           (overlay-put ov 'after-string nil)
           (overlay-put ov 'display nil)
           ))))

      (hide
       (when company-ttt--overlay
         (delete-overlay company-ttt--overlay)
         (kill-local-variable 'company-ttt--overlay)))
      (pre-command
       (when-let (ov company-ttt--overlay)
         (overlay-put ov 'after-string nil))
       (when (and company-selection
                  (not (company--company-command-p (this-command-keys))))
         (company--unread-this-command-keys)
         (setq this-command 'company-complete-selection)))))


  (setq company-require-match nil
        company-selection-default nil

        company-frontends '(comp-ttt-frontend
                            ;; company-pseudo-tooltip-unless-just-one-frontend
                            company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)

        company-active-map company-ttt-map)
  )

(company-ttt-mode)


;; eval this

;; basic company configuration
(setq company-frontends '(company-tng-frontend
                          company-pseudo-tooltip-frontend
                          company-echo-metadata-frontend)

      company-active-map company-tng-map)


(provide 'omnitab)
