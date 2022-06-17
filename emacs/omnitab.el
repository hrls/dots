;; https://www.emacswiki.org/emacs/TabCompletion#SmartTab

;; indent if line is empty (indent-according-to-mode)
;; (complete-at-point-function) if line is already indented
(defun omni-func ()
  (interactive)

  (cond
   ((use-region-p) (indent-for-tab-command))
   (t
    (if (looking-at "\\>") ; end of symbol
        (when (company-manual-begin)
          (if (= company-candidates-length 1)
              (company-complete-number 1)))
      (indent-for-tab-command)))))



;; ttt ======================================================================
(require 'company)
(require 'cl-lib)

(defvar company-ttt-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap company-active-map)
    (define-key keymap [return] nil)
    (define-key keymap (kbd "RET") nil)

    (define-key keymap [tab] 'company-select-next)
    ;; (define-key keymap [tab] 'company-complete-common-or-cycle)
    ;; (define-key keymap [tab] 'company-complete-common-or-show-delayed-tooltip)
    (define-key keymap (kbd "C-t") 'my/ctrl-t)

    (define-key keymap (kbd "TAB") 'company-select-next)
    (define-key keymap [backtab] 'company-select-previous)
    (define-key keymap (kbd "S-TAB") 'company-select-previous)
    keymap))

(defvar-local company-ttt--overlay nil)

(progn
  (cl-defstruct company-variables
    command prefix common selected inline-p tooltip)

  (defun company-capture (command)
    (make-company-variables
     :command command
     :prefix company-prefix
     :common company-common
     :selected company-selection
     :inline-p (company--show-inline-p)
     :tooltip (if (company-tooltip-visible-p) :visible)))

  (defun print-company-variables (vars)
    ;; TODO: cl-destructuring-bind
    (message "command %s, prefix '%s', common '%s', selected %s, inlined %s, tooltip %s"
             (company-variables-command  vars)
             (company-variables-prefix   vars)
             (company-variables-common   vars)
             (company-variables-selected vars)
             (company-variables-inline-p vars)
             (company-variables-tooltip  vars)))
  )


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


(defun company-ttt-frontend (command)
  (print-company-variables (company-capture command))

  (cl-case command
    (show
     (let ((ov (make-overlay (point) (point))))
       (setq company-ttt--overlay ov)
       (overlay-put ov 'priority 2)))
    (update
     (let* ((ov company-ttt--overlay)
            (selected (and company-selection
                           (nth company-selection company-candidates)))
            (prefix (length company-prefix)))
       (move-overlay ov (- (point) prefix) (point))
       (overlay-put ov
                    (if (= prefix 0) 'after-string 'display)
                    selected)))
    (hide
     (when company-ttt--overlay
       (delete-overlay company-ttt--overlay)
       (kill-local-variable 'company-ttt--overlay)))
    (pre-command
     (when (and company-selection
                (not (company--company-command-p (this-command-keys))))
       (company--unread-this-command-keys)
       (setq this-command 'company-complete-selection)))))

(progn
  (setq company-frontends '(company-ttt-frontend
                            company-pseudo-tooltip-frontend
                            ;; company-pseudo-tooltip-unless-just-one-frontend
                            ;; company-pseudo-tooltip-unless-just-one-frontend-with-delay
                            company-echo-metadata-frontend))
  (setq company-active-map company-ttt-map)
  (setq company-require-match nil
        company-selection-default nil))




(provide 'omnitab)
