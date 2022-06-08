(provide 'wm)

(defun my/cmd-w-kill ()
  "Close window, or frame, or kill buffer"
  (interactive)
  (cond ((< 1 (length (window-list))) (delete-window))
        ((< 1 (length (frame-list))) (delete-frame))
        (t
         (message "Buffer was killed by greedy W")
         (kill-buffer))))

(defun make-frame-at-center ()
  (interactive)
  (let ((displays (display-monitor-attributes-list)))
    (cond ((= 1 (length displays))
           (let* ((workarea (alist-get 'workarea (car displays)))
                  (width-padding (lambda (w) (/ w 4)))
                  (height-padding (lambda (h) (/ h 7))))
             (cl-multiple-value-bind
                 (left top right bottom)
                 (padded-rectangle workarea width-padding height-padding)
               (let* ((width (- right left))
                      (height (- bottom top))
                      (params (list
                               ;; visibility . nil => make-frame-visible
                               (cons 'top top)
                               (cons 'left left)
                               (cons 'width (cons 'text-pixels width))
                               (cons 'height (cons 'text-pixels height)))))
                 (make-frame params)))))
          (t (user-error "TODO: support multiple monitors")))))

;; area :: (x y x y)
;; w-padding  :: const, \width  -> padding
;; h-padding :: const, \height -> padding
;; -> (x y x y)
(defun padded-rectangle (area w-padding h-padding)
  (cl-multiple-value-bind
      (left top right bottom) area
    (let ((w-span (cond ((numberp w-padding) w-padding)
                        ((functionp w-padding)
                         (funcall w-padding (- right left)))))
          (h-span (cond ((numberp h-padding) h-padding)
                        ((functionp h-padding)
                         (funcall h-padding (- bottom top))))))

      (list (+ left w-span)
            (+ top h-span)
            (- right w-span)
            (- bottom h-span)))))

(defun select-nth-visible-frame (n)
  (let* (($frame-position
          (lambda ($frame)
            (if (eq 't (frame-parameter $frame 'visibility))
                (list (cons (frame-parameter $frame 'left) $frame)))))
         ($frames (seq-sort-by 'car '< (mapcan $frame-position (frame-list)))))
    (if-let (($target-frame (cdr (nth (- n 1) $frames))))
        (select-frame-set-input-focus $target-frame))))


(global-set-key (kbd "s-w") 'my/cmd-w-kill)
(global-set-key (kbd "s-n") 'make-frame-at-center)
(global-set-key (kbd "s-<f11>") 'ns-do-hide-others)

(global-set-key (kbd "s-1") (lambda () (interactive) (select-nth-visible-frame 1)))
(global-set-key (kbd "s-2") (lambda () (interactive) (select-nth-visible-frame 2)))
(global-set-key (kbd "s-3") (lambda () (interactive) (select-nth-visible-frame 3)))
(global-set-key (kbd "s-4") (lambda () (interactive) (select-nth-visible-frame 4)))


;; TODO: save in custom.el
;; defcustom wm-initial-frame-alist
;; setq initial-frame-alist wm-initial-frame-alist

(defun wm-save-selected-frame-as-initial ()
  (interactive)
  (let ((position (frame-position)))
    (set-in-alist 'initial-frame-alist (cons 'left (car position)))
    (set-in-alist 'initial-frame-alist (cons 'top (cdr position)))
    (set-in-alist 'initial-frame-alist (cons 'width (frame-width)))
    (set-in-alist 'initial-frame-alist (cons 'height (frame-height)))
    (customize-set-value 'initial-frame-alist initial-frame-alist)))


;; 1300 x 130 on iMac 27
;; 888 x 50 on MBP 16
(setq initial-frame-alist
      '((left . 888) (top . 50) (width . 120) (height . 70)))
;; (add-to-list 'default-frame-alist '(font . "Monaco 12") t)
(add-to-list 'default-frame-alist '(font . "Menlo 12") t)


;;; N-frames environment
;;; External commmands always open frames in some predicted area
;;;   rotate relative offset from already existing frames

;;; terminal 'e file_1 file_2 file_3' => [f[f[file_3]]] overlapping batch
;;; terminal 'e ${dir_or_file}' as initial frame

;; (setq default-frame-alist initial-frame-alist + some offset )


;; TODO: add it to {after-make,after-delete,move}-frame-functions
(defun wm-set-position-for-new-frame ())
;;  (setq default-frame-alist (wm-free-space-for-new-frame))
