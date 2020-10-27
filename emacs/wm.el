(provide 'wm)

(defun close-window-then-frame ()
  (interactive)
  (cond ((< 1 (length (window-list))) (delete-window))
        ((< 1 (length (frame-list))) (delete-frame))
        (t (user-error "TODO: keep server running"))))

(defun make-frame-at-center ()
  (interactive)
  (let ((displays (display-monitor-attributes-list)))
    (cond ((= 1 (length displays))
           (let* ((workarea (alist-get 'workarea (car displays)))
                  (width-padding (lambda (w) (/ w 3)))
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



(global-set-key [C-f11] 'ns-do-hide-others)

(global-set-key (kbd "s-w") 'close-window-then-frame)
(global-set-key (kbd "s-n") 'make-frame-at-center)

(global-set-key (kbd "s-1") (lambda () (interactive) (select-nth-visible-frame 1)))
(global-set-key (kbd "s-2") (lambda () (interactive) (select-nth-visible-frame 2)))
(global-set-key (kbd "s-3") (lambda () (interactive) (select-nth-visible-frame 3)))



(setq initial-frame-alist
      '((left . 1300) (top . 130) (width . 120) (height . 70)))
