#!/usr/bin/csi -s

(use cairo)
(include "mathh-constants")

(define difference
  (lambda (A B)
    (cons
      (- (car B) (car A)) 
      (- (cdr B) (cdr A)))))

(define rotate-and-scale-down
  (lambda (A B theta factor)
    (let ((ab (difference A B))) 
      (cons 
        (+ 
          (/ (-
               (* (car ab) (cos theta))
               (* (cdr ab) (sin theta)))
             factor)
          (car A))
        (+ 
          (/ (+
               (* (car ab) (sin theta))
               (* (cdr ab) (cos theta)))
             factor)
          (cdr A))))))

(define rotate
  (lambda (A B theta)
    (let ((ab (difference A B))) 
      (cons 
        (+ 
          (* (car ab) (cos theta))
          (- (* (cdr ab) (sin theta)))
          (car A))
        (+ 
          (* (car ab) (sin theta))
          (* (cdr ab) (cos theta))
          (cdr A))))))

(define create-cairo-context
  (lambda (surface xmax ymax)
    (let ((context (cairo-create surface)))
      (cairo-set-source-rgba context 0 0 0 1)
      (cairo-set-line-width context 1)
      (cairo-new-path context)
      context)))

(define draw-fractal
  (lambda (rule depth filename)
    (let* ((xmax 1600) ; size of image
           (ymax 1200)
           (start (cons 10 (/ ymax 2))) ; start just off the middle of the left border
           (end (cons (- xmax 10) (/ ymax 2))) ; end just off the middle of the right
           ;(s (create-sdl-context xmax ymax))
           (surface (cairo-svg-surface-create filename xmax ymax))
           (context (create-cairo-context surface xmax ymax))
           (rule (lambda (draw-unit) ; bind the move-to argument of rule
                   (rule draw-unit 
                         (lambda (point)
                           (cairo-move-to context (car point) (cdr point)))))))
      (cairo-move-to context (car start) (cdr start))
      ((let layer ((depth depth))
         (if (= depth 0)
           (rule (lambda (current next) ; pass a simple segment-drawer for innermost draw-unit
                   (cairo-line-to context (car next) (cdr next))))
           (rule (layer (- depth 1))))) ; pass rule as the draw-unit argument of rule
         start end)
      (cairo-stroke context) ; update drawn image
      (cairo-surface-flush surface)
      (cairo-surface-finish surface)
      (cairo-surface-destroy surface)
      (cairo-destroy context))))

