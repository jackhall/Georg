#!/usr/bin/csi -s

(require-extension cairo)
(require-extension defstruct)
(include "mathh-constants")

(define difference
  (lambda (A B)
    (cons (- (car B) (car A)) 
          (- (cdr B) (cdr A)))))

(define distance
  (lambda (A B)
    (let ((ab (difference A B)))
      (sqrt (+ (expt (car ab) 2)
               (expt (cdr ab) 2))))))

;(define rotate-and-scale-down
;  (lambda (A B theta factor)
;    (let ((ab (difference A B))) 
;      (cons (+ (/ (- (* (car ab) (cos theta))
;                     (* (cdr ab) (sin theta)))
;                  factor)
;               (car A))
;            (+ (/ (+ (* (car ab) (sin theta))
;                     (* (cdr ab) (cos theta)))
;                  factor)
;               (cdr A))))))

(define rotate
  (lambda (A B theta)
    (let ((ab (difference A B))) 
      (cons (+ (* (car ab) 
                  (cos theta))
               (- (* (cdr ab) 
                     (sin theta)))
               (car A))
            (+ (* (car ab) 
                  (sin theta))
               (* (cdr ab) 
                  (cos theta))
               (cdr A))))))

(define scale-down
  (lambda (A B factor)
    (let ((ab (difference A B)))
      (cons (+ (/ (car ab) 
                  factor)
               (car A))
            (+ (/ (cdr ab)
                  factor)
               (cdr A))))))

(define rotate-and-scale-down
  (lambda (A B theta factor)
    (scale-down A
                (rotate A B theta)
                factor)))

(define create-cairo-context
  (lambda (surface xmax ymax)
    (let ((context (cairo-create surface)))
      (cairo-set-source-rgba context 0 0 0 1)
      (cairo-set-line-width context 1)
      (cairo-new-path context)
      context)))

(define segment
  (lambda (context reference-points)
    (let ((next (list-ref reference-points 1)))
      (cairo-line-to context (car next) (cdr next)))))

(define xmax 1600)
(define ymax 1200)

; a struct describing where to draw the next unit
(defstruct frame context surface points)

; shortcut for creating frames
(define standard-frame 
  (lambda (filename reference-points)
    (let* ((xmax 1600)
           (ymax 1200)
           (surface (cairo-svg-surface-create filename xmax ymax))
           (context (create-cairo-context surface xmax ymax)))
      (make-frame context: context 
                  surface: surface 
                  points: reference-points))))

; draw Lindenmayer systems
(define draw-fractal
  (lambda (rule depth part create-frame)
    (let* ((image (create-frame))
           (move-to (lambda (point)
                      (cairo-move-to (frame-context image) 
                                     (car point) 
                                     (cdr point))))
           (part (lambda (ref-p) ; bind context argument of part
                   (part (frame-context image) ref-p)))
           (rule (lambda (draw-unit) ; bind the move-to argument of rule
                   (rule draw-unit move-to))))
      ((let layer ((depth depth))
               (if (= depth 0)
                 (rule part)
                 (rule (layer (- depth 1))))) ; pass rule as the draw-unit argument of rule
         (frame-points image))
      (cairo-stroke (frame-context image)) ; update drawn image
      (cairo-surface-flush (frame-surface image))
      (cairo-surface-finish (frame-surface image))
      (cairo-surface-destroy (frame-surface image))
      (cairo-destroy (frame-context image)))))

