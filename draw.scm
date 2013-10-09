#!/usr/bin/csi -s

; ; ; ezxdisp version
(use ezxdisp)
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
          (* factor
             (-
               (* (car ab) (cos theta))
               (* (cdr ab) (sin theta))))
          (car A))
        (+ 
          (* factor
             (+
               (* (car ab) (sin theta))
               (* (cdr ab) (cos theta))))
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

(define rule
  (lambda (draw-unit)
    (let ((rule-unit (sqrt 10))
          (rule-angle (atan (/ 3))))
      (lambda (A B)
        (let* ((a (rotate-and-scale-down A B rule-angle rule-unit))
               (b (rotate-and-scale-down A B (- rule-angle PI/4) rule-unit)))
               (c (rotate a b PI/2))
               (d (rotate b a (- PI/2)))
            (draw-unit A a)
            (draw-unit a b)
            (draw-unit a c)
            (draw-unit b d)
            (draw-unit c d)
            (draw-unit d B))))))

(define image 
  (ezx-init 640 480 "fractal"))
(ezx-set-background image (make-ezx-color 1 1 1))
(define color
  (make-ezx-color 0 0 0))

(define draw-segment
  (lambda (A B)
    (ezx-line-2d image (car A) (cdr A) (car B) (cdr B) color)))

(define draw-fractal
  (lambda (depth start end)
    ((let layer ((depth depth))
       (if (= depth 0)
         (rule draw-segment)
         (rule (layer (- depth 1))))) 
     start end)))

(draw-fractal 2 (cons 10 240) (cons 630 240))
(ezx-redraw image)
(ezx-next-event image) ; wait for user to close window
(ezx-quit image)


; ; ; simple-graphics version
;(use simple-graphics)
;(include "mathh-constants")
;
;(define rule-unit (sqrt 10))
;(define ^
;  (lambda (x n)
;    (if (= n 1) x
;        (^ (* x x) (- n 1)))))
;
;(define rule
;  (lambda (step)
;    (lambda ()
;      (slower rule-unit)
;      (step)
;      (adventure
;        (right)
;        (step)
;        (left)
;        (step))
;      (step)
;      (right)
;      (step)
;      (left)
;      (step)
;      (faster rule-unit))))
;
;(define single-step (lambda () (forward 1)))
;
;(define draw-fractal 
;  (lambda (depth)
;    (clear)
;    (go-to -300 150)
;    (right)
;    (left (/ (* depth (atan (/ 1 3)) 180) PI))
;    (faster (* rule-unit depth 4))
;    ((let layer ((depth depth))
;       (if (= depth 0) 
;           (rule single-step)
;           (rule (layer (- depth 1))))))))
;
;(draw-fractal 5)
;(save)

; ; ; cairo version
;; extract the last point from a shape
;(define shape-end
;  (lambda (shape)
;    (let follow-branch ((branch shape))
;      (if (list? branch)
;          (let ((next (car branch))
;                (rest (cdr branch)))
;            (if (null? rest) 
;                (if (list? next)
;                    (follow-branch next)
;                    next)
;                (begin
;                  (follow-branch next)
;                  (follow-branch rest))))
;          branch))))
;
;; return a scaled, translated, and rotated version of shape
;(define fit-shape
;  (lambda (shape start end)
;    (let ((end (fit-shape shape)))
;      #f)))

;(define cairo-apply-pair 
;  (lambda (function pair) ; check bounds on pair?
;    (function context (car pair) (cdr pair))))


;(define draw-shape 
;  (lambda (start shape)
;    (cairo-apply-pair cairo-move-to start) 
;    (let draw-branch ((local-start start) (branch shape))
;      (unless (null? branch)
;        (let ((next (car branch))
;              (rest (cdr branch)))
;          (if (list? next)
;              (begin
;                (draw-branch local-start next)
;                (draw-shape local-start rest))
;              (begin
;                (cairo-apply-pair cairo-line-to next)
;                (draw-branch next rest))))))))
;
;(define base
;        (list (cons 20 10) 
;              (list (cons 30 10) 
;                    (cons 30 20)) 
;              (list (cons 20 20) 
;                    (cons 30 20) 
;                    (cons 40 20))))
;(define rule base)

;(draw-shape '(10 . 10) base)
;
;(cairo-stroke context)
;
;(sdl-flip s)
;
;(let ((event (make-sdl-event)))
;  (let loop ()
;    (sdl-wait-event! event)
;    (let ((t (sdl-event-type event)))
;      (if (= t SDL_QUIT)
;      'done
;      (loop)))))
;
;(sdl-quit)
; Represent fractals as trees of line segments.
;
