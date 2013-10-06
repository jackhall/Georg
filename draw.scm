#!/usr/bin/csi -s

(use simple-graphics)
(include "mathh-constants")

(define rule-unit (sqrt 10))
(define ^
  (lambda (x n)
    (if (= n 1) x
        (^ (* x x) (- n 1)))))

(define rule
  (lambda (step)
    (lambda ()
      (slower rule-unit)
      (step)
      (adventure
        (right)
        (step)
        (left)
        (step))
      (step)
      (right)
      (step)
      (left)
      (step)
      (faster rule-unit))))

(define single-step (lambda () (forward 1)))

(define draw-fractal 
  (lambda (depth)
    (clear)
    (go-to -300 150)
    (right)
    (left (/ (* depth (atan (/ 1 3)) 180) PI))
    (faster (* rule-unit depth 4))
    ((let layer ((depth depth))
       (if (= depth 0) 
           (rule single-step)
           (rule (layer (- depth 1))))))))

(draw-fractal 5)
(save)


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
