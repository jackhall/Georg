#!/usr/bin/csi -s

(use cairo)
(include "mathh-constants")
(load "fractal.scm")

; working
(define square-tail
  (lambda (draw-unit move-to)
    (let ((rule-unit (sqrt (+ (expt 3 2) (expt 1 2))))
          (rule-angle (- (atan (/ 1 3)))))
      (lambda (A B)
        (let* ((a (rotate-and-scale-down A B rule-angle rule-unit))
               (b (rotate-and-scale-down A B (+ rule-angle PI/4) (/ rule-unit (sqrt 2))))
               (c (rotate a b (- PI/2)))
               (d (rotate b a PI/2)))
            (draw-unit A a)
            (draw-unit a b)
            (draw-unit b d)
            (draw-unit d B) 
            (move-to a)
            (draw-unit a c)
            (draw-unit c d)
            (move-to B))))))

; not working
(define slant-tail
  (lambda (draw-unit move-to)
    (let ((rule-unit (sqrt (+ (expt 2.5 2) (expt (/ SQRT3 2) 2))))
          (rule-angle (- (atan (/ (/ SQRT3 2) 2.5)))))
      (lambda (A B)
        (let* ((b (rotate-and-scale-down A B (+ rule-angle (/ PI 3)) rule-unit))
               (a (rotate b A (/ PI 3)))
               (c (rotate a b (- (* 2 PI (/ 3)))))
               (d (rotate b a (/ PI 3))))
            (draw-unit A a)
            (draw-unit a b)
            (draw-unit b d)
            (draw-unit d B) 
            (move-to a)
            (draw-unit a c)
            (draw-unit c d)
            (move-to B))))))

(define spiral
  (lambda (draw-unit move-to)
    (let ((rule-unit SQRT3)
          (rule-angle (- (atan (/ SQRT3)))))
      (lambda (A B)
        (let ((C (rotate-and-scale-down A B rule-angle rule-unit)))
          (draw-unit A C)
          (draw-unit C B))))))

(draw-fractal square-tail 4 "/home/jack/fractal.svg")

