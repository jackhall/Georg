#!/usr/bin/csi -s

(require-extension cairo)
(include "mathh-constants")
(load "fractal.scm")

; square-tail
(define square-tail
  (lambda (draw-unit move-to)
    (let ((rule-unit (sqrt (+ (expt 3 2) (expt 1 2))))
          (rule-angle (- (atan (/ 1 3)))))
      (lambda (reference-points)
        (let ((A (list-ref reference-points 0))
              (B (list-ref reference-points 1)))
          (let* ((a (rotate-and-scale-down A B rule-angle rule-unit))
                 (b (rotate-and-scale-down A B (+ rule-angle PI/4) (/ rule-unit (sqrt 2))))
                 (c (rotate a b (- PI/2)))
                 (d (rotate b a PI/2)))
            (draw-unit (list A a))
            (draw-unit (list a b))
            (draw-unit (list b d))
            (draw-unit (list d B))
            (move-to a)
            (draw-unit (list a c))
            (draw-unit (list c d))
            (move-to B)))))))

(draw-fractal square-tail 4 segment
              (let ((start (cons 10 (/ ymax 2)))
                    (end (cons (- xmax 10) (/ ymax 2))))
                (lambda () 
                  (let ((image (standard-frame "square-tail.svg" (list start end))))
                    (cairo-move-to (frame-context image) 
                                   (car start) 
                                   (cdr start))
                    image))))


