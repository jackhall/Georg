#!/usr/bin/csi -s

(require-extension cairo)
(include "mathh-constants")
(load "fractal.scm")

; dragon
(define dragon-curve
  (lambda (draw-unit move-to)
    (lambda (reference-points)
      (let* ((A (list-ref reference-points 0))
             (C (list-ref reference-points 1))
             (B (rotate-and-scale-down A C PI/4 SQRT2)))
        (draw-unit (list A B))
        (move-to C)
        (draw-unit (list C B))
        (move-to C)))))
      
(draw-fractal dragon-curve 16 segment
              (let ((start (cons (/ xmax 4) (/ ymax 3)))
                    (end (cons (* xmax 7 (/ 8)) (/ ymax 3))))
                (lambda () 
                  (let ((image (standard-frame "dragon-curve.svg" (list start end))))
                    (cairo-move-to (frame-context image) 
                                   (car start) 
                                   (cdr start))
                    image))))


