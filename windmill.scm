#!/usr/bin/csi -s

(require-extension cairo)
(include "mathh-constants")
(load "fractal.scm")

; windmill - doesn't quite fit together yet
(define bisection
  (lambda (objective A B)
    (let* ((a (objective A))
           (b (objective B))
           (C (/ (+ B A) 2))
           (c (objective C)))
      (cond ((> (* a b) 0) #f)
            ((< (- B A) 0.0000000001) C)
            ((> (* c a) 0) (bisection objective C B))
            ((> (* c b) 0) (bisection objective A C))
            ((= c 0) C)
            (else #f)))))

(define windmill-rule-angle (/ PI 18))
(define windmill-gamma 
  (bisection 
    (lambda (gamma)
      (- (* (sin (/ PI 3))
            (+ (/ (sin (- (/ (* PI 2) 3) 
                          windmill-rule-angle)))
               (/ (sin (- (/ (* PI 2) 3)
                          windmill-rule-angle
                          gamma))
                  (* (sin (+ (/ PI 3) 
                             windmill-rule-angle))
                     (sin (+ (/ PI 3)
                             windmill-rule-angle
                             gamma))))))
         (/ (sin (- PI
                    windmill-rule-angle
                    gamma))
            (sin gamma))))
    0.0001 1)) ; starting interval (radians)
(define windmill-rule-unit
  (/ (sin (- PI windmill-rule-angle windmill-gamma)) ; compute rule-unit from gamma
     (sin windmill-gamma)))
(define windmill-small-side
  (let ((beta (- (/ PI 3) windmill-rule-angle windmill-gamma)))
    (* windmill-rule-unit
       (/ (sin beta))
       (sin (- (* 2 (/ PI 3)) beta)))))

(define open-triangle
  (lambda (context reference-points)
    (let ((A (list-ref reference-points 0))
          (B (list-ref reference-points 1))
          (c (list-ref reference-points 2)))
      (segment context (list A B))
      (segment context (list B c)))))

(define windmill
  (lambda (draw-unit move-to)
    (lambda (reference-points)
      (let ((A (list-ref reference-points 0))
            (B (list-ref reference-points 1)))
        (let* ((C (rotate A B (/ PI 3)))
               (a (rotate-and-scale-down A B windmill-rule-angle windmill-rule-unit))
               (b (rotate-and-scale-down B C windmill-rule-angle windmill-rule-unit))
               (c (rotate-and-scale-down C A windmill-rule-angle windmill-rule-unit))
               (e (rotate A a (/ PI 3)))
               (f (rotate B b (/ PI 3)))
               (g (rotate C c (/ PI 3))))
            (move-to A)
            (draw-unit (list A a c))
            (move-to B)
            (draw-unit (list B b a))
            (move-to C)
            (draw-unit (list C c b)))))))

(draw-fractal windmill 8 open-triangle
              (let ((A (cons (/ xmax 2) 
                             10))
                    (B (cons (+ (/ xmax 2) 
                                (/ (- ymax 210)
                                   SQRT3))
                             (- ymax 200))))
                (lambda ()
                  (standard-frame "windmill.svg" 
                                  (list A B (rotate-and-scale-down B 
                                                                   A 
                                                                   (- (/ PI 3)) 
                                                                   windmill-small-side))))))


; not working
;(define slant-tail
;  (lambda (draw-unit move-to)
;    (let ((rule-unit (sqrt (+ (expt 2.5 2) (expt (/ SQRT3 2) 2))))
;          (rule-angle (- (atan (/ (/ SQRT3 2) 2.5)))))
;      (lambda (A B)
;        (let* ((b (rotate-and-scale-down A B (+ rule-angle (/ PI 3)) rule-unit))
;               (a (rotate b A (/ PI 3)))
;               (c (rotate a b (- (* 2 PI (/ 3)))))
;               (d (rotate b a (/ PI 3))))
;            (draw-unit A a)
;            (draw-unit a b)
;            (draw-unit b d)
;            (draw-unit d B) 
;            (move-to a)
;            (draw-unit a c)
;            (draw-unit c d)
;            (move-to B))))))
;
;(define spiral
;  (lambda (draw-unit move-to)
;    (let ((rule-unit SQRT3)
;          (rule-angle (- (atan (/ SQRT3)))))
;      (lambda (A B)
;        (let ((C (rotate-and-scale-down A B rule-angle rule-unit)))
;          (draw-unit A C)
;          (draw-unit C B))))))

