#!/usr/bin/csi -s

(use sdl)
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
  (lambda (draw-unit move-to)
    (let ((rule-unit (sqrt 10))
          (rule-angle (atan (/ 3))))
      (lambda (A B)
        (let* ((a (rotate-and-scale-down A B rule-angle rule-unit))
               (b (rotate-and-scale-down A B (- rule-angle PI/4) rule-unit))
               (c (rotate a b PI/2))
               (d (rotate b a (- PI/2))))
            (draw-unit A a)
            (draw-unit a b)
            (draw-unit b d)
            (draw-unit d B) ; now move back to "a"
            (move-to a)
            (draw-unit a c)
            (draw-unit c d))))))

(define create-sdl-context
  (lambda (maxx maxy)
    (sdl-init SDL_INIT_EVERYTHING)
    (sdl-wm-set-caption "fractal" "fractal")
    (let ((s (sdl-set-video-mode maxx maxy 0 (+ SDL_HWSURFACE
                                                SDL_HWPALETTE
                                                SDL_DOUBLEBUF))))
      (sdl-fill-rect s 
                     (make-sdl-rect 0 0 maxx maxy) 
                     (sdl-map-rgb (sdl-surface-pixel-format s) 0 0 0))
      (sdl-flip s)
      s)))

(define create-cairo-context
  (lambda (s maxx maxy)
    (let ((context (cairo-create 
                     (cairo-image-surface-create-for-data
                       (sdl-surface-pixels s)
                       CAIRO_FORMAT_RGB24 maxx maxy
                       (sdl-surface-pitch s)))))
      (cairo-set-source-rgba context 1 1 1 1)
      (cairo-set-line-width context 1)
      (cairo-new-path context)
      context)))

(define draw-fractal
  (lambda (depth start end)
    (let* ((maxx 640)
           (maxy 480)
           (s (create-sdl-context maxx maxy))
           (context (create-cairo-context s maxx maxy))
           (rule (lambda (draw-unit)
                   (rule draw-unit 
                         (lambda (point)
                           (cairo-move-to context (car point) (cdr point)))))))
      (cairo-move-to context (car start) (cdr start))
      ((let layer ((depth depth))
         (if (= depth 0)
           (rule (lambda (current next)
                   (cairo-line-to context (car next) (cdr next))))
           (rule (layer (- depth 1))))) 
         start end)
      ; update the drawn image
      (cairo-stroke context)
      (sdl-flip s))
    ; loop and wait for the user to close the window
    (let ((event (make-sdl-event)))
      (let loop ()
        (sdl-wait-event! event)
        (let ((t (sdl-event-type event)))
          (if (= t SDL_QUIT)
          'done
          (loop)))))
    (sdl-quit)))

(draw-fractal 2 (cons 10 240) (cons 630 240))

;(define xsize 640)
;(define ysize 480)
;(define s (create-sdl-context xsize ysize))
;(define context (create-cairo-context s xsize ysize))

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

