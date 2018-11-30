(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:)
     srfi-18
     matchable
     section-combinators)

;; aliases

(define C sdl2:make-color)
(define P sdl2:make-point)
(define R sdl2:make-rect)

;; initialization

(sdl2:set-main-ready!)
(sdl2:init! '(video events))
(ttf:init!)

(on-exit sdl2:quit!)

(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     (sdl2:quit!)
     (original-handler exception))))

;; scene

(define +screen-width+ 800)
(define +screen-height+ 400)

(define +title+ "hexagons")

(define +black+ (C 0 0 0))
(define +white+ (C 255 255 255))

(define hexes
  '(
    ; q r
    (0 0)
    (1 0)
    (0 1)
    (1 1)
    (2 0)
    (2 1)
    (0 2)
    (2 2)
    (4 1)
    (5 3)))

;; math

(define +pi+ 3.141592653589793)
(define +pi/3+ (/ +pi+ 3))
(define +pi/6+ (/ +pi+ 6))

;; window / renderer

(define-values (*window* *renderer*)
  (sdl2:create-window-and-renderer!
   +screen-width+ +screen-height+))

(set! (sdl2:window-title *window*) +title+)

(set! (sdl2:render-viewport *renderer*)
      (R 0 0 +screen-width+ +screen-height+))

(display (sdl2:renderer-info-flags (sdl2:get-renderer-info *renderer*)))
(newline)

;; text

(define *font* (ttf:open-font "DejaVuSansMono.ttf" 20))

;; render

(define (hex-points cx cy radius)
  (map (lambda (n)
         (let* ((angle (- (* +pi/3+ n) +pi/6+))
                (x (round (+ cx (* radius (cos angle)))))
                (y (round (+ cy (* radius (sin angle))))))
           (P x y)))
       (iota 7 0)))

(define (render-coord-text! renderer cx cy coord)
  (let* ((s (string-join (map number->string coord) ","))
         (surface (ttf:render-text-solid *font* s +white+))
         (texture (sdl2:create-texture-from-surface *renderer* surface))
         (w (sdl2:surface-w surface))
         (h (sdl2:surface-h surface))
         (dest-rect (R (round (- cx (/ w 2))) (round (- cy (/ h 2))) w h)))
    (sdl2:render-copy! renderer texture #f dest-rect)))

(define (render-hex! renderer cx cy radius)
  (sdl2:render-draw-lines! renderer (hex-points cx cy radius)))

(define (coord->pixel coord scale)
  (let* ((q (car coord))
         (r (cadr coord))
         (x (* scale (+ (* (sqrt 3) q) (* (/ (sqrt 3) 2) r))))
         (y (* scale (/ 3 2) r)))
    (P (round x) (round y))))

(define (render-hex-coord! renderer coord scale)
  (let* ((point (coord->pixel coord scale))
         (cx (sdl2:point-x point))
         (cy (sdl2:point-y point)))
    (render-hex! renderer cx cy scale)
    (render-coord-text! renderer cx cy coord)))

(define (render-scene! renderer)
  (set! (sdl2:render-draw-color *renderer*) +black+)
  (sdl2:render-clear! *renderer*)

  (set! (sdl2:render-draw-color renderer) +white+)

  (for-each
   (lambda (coord)
     (render-hex-coord! renderer coord 50))
   hexes))

;; event loop

(define (event-loop)
  (let loop ()
    (render-scene! *renderer*)

    (sdl2:render-present! *renderer*)
    (sdl2:delay! 20)
    (thread-yield!)
    (loop)))

(define *event-loop-thread* (make-parameter void))

(begin
  (*event-loop-thread* (thread-start! event-loop)))
