(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:)
     srfi-18
     srfi-8
     matchable
     section-combinators
     numbers)

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
     ;(sdl2:quit!)
     (original-handler exception))))

;; scene

(define +screen-width+ 800)
(define +screen-height+ 400)

(define +title+ "hexagons")

(define +black+ (C 0 0 0))
(define +white+ (C 255 255 255))
(define +darkgrey+ (C 68 68 68))
(define +lightgrey+ (C 153 153 153))

(define +hexes+
  '(
    ; q r
    (1 0)
    (0 1)
    (1 1)
    (2 0)
    (2 1)
    (0 2)
    (2 2)
    (4 1)
    (5 3)))

;; grip

(define-record-type grip
  (make-grip x y dx dy)
  grip?
  (x grip-x (setter grip-x))
  (y grip-y (setter grip-y))
  (dx grip-dx (setter grip-dx))
  (dy grip-dy (setter grip-dy)))

(define *grip* (make-grip 0 0 0 0))

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

;; model

(define *hover-coord* '(0 0))

;; render

(define (hex-points cx cy radius)
  (map (lambda (n)
         (let* ((angle (- (* +pi/3+ n) +pi/6+))
                (x (round (+ cx (* radius (cos angle)))))
                (y (round (+ cy (* radius (sin angle))))))
           (P x y)))
       (iota 7 0)))

(define (render-coord-text! renderer cx cy coord color)
  (let* ((s (string-join (map number->string coord) ","))
         (surface (ttf:render-text-solid *font* s color))
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
    (P (+ (grip-dx *grip*) (round x))
       (+ (grip-dy *grip*) (round y)))))

(define (pixel->coord point scale)
  (let* ((x (- (sdl2:point-x point) (grip-dx *grip*)))
         (y (- (sdl2:point-y point) (grip-dy *grip*)))
         (q (/ (- (* (/ (sqrt 3) 3) x) (* (/ 1 3) y)) scale))
         (r (/ (* (/ 2 3) y)  scale)))
    (list q r)))

(define (render-hex-coord! renderer coord scale color)
  (let* ((point (coord->pixel coord scale))
         (cx (sdl2:point-x point))
         (cy (sdl2:point-y point)))
    (set! (sdl2:render-draw-color renderer) color)
    (render-hex! renderer cx cy scale)
    (render-coord-text! renderer cx cy coord color)))

(define (render-hexes! renderer scale)
  (let-values (((bg fg)
                (partition (lambda (hex) (not (equal? hex *hover-coord*))) +hexes+)))
    (for-each
     (lambda (hex) (render-hex-coord! renderer hex scale +darkgrey+))
     bg)
    (for-each
     (lambda (hex) (render-hex-coord! renderer hex scale +lightgrey+))
     fg)))

(define (render-scene! renderer)
  (set! (sdl2:render-draw-color *renderer*) +black+)
  (sdl2:render-clear! *renderer*)

  (render-hexes! renderer 50))

;; event handling

(define (handle-event! ev)
  (case (sdl2:event-type ev)
    ((mouse-button-down)
     (case (sdl2:mouse-button-event-button ev)
       ((middle)
        (set! (grip-x *grip*) (sdl2:mouse-button-event-x ev))
        (set! (grip-y *grip*) (sdl2:mouse-button-event-y ev)))))
    ((mouse-motion)
     (match (sdl2:mouse-motion-event-state ev)
       ('(middle)
        (set! (grip-dx *grip*) (+ (grip-dx *grip*)
                                  (- (grip-x *grip*)
                                     (sdl2:mouse-motion-event-x ev))))
        (set! (grip-dy *grip*) (+ (grip-dy *grip*)
                                  (- (grip-y *grip*)
                                     (sdl2:mouse-motion-event-y ev))))
        (set! (grip-x *grip*) (sdl2:mouse-motion-event-x ev))
        (set! (grip-y *grip*) (sdl2:mouse-motion-event-y ev)))
       ('()
        (let* ((point (P (sdl2:mouse-motion-event-x ev) (sdl2:mouse-motion-event-y ev)))
               (coord (pixel->coord point 50))
               (rcoord (map (compose exact round) coord)))
          (set! *hover-coord* rcoord)))
       (x #f)))))

(define (handle-events!)
  (cond
   ((sdl2:has-events?)
    (begin
      (handle-event! (sdl2:poll-event!))
      (handle-events!)))))

;; event loop

(define (event-loop)
  (let loop ()
    (sdl2:pump-events!)
    (handle-events!)

    (render-scene! *renderer*)

    (sdl2:render-present! *renderer*)
    (sdl2:delay! 20)
    (thread-yield!)
    (loop)))

(define *event-loop-thread* (make-parameter #f))

(begin
  (cond
   ((not (eq? (*event-loop-thread*) #f)) (thread-terminate! (*event-loop-thread*))))
  (*event-loop-thread* (thread-start! event-loop)))
