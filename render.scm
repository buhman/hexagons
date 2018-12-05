(include "geometry.scm")

;; render

(define (render-cube-text! renderer cx cy cube color)
  (let* ((s (string-join (map number->string (take cube 2)) ","))
         (surface (ttf:render-text-solid *font* s color))
         (texture (sdl2:create-texture-from-surface *renderer* surface))
         (w (sdl2:surface-w surface))
         (h (sdl2:surface-h surface))
         (dest-rect (R (round (- cx (/ w 2))) (round (- cy (/ h 2))) w h)))
    (sdl2:render-copy! renderer texture #f dest-rect)))

(define (render-hex! renderer cx cy radius color)
  (let* ((points (hexagon-points cx cy radius)))
    (set! (sdl2:render-draw-color renderer) color)
    (render-draw-lines! renderer points)))

(define (render-traps! renderer traps dx dy color)
  (let ((ts (translate-trapezoids traps dx dy)))
    (set! (sdl2:render-draw-color renderer) color)
    (for-each
     (lambda (t)
       (render-draw-trapezoid! renderer t))
     ts)))

(define (render-hex-cube! renderer scale cube color ht)
  (let ((point (cube->point *grip* cube)))
    (match point
      ((cx . cy)
       (render-traps! renderer ht cx cy (sdl2:color-mult color +ultragrey+))
       (render-hex! renderer cx cy scale color)
       (render-cube-text! renderer cx cy cube +white+)))))

(define (select-color base-color selected?)
  (let ((grey (if selected? +lightgrey+ +darkgrey+)))
    (sdl2:color-mult grey base-color)))

(define (render-tile! renderer scale selected? tile ht)
  (let* ((cube (tile-cube tile))
         (base-color (tile-color tile))
         (color (select-color base-color selected?)))
    (render-hex-cube! renderer scale cube color ht)))

(define (render-tiles! renderer scale)
  (let-values (((bg fg) (partition (lambda (tile)
                                     (not (equal? (tile-cube tile)
                                                  (selector-hover-tile *selector*))))
                                   (map cdr +tiles+)))
               ((ht) (hexagon-trapezoids scale)))
    (for-each
     (lambda (tile) (render-tile! renderer scale #f tile ht))
     bg)
    (for-each
     (lambda (tile) (render-tile! renderer scale #t tile ht))
     fg)))

(define (render-linear-path! renderer scale)
  (let ((a (selector-hover-tile *selector*))
        (b (selector-focus-tile *selector*)))
    (when (and a b (not (equal? a b)))
      (set! (sdl2:render-draw-color renderer) (sdl2:color-mult +purple+ +darkgrey+))
      (render-draw-lines! renderer (map (lambda (c) (cube->point *grip* c)) (cube-line a b))))))

(define (render-flood-path! renderer scale)
  (let* ((a (selector-focus-tile *selector*))
         (b (selector-hover-tile *selector*)))
    (when (and a b (not (equal? a b)))
      (let* ((node-graph (flood-search a +tiles+ tile-neighbors))
             (node-path (flood-path a b node-graph)))
        (when node-path
          (set! (sdl2:render-draw-color renderer) (sdl2:color-mult +green+ +darkgrey+))
          (render-draw-lines! renderer (map (lambda (c) (cube->point *grip* c)) node-path)))))))

(define (render-neighbors! renderer scale)
  (let* ((cube (selector-hover-tile *selector*))
         (ll (map (lambda (n) (list (cube->point *grip* cube) (cube->point *grip* n)))
                  (tile-neighbors cube +tiles+))))
    (set! (sdl2:render-draw-color renderer) (sdl2:color-mult +yellow+ +darkgrey+))
    (for-each
     (lambda (lines) (render-draw-lines! renderer lines))
     ll)))

;; tokens

(define (multiply-alpha color alpha)
  (let-values (((r g b a) (sdl2:color->values color)))
    (C r g b alpha)))

(define (render-token! renderer scale token)
  (let* ((cube (token-cube token))
         (base-color (token-color token))
         (alpha (if (equal? (selector-focus-tile *selector*) cube) 128 64))
         (color (multiply-alpha base-color alpha))
         (half-side (floor (/ scale 2)))
         (point (cube->point *grip* cube)))
    (match point
      ((x . y)
       (set! (sdl2:render-draw-color renderer) color)
       (sdl2:render-fill-rect! renderer (R (- x half-side) (- y half-side) scale scale))))))

(define (render-tokens! renderer scale)
  (for-each
   (lambda (token) (render-token! renderer scale token))
   (map cdr *tokens*)))

;; scene

(define (render-scene! renderer)
  (set! (sdl2:render-draw-color *renderer*) +black+)
  (set! (sdl2:render-draw-blend-mode *renderer*) 'blend)
  (sdl2:render-clear! *renderer*)

  (let ((scale (grip-scale *grip*)))
    (render-tiles! renderer scale)
    (render-neighbors! renderer scale)
    (render-linear-path! renderer scale)
    (render-flood-path! renderer scale)

    (render-tokens! renderer scale)))

;; fps

(define (render-fps! renderer dt)
  (let* (;(fps (round (* (/ 1 dt) 1000)))
         (surface (ttf:render-text-solid *font* (number->string dt) +white+))
         (texture (sdl2:create-texture-from-surface *renderer* surface))
         (w (sdl2:texture-w texture))
         (h (sdl2:texture-h texture)))
    (sdl2:render-copy! renderer texture #f (R 0 0 w h))))
