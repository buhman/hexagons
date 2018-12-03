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

(define (render-hex! renderer cx cy radius color filled?)
  (let* ((points (hexagon-points cx cy radius))
         (tris (intersperse points (P cx cy))))
    (when filled?
      (set! (sdl2:render-draw-color renderer) (sdl2:color-mult color +ultragrey+))
      (sdl2:render-draw-lines! renderer tris))
    (set! (sdl2:render-draw-color renderer) color)
    (sdl2:render-draw-lines! renderer points)))

(define (render-hex-cube! renderer scale cube color filled?)
  (let* ((point (cube->pixel *grip* cube))
         (cx (sdl2:point-x point))
         (cy (sdl2:point-y point)))
    (render-hex! renderer cx cy scale color filled?)
    (render-cube-text! renderer cx cy cube color)))

(define (select-color base-color selected?)
  (let ((grey (if selected? +lightgrey+ +darkgrey+)))
    (sdl2:color-mult grey base-color)))

(define (render-tile! renderer scale selected? filled? tile)
  (let* ((cube (tile-cube tile))
         (base-color (tile-color tile))
         (color (select-color base-color selected?)))
    (render-hex-cube! renderer scale cube color filled?)))

(define (render-tiles! renderer scale)
  (let-values (((bg fg) (partition (lambda (tile)
                                     (not (equal? (tile-cube tile)
                                                  (selector-hover-tile *selector*))))
                                   (map cdr +tiles+))))
    (for-each
     (lambda (tile) (render-tile! renderer scale #f #f tile))
     bg)
    (for-each
     (lambda (tile) (render-tile! renderer scale #t #f tile))
     fg)))

(define (render-linear-path! renderer scale)
  (let ((a (selector-hover-tile *selector*))
        (b (selector-focus-tile *selector*)))
    (when (and a b (not (equal? a b)))
      (set! (sdl2:render-draw-color renderer) (sdl2:color-mult +purple+ +darkgrey+))
      (sdl2:render-draw-lines! renderer (map (lambda (c) (cube->pixel *grip* c)) (cube-line a b))))))

(define (render-flood-path! renderer scale)
  (let* ((a (selector-hover-tile *selector*))
         (b (selector-focus-tile *selector*)))
    (when (and a b (not (equal? a b)))
      (let* ((node-graph (flood-search b +tiles+ tile-neighbors))
             (node-path (flood-path b a node-graph)))
        (when node-path
          (set! (sdl2:render-draw-color renderer) (sdl2:color-mult +green+ +darkgrey+))
          (sdl2:render-draw-lines! renderer
                                   (map (lambda (c) (cube->pixel *grip* c)) node-path)))))))

(define (render-neighbors! renderer scale)
  (let* ((cube (selector-hover-tile *selector*))
         (lines (map (lambda (n) (list (cube->pixel *grip* cube) (cube->pixel *grip* n)))
                     (tile-neighbors cube +tiles+))))
    (set! (sdl2:render-draw-color renderer) (sdl2:color-mult +yellow+ +darkgrey+))
    (for-each
     (lambda (line) (sdl2:render-draw-lines! renderer line))
     lines)))

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
         (point (cube->pixel *grip* cube)))
    (let-values (((x y) (apply values (map (lambda (c) (- c half-side))
                                           (sdl2:point->list point)))))
      (set! (sdl2:render-draw-color renderer) color)
      (sdl2:render-fill-rect! renderer (R x y scale scale)))))

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
