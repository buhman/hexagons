(include "geometry.scm")

;; render

(define (render-coord-text! renderer cx cy coord color)
  (let* ((s (string-join (map number->string coord) ","))
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

(define (render-hex-coord! renderer scale coord color filled?)
  (let* ((point (coord->pixel *grip* coord))
         (cx (sdl2:point-x point))
         (cy (sdl2:point-y point)))
    (render-hex! renderer cx cy scale color filled?)
    (render-coord-text! renderer cx cy coord color)))

(define (select-color base-color selected?)
  (let ((grey (if selected? +lightgrey+ +darkgrey+)))
    (sdl2:color-mult grey base-color)))

(define (render-tile! renderer scale selected? filled? tile)
  (let* ((coord (tile-coord tile))
         (base-color (tile-color tile))
         (color (select-color base-color selected?)))
    (render-hex-coord! renderer scale coord color filled?)))

(define (render-tiles! renderer scale)
  (let-values (((bg fg) (partition (lambda (tile)
                                     (not (equal? (tile-coord tile)
                                                  (selector-hover-tile *selector*))))
                                   +tiles+)))
    (for-each
     (lambda (tile) (render-tile! renderer scale #f #f tile))
     bg)
    (for-each
     (lambda (tile) (render-tile! renderer scale #t #f tile))
     fg)))

(define (render-path! renderer scale)
  (let ((a (selector-hover-tile *selector*))
        (b (selector-focus-tile *selector*)))
    (when (not (equal? (cube->coord a) (cube->coord b)))
      (set! (sdl2:render-draw-color renderer) (sdl2:color-mult +purple+ +darkgrey+))
      (sdl2:render-draw-lines! renderer (map (lambda (c) (coord->pixel *grip* c)) (coord-line a b))))))

(define (render-scene! renderer)
  (set! (sdl2:render-draw-color *renderer*) +black+)
  (sdl2:render-clear! *renderer*)

  (render-tiles! renderer (grip-scale *grip*))
  (render-path! renderer (grip-scale *grip*)))
