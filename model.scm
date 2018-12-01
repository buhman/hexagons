;; tile

(define-record-type tile
  (make-tile coord color pathable)
  tile?
  (coord tile-coord)
  (color tile-color)
  (pathable tile-pathable?))

;; grip

(define-record-type grip
  (make-grip x y dx dy scale)
  grip?
  (x grip-x (setter grip-x))
  (y grip-y (setter grip-y))
  (dx grip-dx (setter grip-dx))
  (dy grip-dy (setter grip-dy))
  (scale grip-scale (setter grip-scale)))

;;

(define-record-type selector
  (make-selector hover-tile focus-tile)
  selector?
  (hover-tile selector-hover-tile (setter selector-hover-tile))
  (focus-tile selector-focus-tile (setter selector-focus-tile)))

;; qr(s) axial coordinate system

(define (coord->pixel grip coord)
  (let* ((scale (grip-scale grip))
         (q (car coord))
         (r (cadr coord))
         (x (* scale (+ (* (sqrt 3) q) (* (/ (sqrt 3) 2) r))))
         (y (* scale (/ 3 2) r)))
    (P (+ (grip-dx grip) (round x))
       (+ (grip-dy grip) (round y)))))

(define (pixel->coord grip point)
  (let* ((scale (grip-scale grip))
         (x (- (sdl2:point-x point) (grip-dx grip)))
         (y (- (sdl2:point-y point) (grip-dy grip)))
         (q (/ (- (* (/ (sqrt 3) 3) x) (* (/ 1 3) y)) scale))
         (r (/ (* (/ 2 3) y)  scale)))
    (list q r)))

(define coord-q car)
(define coord-r cadr)
(define coord-s caddr)

(define +axes+ (list coord-q coord-r coord-s))

(define (coord->cube coord)
  (let* ((q (coord-q coord))
         (r (coord-r coord))
         (s (- (- q) r)))
    (list q r s)))

(define (cube->coord cube)
  (take cube 2))

(define (coord-distance a b)
  (let ((cube-a (coord->cube a))
        (cube-b (coord->cube b)))
    (apply max (map (lambda (axis) (abs (- (axis cube-a) (axis cube-b))))
                    +axes+))))

(define (lerp a b t)
  (+ a (* t (- b a))))

(define (coord-lerp a b t)
  (let ((cube-a (coord->cube a))
        (cube-b (coord->cube b)))
    (map (lambda (axis) (lerp (axis cube-a) (axis cube-b) t))
         +axes+)))

(define (reset-axis delta)
  (if (and (> (coord-q delta) (coord-r delta))
           (> (coord-q delta) (coord-s delta)))
    coord-q
    (if (> (coord-r delta) (coord-s delta))
      coord-r
      coord-s)))

;; I can't say I've ever written worse code
(define (coord-nearest coord)
  (let* ((cube (coord->cube coord))
         (round-cube (map (compose exact round) cube))
         (delta (map (lambda (x rx) (abs (- rx x))) cube round-cube))
         (fix-axis (reset-axis delta))
         (other-axes (filter (lambda (i) (not (eq? fix-axis i))) +axes+)))
    (set! (fix-axis round-cube)
          (apply (lambda (ax bx) (- (- (ax round-cube)) (bx round-cube))) other-axes))
    round-cube))

(define (coord-line a b)
  (let ((dc (coord-distance a b)))
    (map
     (lambda (i) (coord-nearest (coord-lerp a b (* (/ 1 dc) i))))
     (iota (+ 1 dc) 0))))
