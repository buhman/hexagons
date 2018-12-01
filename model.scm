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

(define (coord-nearest coord)
  (map (compose exact round) coord))

(define (coord-line a b)
  (let ((dc (coord-distance a b)))
    (map
     (lambda (i) (coord-nearest (coord-lerp a b (* (/ 1 dc) i))))
     (iota (+ 1 dc) 0))))
