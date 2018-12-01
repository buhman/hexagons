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

(define (coord->cube coord)
  (let* ((q (coord-q coord))
         (r (coord-r coord))
         (s (- (- q) r)))
    (list q r s)))

(define (cube->coord cube)
  (take cube 2))

(define (distance a b)
  (let* ((cube-a (coord->cube a))
         (cube-b (coord->cube b))
         (dq (abs (- (coord-q cube-a) (coord-q cube-b))))
         (dr (abs (- (coord-r cube-a) (coord-r cube-b))))
         (ds (abs (- (coord-s cube-a) (coord-s cube-b)))))
    (max dq dr ds)))
