;; synchronized: game state

(define-record-type game-state
  (make-state tile-map tile-kd tokens port)
  state?
  ;; hash-table: cube -> tile
  (tile-map state-tile-map (setter state-tile-map))
  ;; kd-tree: axial/2-vector
  (tile-kd state-tile-kd (setter state-tile-kd))
  ;; alist: '((cube . token) ...)
  (tokens state-tokens (setter state-tokens))
  ;; PORT or #f
  (port state-port (setter state-port)))

;; unsynchronized: editor state

(define-record-type editor-state
  (make-editor mode tile-mode token-color)
  editor?
  ;; 'tile 'token
  (mode editor-mode (setter editor-mode))
  ;; alist of: 'pathable 'visible
  (tile-mode editor-tile-mode (setter editor-tile-mode))
  ;; symbol
  (token-color editor-token-color (setter editor-token-color)))

;; tile

(define-record-type tile
  (make-tile cube color pathable visible)
  tile?
  (cube tile-cube)
  (color tile-color)
  (pathable tile-pathable?)
  (visible tile-visible?))

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

;; token

(define-record-type token
  (make-token cube id color)
  token?
  (cube token-cube (setter token-cube))
  (id token-id)
  (color token-color))

;; qr(s) axial coordinate system

(define cube-q car)
(define cube-r cadr)
(define cube-s caddr)

(define +axes+ (list cube-q cube-r cube-s))

(define (axis-sub x y)
  (- (- x) y))

(define (cube->point grip cube)
  (let* ((scale (grip-scale grip))
         (q (cube-q cube))
         (r (cube-r cube))
         (x (* scale (+ (* (sqrt 3) q) (* (/ (sqrt 3) 2) r))))
         (y (* scale (/ 3 2) r)))
    (cons (+ (grip-dx grip) (inexact->exact (round x)))
          (+ (grip-dy grip) (inexact->exact (round y))))))

(define (point->cube grip point)
  (let* ((scale (grip-scale grip))
         (x (- (car point) (grip-dx grip)))
         (y (- (cdr point) (grip-dy grip)))
         (q (/ (- (* (/ (sqrt 3) 3) x) (* (/ 1 3) y)) scale))
         (r (/ (* (/ 2 3) y) scale))
         (s (axis-sub q r)))
    (list q r s)))

(define (axial->cube axial)
  (let* ((q (cube-q axial))
         (r (cube-r axial))
         (s (axis-sub q r)))
    (list q r s)))

(define (axial-skew q r)
  (- q (/ r -2)))

(define (rect-skew x r)
  (+ x (/ r -2)))

(define (cube->rect-vector cube)
  (let* ((q (cube-q cube))
         (r (cube-r cube))
         (x (axial-skew q r)))
    (vector x r)))

(define (rect-vector->cube a-vec)
  (let* ((x (vector-ref a-vec 0))
         (r (vector-ref a-vec 1))
         ;; hack?
         (q (exact/round (rect-skew x r)))
         (s (axis-sub q r)))
    (list q r s)))

(define (cube-distance a b)
  (apply max (map (lambda (axis) (abs (- (axis a) (axis b))))
                  +axes+)))

(define (lerp a b t)
  (+ a (* t (- b a))))

(define (cube-lerp a b t)
  (map (lambda (axis) (lerp (axis a) (axis b) t))
       +axes+))

(define (cube-nearest cube)
  (let* ((relt (map (compose inexact->exact round) cube))
         (delt (map (lambda (r c) (abs (- r c))) relt cube)))
    (let-values (((q r s) (apply values relt))
                 ((dq dr ds) (apply values delt)))
      (if (< dq dr)
        (if (< dr ds)
          (list q r (axis-sub q r))
          (list q (axis-sub q s) s))
        (if (< dq ds)
          (list q r (axis-sub q r))
          (list (axis-sub r s) r s))))))

(define (cube-line a b)
  (let ((dc (cube-distance a b)))
    (map
     (lambda (i) (cube-nearest (cube-lerp a b (* (/ 1 dc) i))))
     (iota (+ 1 dc) 0))))

(define cube-directions
  '(( 1 -1  0)
    ( 1  0 -1)
    ( 0  1 -1)
    (-1  1  0)
    (-1  0  1)
    ( 0 -1  1)))

(define (cube-add a b)
  (map + a b))

;; tile

(define (tile-neighbors cube t-map)
  (let ((candidates (map (lambda (dir) (cube-add dir cube)) cube-directions)))
    (filter (lambda (c)
              (let ((tile (hash-table-ref/default t-map c #f)))
                (and tile (tile-pathable? tile))))
            candidates)))

;; path

(define (cube-path-lerp path t)
  (let* ((steps (length path))
         (step (* t (sub1 steps)))
         (fstep (inexact->exact (floor step)))
         (a (list-ref path fstep))
         (b (list-ref path (add1 fstep))))
    (cube-lerp a b (- step fstep))))

;;

(define (point-rect->axial-vec-range grip a b)
  (let ((convert (lambda (p) (cube->rect-vector (point->cube grip p)))))
    (values (convert a) (convert b))))

(define (viewport-point-range renderer)
  (let-values (((x y) (sdl2:renderer-output-size renderer)))
    (values (cons 0 0) (cons x y))))
