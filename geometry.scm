;; math

(define +pi+ 3.141592653589793)
(define +pi/3+ (/ +pi+ 3))
(define +pi/6+ (/ +pi+ 6))

;; hexagon

(define (hexagon-points cx cy radius)
  (map (lambda (n)
         (let* ((angle (- (* +pi/3+ n) +pi/6+))
                (x (round (+ cx (* radius (cos angle)))))
                (y (round (+ cy (* radius (sin angle))))))
           (P x y)))
       (iota 7 0)))

(define (hexagon-pts cx cy radius)
  (map (lambda (n)
         (let* ((angle (- (* +pi/3+ n) +pi/6+))
                (x (round (+ cx (* radius (cos angle)))))
                (y (round (+ cy (* radius (sin angle))))))
           (list x y)))
       (iota 6 0)))

;; line

(define (line-lerp-y->x a b y)
  (let ((ax (car a))
        (ay (cadr a))
        (bx (car b))
        (by (cadr b)))
    (let ((t (/ (- y ay) (- by ay))))
      (exact (floor (lerp ax bx t))))))

;; edges

(define (points->edges points)
  (let loop ((pts points))
    (match pts
      ((a . (b . rest))
       (cons (list a b) (loop (cons b rest))))
      ((a . '())
       (list (list a (car points))))
      (() '()))))

(define (sort-pair pair)
  (match pair
   ((a . b) (if (< a b) (cons a b) (cons b a)))))

(define (sort-edge ep)
  (match ep
    ((a b)
     (if (< (cadr a) (cadr b))
       (list a b)
       (list b a)))))

(define (edges-y-intercepts edges y max-y)
  (let loop ((edges edges))
    (match edges
      ((edge . rest)
       (match (sort-edge edge)
         (((xa ya) (xb yb))
          (if (or (and (>= y ya) (< y yb))
                  (or (= y ya max-y) (= y yb max-y)))
            (cons (line-lerp-y->x (list xa ya) (list xb yb) y)
                  (loop rest))
            (loop rest)))))
      (() '()))))

(define (edge-y-values edges)
  (let loop ((edges edges))
    (match edges
      ((edge . rest)
       ;; not sure what value a nested edge structure is providing at this point
       (cons (cadar edge)
             (cons (cadadr edge)
                   (loop rest))))
      (() '()))))

(define (edge-unique-y-values edges)
  (let ((y-vals (sort (edge-y-values edges) <)))
    (let loop ((ys y-vals)
               (last-y #f))
      (match ys
        ((y . rest)
         (if (and last-y (= last-y y))
           (loop rest last-y)
           (cons y (loop rest y))))
        (() '())))))

;; trapezoid

(define (make-trapezoid xa0 xb0 y0 xa1 xb1 y1)
  (match (cons (sort-pair (cons xa0 xb0))
               (sort-pair (cons xa1 xb1)))
    (((xal . xar) . (xbl . xbr))
     `((,xal ,y0) (,xar ,y0)
       (,xbl ,y1) (,xbr ,y1)))))

;; get unique y-values
;; intercept all lines in range
;; produce trapezoid from xa xb y, xa-1 xb-1 y-1

(define (trapezoid-decompose-y edges y max-y)
  (let ((int-xs (edges-y-intercepts edges y max-y)))
    ;; if this doesn't match, the polygon is concave or not a polygon
    ;; or, possibly has horizontal edges?
    (match int-xs
      ((x) (list x x y))
      ((xa xb) (list xa xb y)))))

(define (trapezoid-decompose edges)
  (let* ((y-values (edge-unique-y-values edges))
         (max-y (last y-values)))
    (let loop ((ys y-values)
               (last-ht #f))
      (match ys
        ((y . rest)
         (let ((ht (trapezoid-decompose-y edges y max-y)))
           (if last-ht
             (cons (apply make-trapezoid `(,@last-ht ,@ht)) (loop rest ht))
             (loop rest ht))))
        (() '())))))

;; hexagon-trapezoids

(define (hexagon-trapezoids scale)
  (let* ((points (hexagon-pts 0 0 scale))
         (edges (points->edges points))
         (traps (trapezoid-decompose edges)))
    traps))

(define (translate-trapezoid trap dx dy)
  (map
   (lambda (pair)
     (match pair
       ((x y) (list (+ dx x) (+ dy y)))))
   trap))

(define (translate-trapezoids traps dx dy)
  (map
   (lambda (trap)
     (translate-trapezoid trap dx dy))
   traps))
