(define exact/round (compose inexact->exact round))

(define (points->rays center points-list)
  (map
   (lambda (point)
     (cons center point))
   (flatten points-list)))

(define (obstruction-points grip)
  (let loop ((tiles +tiles+))
    (match tiles
      ((tile . rest)
       (match tile
         ((cube . tile)
          (if (not (tile-pathable? tile))
            (let* ((point (cube->point grip cube))
                   (scale (grip-scale grip))
                   (points (hexagon-points (car point) (cdr point) scale)))
              (cons points (loop rest)))
            (loop rest)))))
      (() '()))))

(define (points-list->edges points-list)
  (flatten (map points->edges points-list)))

(define (edge-center edge)
  (match edge
    (((xa . ya) . (xb . yb))
     (cons (exact/round (/ (+ xa xb) 2))
           (exact/round (/ (+ ya yb) 2))))))

(define (point-distance a b)
  (let ((dx (- (car a) (car b)))
        (dy (- (cdr a) (cdr b))))
    (+ (* dx dx) (* dy dy))))

(define (edge-compare center cmp)
  (lambda (a b)
    (cmp (point-distance center (edge-center a))
         (point-distance center (edge-center b)))))

(define (obstruction-edges center points-list)
  (let ((edges (points-list->edges points-list)))
    (sort edges (edge-compare center <))))

;; line segment intersection
;; quick maffs

(define (line-on-segment? p q r)
  (match (list p q r)
    (((px . py) (qx . qy) (rx . ry))
     (and (<= qx (max px rx))
          (>= qx (min px rx))
          (<= qy (max py ry))
          (>= qy (min py ry))))))

(define (line-orientation p q r)
  (match (list p q r)
    (((px . py) (qx . qy) (rx . ry))
     (let* ((o (- (* (- qy py) (- rx qx))
                  (* (- qx px) (- ry qy)))))
       (cond
        ((= o 0) 'colinear)
        ((< o 0) 'cw)
        ((> o 0) 'ccw))))))

(define (edges-intersect? a b)
  (match (cons a b)
    (((a1 . a2) . (b1 . b2))
     (let ((of (line-orientation a1 a2 b1))
           (og (line-orientation a1 a2 b2))
           (oh (line-orientation b1 b2 a1))
           (oi (line-orientation b1 b2 a2)))
       (or (and (not (eq? of og))
                (not (eq? oh oi)))
           (and (eq? of 'colinear)
                (line-on-segment? a1 b1 a2))
           (and (eq? og 'colinear)
                (line-on-segment? a1 b2 a2))
           (and (eq? oh 'colinear)
                (line-on-segment? b1 a1 b2))
           (and (eq? oi 'colinear)
                (line-on-segment? b1 a2 b2)))))))

;; (ray-intersect-edge ray edge)
(define (ray-intersect-edge ray edge)
  (match (list ray edge)
    ((((ax1 . ay1) . (ax2 . ay2))
      ((bx1 . by1) . (bx2 . by2)))
     (let* ((day (- ay2 ay1))
            (dax (- ax1 ax2))
            (ac (+ (* day ax1)
                   (* dax ay1)))
            (dby (- by2 by1))
            (dbx (- bx1 bx2))
            (bc (+ (* dby bx1)
                   (* dbx by1)))
            (det (- (* day dbx)
                    (* dby dax))))
       (if (= 0 det)
         ;; rays are from center (ax1 . ay1) to point (ax2 . ay2)
         (cons ax2 ay2)
         (cons (exact/round (/ (- (* dbx ac)
                                  (* dax bc)) det))
               (exact/round (/ (- (* day bc)
                                  (* dby ac)) det))))))))

;; rays to each corner
;; sort each edge by distance (center of edge to point)
;; for each sorted edge, find if ray intersects, stop at first intersection
;; collect triangles

(define (ray-cast ray edges)
  (let loop ((es edges))
    (match es
      ((edge . rest)
       (if (edges-intersect? ray edge)
         (cons (car ray) (ray-intersect-edge ray edge))
         (loop rest)))
      (() #f))))

(define edge1 '((100 . 100) . (200 . 200)))
(define edge2 '((100 . 200) . (200 . 100)))
(define edge3 '((400 . 400) . (550 . 500)))

(define (render-lighting! renderer grip)
  (let* ((points-list (obstruction-points grip))
         (center *mouse*)
         (edges (obstruction-edges center points-list))
         (rays (points->rays center points-list)))

    (let ((r2 (map (lambda (r) (ray-cast r edges)) rays)))
      (set! (sdl2:render-draw-color renderer) (multiply-alpha +yellow+ 64))
      (render-draw-edges! renderer rays)
      (set! (sdl2:render-draw-color renderer) (multiply-alpha +green+ 64))
      (render-draw-edges! renderer r2))))
