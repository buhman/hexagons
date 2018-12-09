(use matchable
     section-combinators)

;; k-d-tree

(define-record-type kd-tree
  (make-kd point left right)
  kd-tree?
  (point kd-point)
  ;; a kd-tree or a list of points
  (left kd-left (setter kd-left))
  (right kd-right (setter kd-right)))

(define-record-printer (kd-tree kd out)
  (fprintf out "#,(kd-tree ~s ~s ~s)" (kd-point kd) (kd-left kd) (kd-right kd)))

(define (ref-less? ref)
  (lambda (a b)
    (< (ref a) (ref b))))

(define (split-points ps axis)
  (let* ((ref (lambda (p) (vector-ref p axis)))
         (sorted-ps (sort ps (ref-less? ref)))
         (mid-pos (quotient (length sorted-ps) 2)))
    (let-values (((left right) (split-at sorted-ps mid-pos)))
      ;; the first element of right is the median
      (values (car right) left (cdr right)))))

(define (make-balanced-kd k points)
  (let go ((axis 0)
           (ps points))
    (match ps
      (() #f)
      (_
       (let-values (((point left right) (split-points ps axis)))
         (let ((next-axis (modulo (add1 axis) k)))
           (make-kd point (go next-axis left) (go next-axis right))))))))

(define (point-extremes axis a b less?)
  (let* ((ref (lambda (p) (vector-ref p axis)))
         (av (ref a))
         (bv (ref b)))
    (if (less? av bv)
      (values av bv)
      (values bv av))))

(define (point-axis-inside? axis point a b)
  (let ((ref (lambda (p) (vector-ref p axis))))
    (<= (ref a) (ref point) (ref b))))

(define (point-inside? k point a b)
  (let ((inside? (right-section point-axis-inside? point a b)))
    (every inside? (iota k))))

(define (kd-select-range k tree a b #!optional (visited void))
  (let go ((axis 0)
           (t tree))
    (match t
      (#f '())
      (($ kd-tree point left right)
       (visited)
       (let-values (((p) (vector-ref point axis))
                    ((l h) (point-extremes axis a b <)))
         (let* ((next-axis (modulo (add1 axis) k))
                (rest (cond
                       ((>= p h) (go next-axis left))
                       ((<= p l) (go next-axis right))
                       (else (append
                              (go next-axis left)
                              (go next-axis right))))))
           (if (point-inside? k point a b)
             (cons point rest)
             rest)))))))
