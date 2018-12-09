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

(define (make-balanced-kd k points #!optional (root-axis 0))
  (let go ((axis root-axis)
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

(define (tree->list tree)
  (match tree
    (#f '())
    (($ kd-tree point left right)

     (cons point (append (tree->list left)
                         (tree->list right))))))

(define (kd-find k tree target #!optional (visited void))
  (let go ((axis 0)
           (last #f)
           (t tree))
    (match t
      (#f (values #f last #f #f))
      (($ kd-tree point left right)
       (visited)
       (cond
        ((equal? point target) (values #t t last axis))
        (else
         (let* ((p (vector-ref point axis))
                (o (vector-ref target axis))
                (next-axis (modulo (add1 axis) k)))
           (cond
            ((> p o) (go next-axis t left))
            ((< p o) (go next-axis t right))
            (else (or (go next-axis t left)
                      (go next-axis t right)))))))))))

;; form the set of all nodes and leaves from the children of the target node,
;; and recreate that part of the tree.
(define (kd-remove! k tree target)
  (let-values (((found? t parent axis) (kd-find k tree target)))
    (if found?
      (let* ((points (append (tree->list (kd-left t)) (tree->list (kd-right t))))
             (new-tree (make-balanced-kd k points axis)))
        (cond
         ((not parent)
          new-tree)
         ((equal? (kd-point (kd-left parent)) target)
          (set! (kd-left parent) new-tree)
          tree)
         ((equal? (kd-point (kd-right parent)) target)
          (set! (kd-right parent) new-tree)
          tree)))
      #f)))

;;
;(define (kd-insert! k tree point))
