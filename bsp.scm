(use matchable)

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
