(include "bsp.scm")

(define +points+
  '(#(2 3) #(5 4) #(9 6) #(4 7) #(8 1) #(7 2)))

;;

(test-group
 "split-points"
 (test-values "split x-axis"
   '(#(7 2) (#(2 3) #(4 7) #(5 4)) (#(8 1) #(9 6)))
   (split-points +points+ 0))

 (test-values "split y-axis"
   '(#(5 4) (#(8 1) #(7 2) #(2 3)) (#(9 6) #(4 7)))
   (split-points +points+ 1)))

;;

(define +tree+
  (make-kd #(7 2)
           (make-kd #(5 4)
                    (make-kd #(2 3) #f #f)
                    (make-kd #(4 7) #f #f))
           (make-kd #(9 6)
                    (make-kd #(8 1) #f #f)
                    #f)))

(define-syntax count-visits
  (ir-macro-transformer
   (lambda (expr inject compare)
     (let* ((body (cdr expr))
            (body (map (lambda (b) (append b (list 'visited))) body)))
       `(let* ((visits 0)
               (visited (lambda () (set! visits (add1 visits)))))
          ,@body
          visits)))))

(test-group
 "kd"
 (test "make-balanced-kd"
   +tree+
   (make-balanced-kd 2 +points+))

 (test "kd-select-range multiple"
   '(#(5 4) #(2 3) #(4 7))
   (kd-select-range 2 +tree+ #(1 1) #(6 7)))

 (test "kd-select-range one"
   '(#(5 4))
   (kd-select-range 2 +tree+ #(3 1) #(6 5)))

 (test "kd-select-range equal"
   '(#(9 6))
   (kd-select-range 2 +tree+ #(9 6) #(9 6)))

 (test "kd-select-range visits"
   3
   (count-visits
    (kd-select-range 2 +tree+ #(7 2) #(7 2))))

 (test "kd-select-range visits split"
   4
   (count-visits
    (kd-select-range 2 +tree+ #(1 1) #(6 7)))))
