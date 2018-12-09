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
   (#(5 4) (#(8 1) #(7 2) #(2 3)) (#(9 6) #(4 7)))
   (split-points +points+ 1)))

;;

(test-group
 "make-balanced-kd"
 (test "make-balanced-kd"
   (make-kd #(7 2)
           (make-kd #(5 4)
                    (make-kd #(2 3) #f #f)
                    (make-kd #(4 7) #f #f))
           (make-kd #(9 6)
                    (make-kd #(8 1) #f #f)
                    #f))
   (make-balanced-kd 2 +points+)))
