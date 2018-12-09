(include "bsp.scm")

(define +points+
  '(#(2 3) #(5 4) #(9 6) #(4 7) #(8 1) #(7 2)))

(define +worst-points+
  '(#(2 3) #(2 4) #(2 6) #(2 7) #(2 1) #(2 2)))

(define +tree+
  (make-kd #(7 2)
           (make-kd #(5 4)
                    (make-kd #(2 3) #f #f)
                    (make-kd #(4 7) #f #f))
           (make-kd #(9 6)
                    (make-kd #(8 1) #f #f)
                    #f)))

(define +bigger-tree+
  (make-kd #(4 5)
           (make-kd #(4 4)
                    (make-kd #(3 1)
                             (make-kd #(1 0) #f #f)
                             #f)
                    (make-kd #(2 7)
                             (make-kd #(0 6) #f #f)
                             #f))
           (make-kd #(8 4)
                    (make-kd #(6 4)
                             (make-kd #(5 1) #f #f)
                             #f)
                    (make-kd #(7 6)
                             (make-kd #(6 9) #f #f)
                             #f))))

(define +more-points+
  '(#(61 29) #(89 21) #(89 12) #(10 72) #(13 83) #(47 25) #(55 94) #(87 51) #(74 73) #(49 98) #(71 7) #(16 91) #(80 96) #(35 54) #(6 60) #(37 68) #(90 27) #(89 79) #(40 99) #(52 53) #(83 0) #(78 38) #(95 65) #(90 69) #(39 40) #(67 10) #(47 84) #(1 27) #(80 36) #(81 86) #(97 18) #(54 87) #(45 44) #(67 85) #(44 20) #(39 28) #(20 18) #(67 15) #(83 57) #(85 23) #(97 52) #(33 45) #(36 35) #(72 17) #(71 54) #(3 69) #(73 58) #(56 19) #(3 24) #(5 48)))

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
    (kd-select-range 2 +tree+ #(1 1) #(6 7))))

 (test "kd-select-range more"
   11
   (count-visits
    (kd-select-range 2 (make-balanced-kd 2 +more-points+) #(25 25) #(40 40))))

 (test "tree->list"
   '(#(7 2) #(5 4) #(2 3) #(4 7) #(9 6) #(8 1))
   (tree->list +tree+))

 (test "kd-find"
   #t
   (kd-find 2 +tree+ #(9 6)))

 (test "kd-find missing"
   #f
   (kd-find 2 +tree+ #(10 10)))

 (test "kd-find visits"
   2
   (count-visits
    (kd-find 2 +tree+ #(9 6))))

 (test "kd-find visits worst"
   5
   (count-visits
    (kd-find 2 (make-balanced-kd 2 +worst-points+) #(2 1))))

 (test "kd-remove!"
   (make-kd #(4 5)
              (make-kd #(4 4)
                         (make-kd #(3 1)
                                    (make-kd #(1 0) #f #f)
                                    #f)
                         (make-kd #(2 7)
                                    (make-kd #(0 6) #f #f)
                                    #f))
              (make-kd #(7 6)
                         (make-kd #(6 4)
                                    (make-kd #(5 1) #f #f)
                                    #f)
                         (make-kd #(6 9) #f #f)))
   (kd-remove! 2 +bigger-tree+ #(8 4)))

 (test "kd-remove! root"
   (make-kd #(4 4)
              (make-kd #(0 6)
                         (make-kd #(3 1)
                                    (make-kd #(1 0) #f #f)
                                    #f)
                         (make-kd #(2 7) #f #f))
              (make-kd #(7 6)
                         (make-kd #(6 4)
                                    (make-kd #(5 1) #f #f)
                                    #f)
                         (make-kd #(6 9) #f #f)))
   (kd-remove! 2 +bigger-tree+ #(4 5)))

 (test "kd-remove! missing"
   #f
   (kd-remove! 2 +tree+ #(99 99))))
