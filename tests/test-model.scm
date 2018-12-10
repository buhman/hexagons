(include "model.scm")

(define +grip+ (make-grip 0 0 0 0 100))

(define +grip-off+ (make-grip 0 0 100 -100 100))

(test-group
 "cube"

 (test "cube->point origin"
   '(0 . 0)
   (cube->point +grip+ '(0 0 0)))

 (test "cube->point unit grip scale"
   '(260 . 150)
   (cube->point +grip+ '(1 1 -2)))

 (test "cube->point grip translation"
   '(100 . -100)
   (cube->point +grip-off+ '(0 0 0)))

 (test "axial->cube plane"
   0
   (apply + (axial->cube '(10 10)))))

(test-group
 "axial-vector"

 #;
 (test-values "point-rect->axial-vec-range unit"
   '(#(0.0 0.0) #(1.00111069989303 1.0))
   (point-rect->axial-vec-range +grip+ '(0 . 0) '(260 . 150))))
