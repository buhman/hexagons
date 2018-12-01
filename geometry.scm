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
