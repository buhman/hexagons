(define (render-draw-trapezoid! renderer trap)
  (match trap
    ((la ra lb rb)
     (let* ((lay (cadr la))
            (lby (cadr lb))
            (next-y (/ (- lby lay) (abs (- lby lay)))))
       (let loop ((y lay))
         (let ((lx (line-lerp-y->x la lb y))
               (rx (line-lerp-y->x ra rb y)))
           (sdl2:render-draw-line! renderer lx y rx y)
           (cond
            ((= y lby))
            (else (loop (+ y next-y))))))))))

(define (render-draw-polygon! renderer edges)
  (let ((ts (trapezoid-decompose edges)))
    (for-each
     (lambda (t)
       (render-draw-trapezoid! renderer t))
     ts)))

(define (render-draw-edges! renderer edges)
  (for-each
   (lambda (edge)
     (match edge
       (((xa ya) (xb yb))
        (sdl2:render-draw-line! renderer xa ya xb yb))))
   edges))

(define (render-draw-lines! renderer points)
  (let loop ((pts points)
             (lp #f))
    (match pts
      ((p . rest)
       (when lp
         (sdl2:render-draw-line! renderer (car lp) (cdr lp) (car p) (cdr p)))
       (loop rest p))
      (() '()))))
