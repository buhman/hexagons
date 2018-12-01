;; event handling

(define (handle-event! ev)
  (case (sdl2:event-type ev)
    ((mouse-button-down)
     (case (sdl2:mouse-button-event-button ev)
       ((middle)
        (set! (grip-x *grip*) (sdl2:mouse-button-event-x ev))
        (set! (grip-y *grip*) (sdl2:mouse-button-event-y ev)))
       ((left)
        (let* ((point (P (sdl2:mouse-button-event-x ev) (sdl2:mouse-button-event-y ev)))
               (coord (pixel->coord *grip* point)))
          (set! (selector-focus-tile *selector*) (coord-nearest coord))))))
    ((mouse-motion)
     (match (sdl2:mouse-motion-event-state ev)
       ('(middle)
        (set! (grip-dx *grip*) (+ (grip-dx *grip*)
                                  (- (grip-x *grip*)
                                     (sdl2:mouse-motion-event-x ev))))
        (set! (grip-dy *grip*) (+ (grip-dy *grip*)
                                  (- (grip-y *grip*)
                                     (sdl2:mouse-motion-event-y ev))))
        (set! (grip-x *grip*) (sdl2:mouse-motion-event-x ev))
        (set! (grip-y *grip*) (sdl2:mouse-motion-event-y ev)))
       ('()
        (let* ((point (P (sdl2:mouse-motion-event-x ev) (sdl2:mouse-motion-event-y ev)))
               (coord (pixel->coord *grip* point)))
          (set! (selector-hover-tile *selector*) (coord-nearest coord))))
       (x #f)))
    ((mouse-wheel)
     (let ((new-scale (+ (grip-scale *grip*)
                         (* 30 (sdl2:mouse-wheel-event-y ev)))))
       (if (< new-scale 30)
         #f
         (set! (grip-scale *grip*) new-scale))))))

(define (handle-events!)
  (cond
   ((sdl2:has-events?)
    (begin
      (handle-event! (sdl2:poll-event!))
      (handle-events!)))))
