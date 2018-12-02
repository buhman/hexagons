;; event handling

(define (handle-event! ev exit-loop! out)
  (case (sdl2:event-type ev)
    ((quit)
     (print "quit")
     (exit-loop! #t))

    ((key-down)
     (chat-handle-key (sdl2:keyboard-event-sym ev) out))
     ;(print 'key-down " " (sdl2:keyboard-event-sym ev)))

    ((key-up))
     ;(print 'key-up " " (sdl2:keyboard-event-sym ev)))

    ((text-input)
     (chat-handle-input-text (sdl2:text-input-event-text ev)))

    ((mouse-button-down)
     (case (sdl2:mouse-button-event-button ev)
       ((middle)
        (set! (grip-x *grip*) (sdl2:mouse-button-event-x ev))
        (set! (grip-y *grip*) (sdl2:mouse-button-event-y ev)))
       ((left)
        (let* ((point (P (sdl2:mouse-button-event-x ev) (sdl2:mouse-button-event-y ev)))
               (cube (pixel->cube *grip* point)))
          (set! (selector-focus-tile *selector*) (cube-nearest cube))))))

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
               (cube (pixel->cube *grip* point)))
          (set! (selector-hover-tile *selector*) (cube-nearest cube))))
       (x #f)))

    ((mouse-wheel)
     (let ((new-scale (+ (grip-scale *grip*)
                         (* 30 (sdl2:mouse-wheel-event-y ev)))))
       (if (< new-scale 30)
         #f
         (set! (grip-scale *grip*) new-scale))))))

(define (handle-events! exit-loop! out)
  (cond
   ((sdl2:has-events?)
    (begin
      (handle-event! (sdl2:poll-event!) exit-loop! out)
      (handle-events! exit-loop! out)))))
