(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:)
     srfi-8
     srfi-13
     srfi-18
     matchable
     section-combinators
     tcp6
     clojurian-syntax)

;; aliases

(define C sdl2:make-color)
(define P sdl2:make-point)
(define R sdl2:make-rect)

;; initialization

(sdl2:set-main-ready!)
(sdl2:init! '(video events))
(ttf:init!)

(on-exit sdl2:quit!)

(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     ;(sdl2:quit!)
     (original-handler exception))))

;; includes

(include "model.scm")
(include "chat.scm")
(include "draw.scm")
(include "render.scm")
(include "events.scm")
(include "path.scm")
(include "network.scm")
(include "token.scm")
(include "animator.scm")

;; constants

(define +screen-width+ 800)
(define +screen-height+ 800)

(define +title+ "hexagons")

(define +black+ (C 0 0 0))
(define +white+ (C 255 255 255))
(define +ultragrey+ (C 55 55 55))
(define +darkgrey+ (C 98 98 98))
(define +lightgrey+ (C 183 183 183))

(define +red+ (C 255 0 0))
(define +orange+ (C 255 128 0))
(define +yellow+ (C 255 255 0))
(define +green+ (C 0 255 0))
(define +cyan+ (C 0 255 128))
(define +blue+ (C 0 0 255))
(define +purple+ (C 255 0 255))
(define +magenta+ (C 255 0 128))

;; helpers

(define assoc/cdr
  (compose cdr assoc))

;; tiles/scene

(define (tile-alist axial color pathable)
  (let ((cube (axial->cube axial)))
    (cons
     cube
     (make-tile cube color pathable))))

(define (TP axial)
  (tile-alist axial +white+ #t))

(define (TU axial)
  (tile-alist axial +blue+ #f))

(define +tiles+
  (append
   (map TP '((0 0)
             (1 0)
             (0 1)
             (1 2)
             (2 0)
             (4 1)
             (3 1)
             (3 0)
             (5 3)
             (2 3)
             (3 2)
             (0 4)
             (1 4)
             (-1 4)
             (2 1)
             (4 0)
             (-1 5)
             (-2 5)))
   (map TU '((0 2)
             (1 1)
             (2 2)
             (1 3)
             (0 3)))))

(define (T axial id color)
  (let ((cube (axial->cube axial)))
    (cons cube (make-token cube id color))))

(define *tokens*
  (list
   (T '(0 0) 'orange +orange+)
   (T '(1 0) 'magenta +magenta+)))

;; grip

(define *grip* (make-grip 0 0 0 0 60))

;; selector

(define *selector* (make-selector '(0 0 0) '(0 0 0)))

;; window / renderer

(define-values (*window* *renderer*)
  (sdl2:create-window-and-renderer!
   +screen-width+ +screen-height+
   '(shown resizable allow-high-dpi)))

(set! (sdl2:window-title *window*) +title+)

(set! (sdl2:render-viewport *renderer*)
      (R 0 0 +screen-width+ +screen-height+))

(display (sdl2:renderer-info-flags (sdl2:get-renderer-info *renderer*)))
(newline)

;; text

(define +default-font-size+ 20)
(define *font-size* +default-font-size+)
(define *font* (ttf:open-font "DejaVuSansMono.ttf" *font-size*))

;; game event loop

(define (event-loop out)
  (call/cc
   (lambda (exit-loop!)
     (let loop ()
       (sdl2:pump-events!)
       (handle-events! exit-loop! out)

       (animator-list-update! (sdl2:get-ticks))

       (render-scene! *renderer*)
       (render-chat! *renderer*)

       (sdl2:render-present! *renderer*)
       (sdl2:delay! 20)
       (thread-yield!)
       (loop)))))

;; network event loop

(tcp-read-timeout #f)

(define (network-loop in)
  (let handle-next-message ()
    (when (and (not (port-closed? in))
               (handle-message in))
      (thread-yield!)
      (handle-next-message)))
  (print "network loop exit"))

;; client

(define +hostname+ "localhost")
(define +port+ 4242)

(define (game-client)
  (let-values (((in out) (tcp-connect +hostname+ +port+)))
    (write '(command log replay) out)
    (let ((net-thread (thread-start! (lambda () (network-loop in)))))
      (event-loop out)
      (thread-terminate! net-thread))))

;; background thread

(define *game-client-thread* (make-parameter #f))

(begin
  (when (not (eq? (*game-client-thread*) #f))
    (thread-terminate! (*game-client-thread*)))
  (*game-client-thread* (thread-start! game-client)))

;; restart a terminated event loop
(when (eq? 'terminated (thread-state (*game-client-thread*)))
  (*game-client-thread* (thread-start! game-client)))

(cond-expand
 (compiling (thread-join! (*game-client-thread*)))
 (else))
