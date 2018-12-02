(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:)
     srfi-8
     srfi-13
     srfi-18
     matchable
     section-combinators)

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
(include "render.scm")
(include "events.scm")
(include "path.scm")

;; constants

(define +screen-width+ 800)
(define +screen-height+ 800)

(define +title+ "hexagons")

(define +black+ (C 0 0 0))
(define +white+ (C 255 255 255))
(define +ultragrey+ (C 55 55 55))
(define +darkgrey+ (C 98 98 98))
(define +lightgrey+ (C 183 183 183))
(define +blue+ (C 34 0 255))
(define +purple+ (C 255 0 255))
(define +yellow+ (C 255 255 0))
(define +green+ (C 0 255 0))

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

;; grip

(define *grip* (make-grip 0 0 0 0 60))

;; selector

(define *selector* (make-selector '(0 0 0) '(0 0 0)))

;; window / renderer

(define-values (*window* *renderer*)
  (sdl2:create-window-and-renderer!
   +screen-width+ +screen-height+))

(set! (sdl2:window-title *window*) +title+)

(set! (sdl2:render-viewport *renderer*)
      (R 0 0 +screen-width+ +screen-height+))

(display (sdl2:renderer-info-flags (sdl2:get-renderer-info *renderer*)))
(newline)

;; text

(define *font* (ttf:open-font "DejaVuSansMono.ttf" 20))

;; event loop

(define (event-loop)
  (call/cc
   (lambda (exit-loop!)
     (let loop ()
       (sdl2:pump-events!)
       (handle-events! exit-loop!)

       (render-scene! *renderer*)

       (sdl2:render-present! *renderer*)
       (sdl2:delay! 20)
       (thread-yield!)
       (loop)))))

;; background thread

(define *event-loop-thread* (make-parameter #f))

(begin
  (when (not (eq? (*event-loop-thread*) #f))
    (thread-terminate! (*event-loop-thread*)))
  (*event-loop-thread* (thread-start! event-loop)))

;; restart a terminated event loop
(when (eq? 'terminated (thread-state (*event-loop-thread*)))
  (*event-loop-thread* (thread-start! event-loop)))

(cond-expand
 (compiling (thread-join! (*event-loop-thread*)))
 (else))
