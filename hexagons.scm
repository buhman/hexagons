(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:)
     srfi-8
     srfi-13
     srfi-18
     srfi-69
     matchable
     section-combinators
     tcp6
     clojurian-syntax
     mailbox)

;; aliases

(define C sdl2:make-color)
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
(include "tile.scm")
(include "animator.scm")
(include "lighting.scm")
(include "bsp.scm")

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
(define +cyan+ (C 0 255 255))
(define +blue+ (C 0 0 255))
(define +purple+ (C 255 0 255))
(define +magenta+ (C 255 0 128))

;; helpers

(define assoc/cdr
  (compose cdr assoc))

;; state locals

(define *state* (make-parameter #f)) ; synchronized

(define +default-tile-mode+ '((visible . #t) (pathable . #t)))
(define *editor* (make-parameter (make-editor
                                  'tile
                                  +default-tile-mode+
                                  'cyan)))

;; state globals
;; - these should probably become parameters as well

(define *grip* (make-grip 0 0 0 0 60))
(define *selector* (make-selector '(0 0 0) #f))
(define *mouse* #f)

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

(define (event-loop event-queue)
  (call/cc
   (lambda (exit-loop!)
     (let loop ((ticks (sdl2:get-ticks)))
       (sdl2:pump-events!)
       (handle-events! exit-loop!)
       (handle-queue-events! event-queue)

       (animator-list-update! (sdl2:get-ticks))

       (render-scene! *renderer*)

       (render-chat! *renderer*)

       (render-network-state! *renderer*)
       (render-editor-state! *renderer*)
       (render-fps! *renderer* (- (sdl2:get-ticks) ticks))

       (sdl2:render-present! *renderer*)
       (sdl2:delay! 20)
       (thread-yield!)
       (loop (sdl2:get-ticks))))))

;; network event loop

(tcp-read-timeout #f)

(define (network-loop event-queue)
  (let reconnect ((sleep 5))
    (mailbox-send! event-queue (make-client-disconnect-event))
    (handle-exceptions exn
        (begin
          (print-error-message exn)
          (case (get-condition-property exn 'exn 'location)
            ((socket-connect)
             (thread-sleep! sleep)
             (reconnect 10))))
      (let-values (((in out) (tcp-connect +hostname+ +port+)))
        ;; inform the game thread we have a new port
        (print "reconnect-event")
        (mailbox-send! event-queue (make-client-reconnect-event out))
        (let handle-next-message ()
          (thread-yield!)
          (if (and (not (port-closed? in))
                   (handle-message in event-queue))
            (handle-next-message)
            (reconnect 5))))))

  (mailbox-send! event-queue (make-client-disconnect-event))
  (print "network loop exit!"))

;; client

(define +hostname+ "localhost")
(define +port+ 4242)

(define *debug-state* #f)

(define (make-default-state #!optional (port #f))
  (make-state (make-hash-table) #f '() port))

(define (game-client)
  (let* ((event-queue (make-mailbox))
         (net-thread (thread-start! (lambda () (network-loop event-queue)))))
    (dynamic-wind
        (lambda ()
          (set! (*state*) (make-default-state))
          ;; give us a way to hack at state
          (set! *debug-state* (*state*)))
        (lambda () (event-loop event-queue))
        (lambda () (thread-terminate! net-thread)))))

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
