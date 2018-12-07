;; event handling

;; generate server events

;(define (server-event))

;; this should be made more generic, also not sure if ->cube should event want a
;; point
(define (mouse-button-event->point ev)
  (cons (sdl2:mouse-button-event-x ev) (sdl2:mouse-button-event-y ev)))

(define (mouse-button-event->cube grip ev)
  (let ((point (mouse-button-event->point ev)))
    (cube-nearest (point->cube grip point))))

;; server-side handlers

(define (event-token-move! cube)
  (let* ((tokens (state-tokens (*state*)))
         (token (assoc (selector-focus-tile *selector*) tokens)))
    (when token
      (let ((msg (make-token-move-event (cdr token) cube)))
        (send-server-message! msg)))))

(define (tile-flags->tile-color flags)
  (match flags
    ('(pathable visible) +white+)
    ('(visible pathable) +white+)
    ('(visible) +blue+)
    ('(pathable) +orange+)
    ('() +ultragrey+)))

(define (alist->flags alist)
  (foldl
   (lambda (l i)
     (if (cdr i)
       (cons (car i) l)
       l))
   '()
   alist))

(define (event-tile-create! cube)
  (let* ((tm (editor-tile-mode (*editor*)))
         (pathable (assoc/cdr 'pathable tm))
         (visible (assoc/cdr 'visible tm))
         (color (tile-flags->tile-color (alist->flags tm)))
         (tile (make-tile cube color pathable visible))
         (tl (tile->list tile))
         (msg (make-tile-create-event tl)))
    (send-server-message! msg)))

(define (event-tile-delete! cube)
  (let* ((tiles (state-tiles (*state*)))
         (tile (alist-ref cube tiles equal?)))
    ;; try to filter obviously-invalid events
    (when (and tile (equal? (tile-cube tile) cube))
      (send-server-message! (make-tile-delete-event cube)))))

(define (event-token-create! cube)
  (let* ((token (make-token cube 'generic +cyan+))
         (tl (token->list token))
         (msg (make-token-create-event tl)))
    (send-server-message! msg)))

(define (event-token-delete! cube)
  (let* ((tokens (state-tokens (*state*)))
         (token (alist-ref cube tokens equal?)))
    ;; try to filter obviously-invalid events
    (when (and token (equal? (token-cube token) cube))
      (send-server-message! (make-token-delete-event cube)))))

;; client-side handlers

(define (event-token-select! cube)
  (let* ((tokens (state-tokens (*state*)))
         (token (assoc cube tokens)))
    (set! (selector-focus-tile *selector*) (and token cube))))

(define (toggle-editor-tile-mode! s)
  (let* ((tm (editor-tile-mode (*editor*)))
         (value (not (assoc/cdr s tm))))
    (set! (editor-tile-mode (*editor*)) (alist-update s value tm))))

(define (handle-mode-switch! sym)
  (case sym
    ((space)
     (let ((mode (editor-mode (*editor*))))
       (set! (editor-mode (*editor*))
         (case mode
           ((tile) 'token-edit)
           ((token-edit) 'token)
           ((token) 'tile)))))
    ; pathable
    ((p) (toggle-editor-tile-mode! 'pathable))
    ; visible
    ((v) (toggle-editor-tile-mode! 'visible))))

(define (handle-mouse-motion! ev)
  (set! *mouse* (cons (sdl2:mouse-motion-event-x ev) (sdl2:mouse-motion-event-y ev)))
  (match (sdl2:mouse-motion-event-state ev)
    ;; grip drag
    ('(middle)
     (set! (grip-dx *grip*) (+ (grip-dx *grip*)
                               (- (grip-x *grip*) (sdl2:mouse-motion-event-x ev))))
     (set! (grip-dy *grip*) (+ (grip-dy *grip*)
                               (- (grip-y *grip*) (sdl2:mouse-motion-event-y ev))))
     (set! (grip-x *grip*) (sdl2:mouse-motion-event-x ev))
     (set! (grip-y *grip*) (sdl2:mouse-motion-event-y ev)))
    ;; tile hover
    ('()
     (let* ((point (cons (sdl2:mouse-motion-event-x ev) (sdl2:mouse-motion-event-y ev)))
            (cube (point->cube *grip* point)))
       (set! (selector-hover-tile *selector*) (cube-nearest cube))))
    (x #f)))

(define (handle-mouse-button-down! ev)
  (let ((mode (editor-mode (*editor*)))
        (point (mouse-button-event->point ev))
        (cube (mouse-button-event->cube *grip* ev)))
    (case (sdl2:mouse-button-event-button ev)
      ((middle)
       ;; probably, the grip should just be a point
       (set! (grip-x *grip*) (car point))
       (set! (grip-y *grip*) (cdr point)))
      ((left)
       (case mode
         ((token) (event-token-select! cube))
         ((tile) (event-tile-create! cube))
         ((token-edit) (event-token-create! cube))))
      ((right)
       (case mode
         ((token) (event-token-move! cube))
         ((tile) (event-tile-delete! cube))
         ((token-edit) (event-token-delete! cube)))))))

(define (handle-mouse-wheel! ev)
  (let ((new-scale (+ (grip-scale *grip*)
                      (* 30 (sdl2:mouse-wheel-event-y ev)))))
    (if (< new-scale 30)
      #f
      (set! (grip-scale *grip*) new-scale))))

;; event dispatch

(define (handle-event! ev exit-loop!)
  (case (sdl2:event-type ev)
    ((quit)
     (print "quit")
     (exit-loop! #t))

    ((window)
     ;; invalidate mouse
     (set! *mouse* #f))

    ((key-down)
     (match (sdl2:keyboard-event-mod ev)
       ('(ctrl lctrl) (handle-mode-switch! (sdl2:keyboard-event-sym ev)))
       ('() (chat-handle-key (sdl2:keyboard-event-sym ev)))
       (_ #f)))

    ((key-up))

    ((text-input)
     (chat-handle-input-text (sdl2:text-input-event-text ev)))

    ((mouse-button-down)
     (handle-mouse-button-down! ev))

    ((mouse-motion)
     (handle-mouse-motion! ev))

    ((mouse-wheel)
     (handle-mouse-wheel! ev))))

(define (handle-events! exit-loop!)
  (cond
   ((sdl2:has-events?)
    (begin
      (handle-event! (sdl2:poll-event!) exit-loop!)
      (handle-events! exit-loop!)))))
