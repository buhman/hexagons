(define *input-buffer* '())

(define (input-buffer->string buf)
  (string-join (reverse buf) ""))

(define (chat-handle-input-text s)
  (set! *input-buffer* (cons s *input-buffer*)))

(define (chat-handle-key sym out)
  (case sym
    ((backspace)
     (match *input-buffer*
       (())
       ((_ . tail) (set! *input-buffer* tail))))
    ((return)
     (match *input-buffer*
       (())
       (_
        (let* ((text (input-buffer->string *input-buffer*))
               (msg (make-chat-message text)))
          (write msg out))
        (set! *input-buffer* '()))))))

(define *chat-history* '())

(define (chat-handle-event! evt)
  (match evt
    (`(message . ,(text . ()))
     (set! *chat-history* (cons text *chat-history*)))))

(define +chat-lines+ 5)

(define (input-buffer-rect renderer line-width line-height line-num)
  (set! *lh* line-height)
  (let-values (((rw rh) (sdl2:renderer-output-size *renderer*)))
    (let* ((padding (floor (/ line-height 2)))
           (line-space (floor (/ line-height 4)))
           (x padding)
           (y (- rh (* (+ line-space line-height) (+ 1 line-num)))))
      (R x y line-width line-height))))

(define (render-chat-line! renderer color s line-num)
  (let* ((surface (ttf:render-text-solid *font* s color))
         (texture (sdl2:create-texture-from-surface *renderer* surface))
         (w (sdl2:surface-w surface))
         (h (sdl2:surface-h surface))
         (dest-rect (input-buffer-rect renderer w h line-num)))
    (sdl2:render-copy! renderer texture #f dest-rect)))

(define (render-input-buffer! renderer)
  (let* ((s (input-buffer->string *input-buffer*)))
    (render-chat-line! renderer +white+ s 0)))

(define (render-chat-history! renderer)
  (map
   (lambda (s line-num) (render-chat-line! renderer +white+ s line-num))
   *chat-history* (iota +chat-lines+ 1)))

(define (render-chat! renderer)
  (when (not (eq? *input-buffer* '()))
    (render-input-buffer! renderer))
  (render-chat-history! renderer))
