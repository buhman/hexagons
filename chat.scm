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
    (`(message . ,(alist . ()))
     (set! *chat-history* (cons alist *chat-history*)))))

(define +chat-lines+ 5)

(define (input-buffer-rect renderer x-offset line-width line-height line-num)
  (let-values (((rw rh) (sdl2:renderer-output-size *renderer*)))
    (let* ((padding (floor (/ line-height 2)))
           (line-space (floor (/ line-height 4)))
           ;; XXX: hacky
           (x (+ x-offset (* padding (if (eq? 0 x-offset) 1 2))))
           (y (- rh (* (+ line-space line-height) (+ 1 line-num)))))
      (R x y line-width line-height))))

(define (input-buffer-rects renderer pre-t text-t line-num)
  (let* ((pre-w (sdl2:texture-w pre-t))
         (pre-h (sdl2:texture-h pre-t))
         (prefix-rect (input-buffer-rect renderer 0 pre-w pre-h line-num))
         (text-w (sdl2:texture-w text-t))
         (text-h (sdl2:texture-h text-t))
         (text-rect (input-buffer-rect renderer pre-w text-w text-h line-num)))
    (values prefix-rect text-rect)))

(define (render-text renderer text color)
  (let ((surface (ttf:render-text-solid *font* text color)))
    (sdl2:create-texture-from-surface *renderer* surface)))

(define (render-chat-text renderer pre text color)
  (let* ((pre-color (sdl2:color-mult color +lightgrey+))
         (pre-t (render-text renderer pre pre-color))
         (text-t (render-text renderer text color)))
    (values pre-t text-t)))

(define (render-chat-line! renderer pre text color line-num)
  (let*-values (((pre-t text-t) (render-chat-text renderer pre text color))
                ((pre-r text-r) (input-buffer-rects renderer pre-t text-t line-num)))
    (sdl2:render-copy! renderer pre-t #f pre-r)
    (sdl2:render-copy! renderer text-t #f text-r)))

(define (render-input-buffer! renderer)
  (let* ((s (input-buffer->string *input-buffer*))
         (s-t (render-text renderer s +white+))
         (w (sdl2:texture-w s-t))
         (h (sdl2:texture-h s-t))
         (s-r (input-buffer-rect renderer 0 w h 0)))
    (sdl2:render-copy! renderer s-t #f s-r)))

(define (render-chat-history! renderer)
  (map
   (lambda (alist line-num)
     (let* ((text (assoc/cdr 'text alist))
            (id (assoc/cdr 'id alist))
            (pre (string-join (list "[" (number->string id) "]") "")))
       (render-chat-line! renderer pre text +white+ line-num)))
   *chat-history* (iota +chat-lines+ 1)))

(define (render-chat! renderer)
  (when (not (eq? *input-buffer* '()))
    (render-input-buffer! renderer))
  (render-chat-history! renderer))
