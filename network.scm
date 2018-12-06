;; network thread

(define (handle-message in event-queue)
  (let ((msg (read in)))
    (case msg
      ((#!eof) #f)
      (else
       (mailbox-send! event-queue msg)
       #t))))

;; mailbox / game thread

(define (handle-queue-events! event-queue)
  (let loop ()
    (when (not (mailbox-empty? event-queue))
      (mailbox-receive! event-queue)
      (loop))))

(define (dispatch-message msg)
  (match msg
    (`(event . ,type)
     (match type
       (`(chat . ,evt) (chat-handle-event! evt))
       (`(token . ,evt) (token-handle-event! evt))))))

;; model

(define (token->list token)
  (list
   (token-cube token)
   (token-id token)
   (sdl2:color->list (token-color token))))

(define (list->token l)
  (let-values (((cube id color) (apply values l)))
    (make-token cube id (apply sdl2:make-color color))))

(define (make-chat-message text)
  `(event chat message (text . ,text)))

(define (make-token-move-event token cube)
  `(event token move ((token . ,(token->list token))
                      (cube . ,cube))))

(define (make-token-create-event token)
  `(event token create ((token . ,token))))

(define (make-token-delete-event token)
  `(event token delete ((token . ,token))))

(define (make-tile-create-event tile)
  `(event tile create ((tile . ,tile))))

(define (make-tile-delete-event cube)
  `(event tile delete ((cube . ,cube))))
