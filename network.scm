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
      (let ((msg (mailbox-receive! event-queue)))
        (dispatch-message msg))
      (loop))))

(define (dispatch-message msg)
  (match msg
    (`(event . ,type)
     (match type
       (`(chat . ,evt) (chat-handle-event! evt))
       (`(token . ,evt) (token-handle-event! evt))))
    (`(client . ,type)
     (match type
       (`(disconnect . ()) (handle-disconnect!))
       (`(reconnect . ,(alist . ())) (handle-reconnect! alist))))))


(define (handle-disconnect!)
  (print "disconnected")
  ;; port is now invalid
  (set! (state-port (*state*)) #f))

(define (handle-reconnect! alist)
  (print "reconnected")
  (let ((port (assoc/cdr 'port alist)))
    ;; our state is now invalid, reset it while setting port
    (set! (*state*) (make-state '() '() port))
    ;; immediately request a log replay
    (write '(command log replay) port)))

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

;; client events, from the network thread

(define (make-client-reconnect-event port)
  `(client reconnect ((port . ,port))))

(define (make-client-disconnect-event)
  `(client disconnect))

;; output port handling

(define (send-server-message! msg)
  (let ((port (state-port (*state*))))
    (when port
      (write msg port))))
