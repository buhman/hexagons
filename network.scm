(define (dispatch-message msg)
  (match msg
    (`(event . ,type)
     (match type
       (`(chat . ,evt) (chat-handle-event! evt))))))

(define (handle-message in)
  (let ((msg (read in)))
    (case msg
      ((#!eof) #f)
      (else
       (dispatch-message msg)
       #t))))

;; model

(define (make-chat-message text)
  (list 'event 'chat 'message text))
