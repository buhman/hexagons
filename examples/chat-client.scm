(use tcp6
     srfi-18
     matchable
     parley)

(define (chat-message text port)
  (let ((msg `(event chat message (text . ,text))))
    (write msg port)))

(define (input-loop out)
  (let loop ()
    (print "input-loop-begin")
    (let ((line (parley "> "))) ;(read-line)))
      (when (not (eq? line #!eof))
        (chat-message line out)
        (thread-yield!)
        (print "input-loop-end")
        (loop))))
  (print "input-loop-terminate"))

(define +hostname+ "localhost")
(define +port+ 4242)

(define (dispatch-message msg)
  (match msg
    (`(event . ,evt)
     (print "evt:" evt)
     (flush-output))))

(define (handle-message in)
  (let ((msg (read in)))
    (case msg
      ((#!eof) #f)
      (else
       (dispatch-message msg)
       #t))))

(define (client-loop in)
  (let handle-next-message ()
    (print "client-loop-begin")
    (when (and (not (port-closed? in))
               (handle-message in))
      (thread-yield!)
      (print "client-loop-end")
      (handle-next-message)))
  (print "client-loop-terminate"))

(define (start-client)
  (let-values (((in out) (tcp-connect +hostname+ +port+)))
    (begin
      (print "requested event log replay")
      (write '(command log replay) out))
    (let ((client (thread-start! (lambda () (client-loop in))))
          (input (thread-start! (lambda () (input-loop out)))))
      (thread-join! client)
      (thread-join! input))))

(tcp-read-timeout #f)

(start-client)
