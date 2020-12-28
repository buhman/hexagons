(import tcp6
        matchable
        srfi-18
        (only srfi-13 string-join)
        (only (chicken string) ->string)
        levo)

(define (chat-message text port)
  (let ((msg `(event chat message (text . ,text))))
    (write msg port)))

(define +hostname+ "localhost")
(define +port+ 4242)

(define (handle-expression s out from-id)
  (let* ((result (interpret (parse-string s)))
         (text (string-join (list (->string from-id) ": " (->string result)) "")))
    (chat-message text out)
    result))

(define (handle-chat-message alist out)
  (let ((id (cdr (assoc 'id alist)))
        (text (cdr (assoc 'text alist))))
    (match (string->list text)
      ((#\, . s) (handle-expression s out id))
      (_))))

(define (dispatch-message msg out)
  (match msg
    (`(event . (chat . (message . (,alist . ())))) (handle-chat-message alist out))
    (_)))

(define (handle-message in out)
  (let ((msg (read in)))
    (case msg
      ((#!eof) #f)
      (else
       (dispatch-message msg out)
       #t))))

(define (client-loop in out)
  (let handle-next-message ()
    (when (and (not (port-closed? in))
               (not (port-closed? out))
               (handle-message in out))
      (thread-yield!)
      (handle-next-message))))

(define (start-client)
  (tcp-read-timeout #f)
  (let-values (((in out) (tcp-connect +hostname+ +port+)))
    (print "connected")
    (client-loop in out)))

(define *game-client-thread* (make-parameter #f))

(*game-client-thread* (thread-start! start-client))

(cond-expand
 (compiling (thread-join! (*game-client-thread*)))
 (else))
