(use tcp6
     srfi-18
     matchable)

(define +port+ 4242)

(define *connections* '())

(define (with-mutex-lock! mut fn)
  (dynamic-wind
      (lambda () (mutex-lock! mut))
      fn
      (lambda () (mutex-unlock! mut))))

(define (mutex-update! mut fn)
  (with-mutex-lock!
   mut
   (lambda ()
     (let ((val (fn (mutex-specific mut))))
       (mutex-specific-set! mut val)
       val))))

(define (make-mutex/value name value)
  (let ((mut (make-mutex name)))
    (mutex-specific-set! mut value)
    mut))

(define *last-exception* #f)

(define (print-exception! exn thread-id)
  (set! *last-exception* exn)
  (print "thread-id[" thread-id "]: ")
  (print-error-message exn)
  (print-call-chain (current-output-port) 0 (current-thread)))

;; (equal? (cdr (condition->list *last-exception*)) '((i/o) (net) (timeout)))
;; #t

(define (broadcast-message msg)
  (map
   (lambda (conn) (write msg (cdr conn)))
   *connections*)
  msg)

(define (make-chat-message-id thread-id alist)
  (let ((al (alist-delete 'id alist)))
    `(event chat message ,(alist-cons 'id thread-id al))))

(define (event-log-replay! out)
  (let* ((event-log (mutex-specific *event-log*))
         (log (reverse event-log)))
    (map
     (lambda (event) (write event out))
     log)))

(define (dispatch-message thread-id out msg)
  (print "dispatch-message: " msg)
  (match msg
    (`(event . ,type)
     (match type
       (`(chat . (message . ,alist))
        (broadcast-message (make-chat-message-id thread-id alist)))
       (`(token . (move . ,alist))
        (broadcast-message msg))
       (`(tile . (create . ,alist))
        (broadcast-message msg))
       (`(tile . (delete . ,alist))
        (broadcast-message msg))
       (`(token . (create . ,alist))
        (broadcast-message msg))
       (`(token . (delete . ,alist))
        (broadcast-message msg))))
    (`(command . ,type)
     (match type
       (`(log . (replay . ()))
        (print "replay requested by: " thread-id)
        (event-log-replay! out)))
     ;; commands never mutate the event log
     #f)))

(define (event-log-append! thunk)
  (mutex-update!
   *event-log*
   (lambda (event-log)
     (let ((event (thunk)))
       (if event
         (cons event event-log)
         event-log)))))

(define (handle-message thread-id in out)
  (handle-exceptions exn
    (case (get-condition-property exn 'exn 'location)
      ((socket-receive!)) ; ignore/retry socket-receive timeout
      (else
       (print-exception! exn thread-id)
       (close-input-port in)
       (close-output-port out)))
    (let ((msg (read in)))
      (case msg
        ((#!eof) (error 'handle-message "client eof"))
        (else
         (event-log-append!
          (lambda () (dispatch-message thread-id out msg))))))))

(define (accept-loop listener)
  (let ((thread-count (make-mutex/value 'thread-count 0)))
    (let accept-next-connection ()
      (let*-values (((in out) (tcp-accept listener))
                    ((local remote) (tcp-addresses in))
                    ((thread-id) (mutex-update! thread-count add1)))
        (with-mutex-lock!
         thread-count
         (lambda () (set! *connections* (alist-cons thread-id out *connections*))))

        (print "connected: " remote)

        (thread-start!
         (lambda ()
           (let handle-next-message ()
             (when (and (not (port-closed? in))
                        (not (port-closed? out)))
               (handle-message thread-id in out)
               (handle-next-message)))
           (close-input-port in)
           (close-output-port out)
           (with-mutex-lock!
            thread-count
            (lambda () (set! *connections* (alist-delete thread-id *connections*)))))))

      (accept-next-connection))))

(define (start-server)
  ; event log persistence?
  (set! *event-log* (make-mutex/value 'event-log '()))
  (let ((listener (tcp-listen +port+)))
    (print "listening " +port+)
    (accept-loop listener)))

;; state

(define *event-log* #f)

;; background thread

(define *accept-loop-thread* (make-parameter #f))

(begin
  (when (not (eq? (*accept-loop-thread*) #f))
    (thread-terminate! (*accept-loop-thread*)))
  (*accept-loop-thread* (thread-start! start-server)))

(when (eq? 'terminated (thread-state (*accept-loop-thread*)))
  (*accept-loop-thread* (thread-start! start-server)))

(cond-expand
 (compiling (thread-join! (*accept-loop-thread*)))
 (else))
