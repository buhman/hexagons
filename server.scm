(use tcp6
     srfi-18
     matchable)

(define +port+ 4242)

(define *connections* '())

(define (mutex-update! mut fn)
  (dynamic-wind
      (lambda () (mutex-lock! mut))
      (lambda ()
        (let ((val (fn (mutex-specific mut))))
          (mutex-specific-set! mut val)
          val))
      (lambda () (mutex-unlock! mut))))

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
   *connections*))

(define (make-chat-message thread-id alist)
  (let ((al (alist-delete 'id alist)))
    `(event chat message ,(alist-cons 'id thread-id al))))

(define (dispatch-message thread-id out msg)
  (match msg
    (`(event . ,type)
     (match type
       (`(chat . (message . ,alist))
        (broadcast-message (make-chat-message thread-id alist)))))
    (`(command . ,cmd)
     (print "cmd:" cmd))))

(define (handle-message thread-id in out)
  (handle-exceptions exn
    (case (get-condition-property exn 'exn 'location)
      ((socket-receive!))
       ;(print "thread-id[" thread-id "]: ignore socket-recieve timeout"))
      (else
       (print-exception! exn thread-id)
       (close-input-port in)
       (close-output-port out)))
    (let ((msg (read in)))
      (case msg
        ((#!eof) (error 'handle-message "client eof"))
        (else (dispatch-message thread-id out msg))))))

(define (with-mutex-lock! mut fn)
  (dynamic-wind
      (lambda () (mutex-lock! mut))
      fn
      (lambda () (mutex-unlock! mut))))

(define (accept-loop listener)
  (let* ((thread-count (make-mutex/value 'thread-count 0)))
    (let accept-next-connection ()
      (let*-values (((in out) (tcp-accept listener))
                    ((local remote) (tcp-addresses in))
                    ((thread-id) (mutex-update! thread-count add1)))
        (with-mutex-lock!
         thread-count
         (lambda () (set! *connections* (alist-cons thread-id out *connections*))))

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
  (let ((listener (tcp-listen +port+)))
    (print "listening " +port+)
    (accept-loop listener)))

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
