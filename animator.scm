;; animator

(define-record-type animator
  (make-animator epoch duration delta-fn end-fn)
  animator?
  (epoch animator-epoch (setter animator-epoch))
  (duration animator-duration)
  (delta-fn animator-delta-fn)
  (end-fn animator-end-fn))

(define *animator-alist* '())

(define (token-path-animator path cube)
  (lambda (t)
    (let* ((token (assoc/cdr cube *tokens*))
           (new-cube (cube-path-lerp path t))
           (new-token (make-token new-cube (token-id token) (token-color token))))
      (set! *tokens* (alist-update cube new-token *tokens* equal?)))))

(define (token-end path cube)
  (lambda ()
    (let* ((token (assoc/cdr cube *tokens*))
           (b (last path))
           (new-token (make-token b (token-id token) (token-color token))))
      (set! *tokens*
        (->> *tokens*
             (alist-delete cube)
             (alist-cons b new-token))))))

(define (make-token-path-animator epoch duration path cube)
  (make-animator
   epoch
   duration
   (token-path-animator path cube)
   (token-end path cube)))

(define (animator-update! ticks animator)
  (let* ((epoch (animator-epoch animator))
         (duration (animator-duration animator))
         (delta (- ticks epoch))
         (t (/ delta duration)))
    (if (>= delta duration)
      (begin
        ((animator-end-fn animator))
        #f)
      (begin
        ((animator-delta-fn animator) t)
        #t))))

(define (animators-update! ticks animators)
  (cons
   (car animators)
   (match (cdr animators)
     ((animator . rest)
      (let ((epoch (animator-epoch animator)))
        (when (not epoch)
          (set! (animator-epoch animator) ticks))
        (if (animator-update! ticks animator)
          (cons animator rest)
          rest)))
     (() '()))))

(define (animator-alist-update! ticks)
  (set! *animator-alist*
    (map
     (lambda (animators)
       (animators-update! ticks animators))
     *animator-alist*)))

(define (register-token-animator! token animator)
  (set! *animator-alist*
    (let* ((id (token-id token))
           (key (list 'token id))
           (animators (alist-ref key *animator-alist* equal? '())))
      (alist-update key (append animators (list animator)) *animator-alist* equal?))))