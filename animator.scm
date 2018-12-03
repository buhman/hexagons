;; animator

(define-record-type animator
  (make-animator epoch duration delta-fn end-fn)
  animator?
  (epoch animator-epoch)
  (duration animator-duration)
  (delta-fn animator-delta-fn)
  (end-fn animator-end-fn))

(define *animators* '())

(define (token-path-animator path cube)
  (lambda (t)
    (let* ((token (assoc/cdr cube *tokens*))
           (new-cube (cube-path-lerp path t))
           (new-token (make-token new-cube (token-id token) (token-color token))))
      (set! *tokens* (alist-update cube new-token *tokens* equal?)))))

(define (token-end cube)
  (lambda ()
    (let* ((token (assoc/cdr cube *tokens*))
           (new-token (make-token cube (token-id token) (token-color token))))
      (set! *tokens* (alist-update cube new-token *tokens* equal?)))))

(define (make-token-path-animator epoch duration path cube)
  (make-animator
   epoch
   duration
   (token-path-animator path cube)
   (token-end cube)))

(define (animator-update! animator ticks)
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

(define (animators-update! ticks)
  (set! *animators*
    (filter
     (lambda (animator)
       (animator-update! animator ticks))
     *animators*)))
