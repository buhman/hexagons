;; animator

(define-record-type animator
  (make-animator type epoch duration init-fn delta-fn end-fn)
  animator?
  (type animator-type)
  (epoch animator-epoch (setter animator-epoch))
  (duration animator-duration (setter animator-duration))
  (init-fn animator-init-fn)
  (delta-fn animator-delta-fn (setter animator-delta-fn))
  (end-fn animator-end-fn))

;; (init-fn) -> (values delta-fn duration-or-false)
;; (delta-fn t) -> (void)
;; (end-fn) -> (void)

(define *animators* '())

(define (animator-delta! duration delta ticks animator)
  (let ((t (/ delta duration)))
    ((animator-delta-fn animator) t)))

(define (animator-init! ticks animator)
  (and-let* ((l ((animator-init-fn animator))))
    (match l
      ((delta-fn . duration)
       (set! (animator-delta-fn animator) delta-fn)
       (set! (animator-duration animator) duration)
       (set! (animator-epoch animator) ticks)))))

(define (animator-end! animator)
  ((animator-end-fn animator)))

(define (animator-update! ticks animator)
  (let* ((epoch (animator-epoch animator))
         (duration (animator-duration animator))
         (delta (and epoch (- ticks epoch))))
    (cond
     ;; animator is not initialized
     ((not epoch)
      ;; init is allowed to fail, if the animation is invalid
      (if (animator-init! ticks animator)
        (animator-update! ticks animator)
        (begin
          (print "invalid animation: " (animator-type animator))
          #f)))
     ;; animator is static or expired -> discard
     ((or (not duration) (>= delta duration))
      (animator-end! animator)
      #f)
     ;; otherwise, do a delta-update -> keep
     (else
      (animator-delta! duration delta ticks animator)
      #t))))

(define (animators-update! ticks animators)
  (match animators
    ((animator . rest)
     (if (animator-update! ticks animator)
       (cons animator rest)
       rest))
    (() '())))

(define (animator-list-update! ticks)
  (set! *animators* (animators-update! ticks *animators*)))

(define (append-animator! animator)
  (set! *animators* (append *animators* (list animator))))
