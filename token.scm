(define +ms-per-node+ 250)

;; models

(define (make-token-path-animator token cube)
  (make-animator
   'token-path
   #f
   #f
   (token-path-animator-init token cube)
   #f
   (token-path-animator-end token cube)))

(define (make-token-animator fn)
  (make-animator
   'token
   #f
   #f
   (lambda () (cons identity #f))
   #f
   fn))

;; animators

(define (token-path-animator-init token cube)
  (lambda ()
    (and-let* ((a (token-cube token))
               (b cube)
               (tiles (state-tiles (*state*)))
               (sg (flood-search a tiles tile-neighbors))
               (rpath (flood-path a b sg))
               (path (reverse rpath))
               (duration (* +ms-per-node+ (length path))))
      (cons
       (token-path-animator-delta token path)
       duration))))

(define (token-path-animator-delta token path)
  (lambda (t)
    (let* ((new-cube (cube-path-lerp path t))
           (new-token (make-token new-cube (token-id token) (token-color token))))
      ;; update token-cube, but do not change alist cube
      (set! (state-tokens (*state*))
        (alist-update (token-cube token) new-token (state-tokens (*state*)) equal?)))))

(define (token-path-animator-end token cube)
  (lambda ()
    (let* ((new-token (make-token cube (token-id token) (token-color token))))
      ;; update the token-cube with the final position, and update alist cube
      (set! (state-tokens (*state*))
        (->> (state-tokens (*state*))
          (alist-delete (token-cube token))
          (alist-cons cube new-token))))

    ;; move the selector with the token, if its previous focus was this token
    (let ((s-cube (selector-focus-tile *selector*)))
      (when (equal? s-cube (token-cube token))
        (set! (selector-focus-tile *selector*) cube)))))

(define (token-create token)
  (lambda ()
    (let ((tokens (state-tokens (*state*)))
          (cube (token-cube token)))
      (set! (state-tokens (*state*))
        (alist-update cube token tokens equal?)))))

(define (token-delete cube)
  (lambda ()
    (let ((tokens (state-tokens (*state*))))
      (set! (state-tokens (*state*))
        (alist-delete cube tokens equal?)))))

;; animator factories

(define (token-handle-move-event! alist)
  (let* ((token (list->token (assoc/cdr 'token alist)))
         (cube (assoc/cdr 'cube alist)))
    (append-animator! (make-token-path-animator token cube))))

(define (token-handle-create-event! alist)
  (let ((token (list->token (assoc/cdr 'token alist))))
    (append-animator! (make-token-animator (token-create token)))))

(define (token-handle-delete-event! alist)
  (let ((cube (assoc/cdr 'cube alist)))
    (append-animator! (make-token-animator (token-delete cube)))))

;; dispatch

(define (token-handle-event! evt)
  (match evt
    (`(move . ,(alist . ())) (token-handle-move-event! alist))
    (`(create . ,(alist . ())) (token-handle-create-event! alist))
    (`(delete . ,(alist . ())) (token-handle-delete-event! alist))))
