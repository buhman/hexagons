(define +ms-per-node+ 250)

(define (token-move! a b)
  (and-let* ((tiles (state-tiles (*state*)))
             (sg (flood-search a tiles tile-neighbors))
             (rpath (flood-path a b sg))
             (path (reverse rpath))
             (duration (* +ms-per-node+ (length path))))
    ;; create a new animator to show the motion
    (append-animator! (make-token-path-animator #f duration path a))
    ;; move the selector with the token, if its previous focus was this token
    (let ((s-cube (selector-focus-tile *selector*)))
      (when (equal? s-cube a)
        (set! (selector-focus-tile *selector*) b)))))

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

(define (token-handle-move-event! alist)
  (let* ((tl (assoc/cdr 'token alist))
         (token (list->token tl))
         (a (token-cube token))
         (b (assoc/cdr 'cube alist)))
    (token-move! a b)))

(define (symbol->color cs)
  (case cs
    ((cyan) +cyan+)
    ((magenta) +magenta+)
    ((green) +green+)))

(define (token-next-color cs)
  (case cs
    ((cyan) 'magenta)
    ((magenta) 'green)
    ((green) 'cyan)))

(define (make-token-animator fn)
  (make-animator
   0
   1
   identity
   fn))

(define (token-handle-create-event! alist)
  (let ((token (list->token (assoc/cdr 'token alist))))
    (append-animator! (make-token-animator (token-create token)))))

(define (token-handle-delete-event! alist)
  (let ((cube (assoc/cdr 'cube alist)))
    (append-animator! (make-token-animator (token-delete cube)))))

(define (token-handle-event! evt)
  (match evt
    (`(move . ,(alist . ())) (token-handle-move-event! alist))
    (`(create . ,(alist . ())) (token-handle-create-event! alist))
    (`(delete . ,(alist . ())) (token-handle-delete-event! alist))))
