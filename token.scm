(define +ms-per-node+ 250)

(define (token-move! a b)
  (and-let* ((sg (flood-search a +tiles+ tile-neighbors))
             (rpath (flood-path a b sg))
             (path (reverse rpath))
             (duration (* +ms-per-node+ (length path))))
    ;; create a new animator to show the motion
    (append-token-animator! (make-token-path-animator #f duration path a))
    ;; move the selector with the token, if its previous focus was this token
    (let ((s-cube (selector-focus-tile *selector*)))
      (when (equal? s-cube a)
        (set! (selector-focus-tile *selector*) b)))))

(define (token-handle-move-event! alist)
  (let* ((tl (assoc/cdr 'token alist))
         (token (list->token tl))
         (a (token-cube token))
         (b (assoc/cdr 'cube alist)))
    (token-move! a b)))

(define (token-handle-event! evt)
  (match evt
    (`(move . ,(alist .())) (token-handle-move-event! alist))))
