(define +ms-per-node+ 250)

(define (token-move! token a b)
  (and-let* ((sg (flood-search a +tiles+ tile-neighbors))
             (path (reverse (flood-path a b sg)))
             (duration (* +ms-per-node+ (length path))))
    ;; update the token key, but do not actually move the token
    (set! *tokens*
      (->> *tokens*
           (alist-delete a)
           (alist-cons b token)))
    ;; instead, create a new animator to show the motion
    (set! *animators*
      (cons (make-token-path-animator (sdl2:get-ticks) duration path b) *animators*))
    ;; move the selector with the token, if its previous focus was this token
    (let ((s-cube (selector-focus-tile *selector*)))
      (when (equal? s-cube a)
        (set! (selector-focus-tile *selector*) b)))))

(define (token-handle-move-event! alist)
  (let* ((tl (assoc/cdr 'token alist))
         (token (list->token tl))
         (a (token-cube token))
         (b (assoc/cdr 'cube alist)))
    (token-move! token a b)))

(define (token-handle-event! evt)
  (match evt
    (`(move . ,(alist .())) (token-handle-move-event! alist))))
