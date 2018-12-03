(define (token-move! alist)
  (let* ((tl (assoc/cdr 'token alist))
         (token (list->token tl))
         (cube (assoc/cdr 'cube alist))
         (new-token (make-token cube (token-id token) (token-color token))))
    (set! *tokens*
      (->> *tokens*
           (alist-delete (token-cube token))
           (alist-cons (token-cube new-token) new-token)))
    (set! (selector-focus-tile *selector*) cube)))

(define (token-handle-event! evt)
  (match evt
    (`(move . ,(alist .())) (token-move! alist))))
