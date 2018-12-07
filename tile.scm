;; there is no animation for tiles, but tile mutations need to be ordered other
;; animated event types

(define (make-tile-animator fn)
  (make-animator
   'tile
   #f
   #f
   (lambda () (values identity #f))
   #f
   fn))

(define (tile-create tile)
  (lambda ()
    (let ((tiles (state-tiles (*state*)))
          (cube (tile-cube tile)))
      (set! (state-tiles (*state*))
        (alist-update cube tile tiles equal?)))))

(define (tile-delete cube)
  (lambda ()
    (let ((tiles (state-tiles (*state*))))
      (set! (state-tiles (*state*))
        (alist-delete cube tiles equal?)))))

(define (tile-handle-create-event! alist)
  (let ((tile (list->tile (assoc/cdr 'tile alist))))
    (append-animator! (make-tile-animator (tile-create tile)))))

(define (tile-handle-delete-event! alist)
  (let ((cube (assoc/cdr 'cube alist)))
    (append-animator! (make-tile-animator (tile-delete cube)))))

(define (tile-handle-event! evt)
  (match evt
    (`(create . ,(alist . ())) (tile-handle-create-event! alist))
    (`(delete . ,(alist . ())) (tile-handle-delete-event! alist))))
