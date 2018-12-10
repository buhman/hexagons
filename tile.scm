;; there is no animation for tiles, but tile mutations need to be ordered other
;; animated event types

(define (make-tile-animator fn)
  (make-animator
   'tile
   #f
   #f
   (lambda () (cons identity #f))
   #f
   fn))

(define (tile-create tile)
  (lambda ()
    (let* ((t-map (state-tile-map (*state*)))
           (t-kd (state-tile-kd (*state*)))
           (cube (tile-cube tile))
           (a-vec (cube->axial-vector cube)))
      ;; kd-insert! could return an entirely new tree
      (set! (state-tile-kd (*state*)) (kd-insert! 2 t-kd a-vec))
      (hash-table-set! t-map cube tile))))

(define (tile-delete cube)
  (lambda ()
    (let ((t-map (state-tile-map (*state*)))
          (t-kd (state-tile-kd (*state*)))
          (a-vec (cube->axial-vector cube)))
      ;; kd-remove! could return an empty tree
      (set! (state-tile-kd (*state*)) (kd-remove! 2 t-kd a-vec))
      (hash-table-delete! t-map cube))))

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
