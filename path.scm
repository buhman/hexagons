(use matchable)

(define (flood-search start-node graph neighbors)
  (let loop ((visited (list (cons start-node #f)))
             (frontier (list start-node)))
    (match frontier
      ((this-node . new-frontier)
       (let* ((unvisited (filter
                          (lambda (n) (not (assoc n visited)))
                          (neighbors this-node graph)))
              (came-from (map
                          (lambda (n) (cons n this-node))
                          unvisited)))
         (loop
          (append visited came-from)
          (append new-frontier unvisited))))
      (() visited))))

(define (flood-path start-node target-node search-graph)
  (and-let* ((find (assoc target-node search-graph)))
    (match find
      ((_ . next-node)
       (if (equal? next-node start-node)
         (list target-node next-node)
         (cons target-node (flood-path start-node next-node search-graph)))))))
