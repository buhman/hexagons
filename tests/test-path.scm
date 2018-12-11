(include "path.scm")

(define +graph+
  '((a . (b))
    (b . (a c d))
    (c . (a))
    (d . (e a))
    (e . (b))))

(define +search-graph+
  '((a . #f) (b . a) (c . b) (d . b) (e . d)))

(test-group
 "flood"

 (test "flood-search"
   '((a . #f) (b . a) (c . b) (d . b) (e . d))
   (flood-search 'a 'e +graph+ alist-ref))

 (test "flood-search exit"
   '((a . #f) (b . a))
   (flood-search 'a 'b +graph+ alist-ref))

 (test "flood-path"
   '(e d b a)
   (flood-path 'a 'e +search-graph+)))
