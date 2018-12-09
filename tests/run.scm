(use test)

(define-syntax test-values
  (syntax-rules ()
    ((_ name expect (expr ...))
     (test name expect (call-with-values (lambda () (expr ...)) list)))))

(test-group
 "bsp"
 (include "tests/test-bsp.scm"))

(test-exit)
