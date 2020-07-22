#lang scheme
(require rackunit rackunit/text-ui)

; 1. find last element of list
(define (my-last xs)
  (if (null? (cdr xs))
      (car xs)
      (my-last (cdr xs))))

(define my-last-tests
  (test-suite "Tests for my-last"
              (check = (my-last '(1 2 3)) 3)))

(run-tests my-last-tests)

; 2. 