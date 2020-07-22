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

; 2. find the last but one element of a list
(define (my-but-last xs)
  (when (not (null? (cdr xs)))
    (if (null? (cddr xs))
        (car xs)
        (my-but-last (cdr xs)))))

(define my-but-last-tests
  (test-suite "Tests for my-but-last"
              (check = (my-but-last '(1 2 3)) 2)))

(run-tests my-but-last-tests)
