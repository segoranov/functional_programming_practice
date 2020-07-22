#lang scheme
(require rackunit rackunit/text-ui)

; 1. Find last element of list
(define (my-last xs)
  (if (null? (cdr xs))
      (car xs)
      (my-last (cdr xs))))

(define my-last-tests
  (test-suite "Tests for my-last"
              (check = (my-last '(1 2 3)) 3)))

(run-tests my-last-tests)

; 2. Find the last but one element of a list
(define (my-but-last xs)
  (when (not (null? (cdr xs)))
    (if (null? (cddr xs))
        (car xs)
        (my-but-last (cdr xs)))))

(define my-but-last-tests
  (test-suite "Tests for my-but-last"
              (check = (my-but-last '(1 2 3)) 2)))

(run-tests my-but-last-tests)

; 3. Find the K'th element of a list.
(define (find-kth k xs)
  (if (= k 0)
      (car xs)
      (find-kth (- k 1) (cdr xs))))

(define find-kth-tests
  (test-suite "Tests for find-kth"
              (check = (find-kth 0 '(1 2 3)) 1)
              (check = (find-kth 1 '(1 2 2)) 2)
              (check = (find-kth 2 '(1 2 3)) 3)))

(run-tests find-kth-tests)

; 4. Find the number of elements of a list.
(define (my-length xs)
  (if (null? xs)
      0
      (+ 1 (my-length (cdr xs)))))

(define my-length-tests
  (test-suite "Tests for my-length"
              (check = (my-length '(1 2 3)) 3)))

(run-tests my-length-tests)