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

; 5. Reverse a list

; first define fold-left
(define (fold-left f init xs)
  (if (null? xs)
      init
      (fold-left f (f (car xs) init) (cdr xs))))

(define fold-left-tests
  (test-suite "Tests for fold-left"
              (check = (fold-left (lambda (init x) (+ init x)) 0 '(1 2 3 4 5 6 7 8 9 10)) 55)))

(run-tests fold-left-tests)

; now use fold-left to reverse the list
(define (my-reverse xs)
  (fold-left cons '() xs))

(define my-reverse-tests
  (test-suite "Tests for my-reverse"
              (check-equal? (my-reverse '(1 2 3 4 5 6 7 8 9 10)) '(10 9 8 7 6 5 4 3 2 1))))

(run-tests my-reverse-tests)

; 6. Find out whether a list is a palindrome
(define (my-is-palindrome xs)
  (equal? xs (reverse xs)))

(define my-is-palindrome-tests
  (test-suite "Tests for my-is-palindrome"
              (check-false (my-is-palindrome '(1 2 3 4 5 6 7 8 9 10)))
              (check-false (my-is-palindrome '(1 2 3 3 2 1 0)))
              (check-true (my-is-palindrome '(1 2 3 2 1)))
              (check-true (my-is-palindrome '(1 2 3 3 2 1)))))

(run-tests my-is-palindrome-tests)

; 7. Flatten a nested list structure.
(define (my-flatten xs)
  (cond ((null? xs) '())
        ((pair? xs) (append (my-flatten (car xs)) (my-flatten (cdr xs))))
        (else (list xs))))

(define my-flatten-tests
  (test-suite "Tests for my-flatten"
              (check-equal? (my-flatten (list (list 1 2 3) (list 4 5 6) (list (list 7 8) (list 9)) 10)) '(1 2 3 4 5 6 7 8 9 10))))

(run-tests my-flatten-tests)

; 8. Eliminate consecutive duplicates of list elements.
(define (my-compress xs)
  (cond ( (or (null? xs) (= (length xs) 1)) xs )
        ( (= (car xs) (cadr xs)) (my-compress (cdr xs)) )
        ( else (cons (car xs) (my-compress (cdr xs))))
  ))

(define my-compress-tests
  (test-suite "Tests for my-compress"
              (check-equal? (my-compress '(1 1 1 2 3 3 3 4 5 5 5 5 6 6)) '(1 2 3 4 5 6))
              (check-equal? (my-compress '(1 2 3 4 5 6)) '(1 2 3 4 5 6))))

(run-tests my-compress-tests)

; 9. Pack consecutive duplicates of list elements into sublists.
(define (my-pack xs)
  (foldr (lambda (x acc)
           (cond
             ( (null? (car acc)) (list (list x)) )
             ( (member x (car acc)) (cons (cons x (car acc)) (cdr acc)) )
             ( else (cons (list x) acc) ) ))
         '(())
         xs))

(define my-pack-tests
  (test-suite "Tests for my-pack"
              (check-equal? (my-pack '(1 1 1 1 2 3 3 1 1 4 5 5 5 5)) '((1 1 1 1) (2) (3 3) (1 1) (4) (5 5 5 5)))
              (check-equal? (my-pack '(1 2 3 4 5)) '((1) (2) (3) (4) (5)))
              (check-equal? (my-pack '()) '(()))))

(run-tests my-pack-tests)
