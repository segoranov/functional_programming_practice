#lang racket

; insertAt 5 69 '(1 2 3 4 5 6 7 8 9 10) -> '(1 2 3 4 5 69 6 7 8 9 10)
(define (insertAt n x xs)
  (append (take xs n) (list x) (drop xs n)))

; putEverywhere 1 '(2 3 4) -> '((1 2 3 4) (2 1 3 4) (2 3 1 4) (2 3 4 1))
(define (putEverywhere x xs)
  (map (lambda (placeToInsert) (insertAt placeToInsert x xs))
       (range 0 (+ 1 (length xs))))) ; in haskell [0..length xs]

; (permutations '(1 2 3)) -> '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
(define (permutations xs)
  (cond ((null? xs) '())
        ((= (length xs) 1) (list xs))
        (else (apply append
                     (map (lambda (perm) (putEverywhere (car xs) perm))
                          (permutations (cdr xs)))))))

; test
(permutations '(1 2 3))
; > '((1 2 3) (2 1 3) (2 3 1) (1 3 2) (3 1 2) (3 2 1)) -> correct.