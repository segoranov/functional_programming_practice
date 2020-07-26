#lang racket
(require rackunit rackunit/text-ui)

(define (selectList l1 l2)
  (if (>= (length l2) (length l1)) l2 l1))

(define (sumMaxRoots f ll)
  (apply +
         (foldl selectList
                '()
                (map
                 (lambda (l)
                   (filter
                    (lambda (x) (= (f x) 0))
                    l))
                 ll))))

(define sumMaxRoots-test
  (test-suite "Test sumMaxRoots"
              (check = (sumMaxRoots (lambda (x) (-(* x x x) x)) '((1 2 3) (-1 0 5) (1 4 -1))) -1))) ; f(x) = x^3 - x

(run-tests sumMaxRoots-test)
