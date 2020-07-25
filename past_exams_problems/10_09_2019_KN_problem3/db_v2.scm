#lang racket
(require rackunit rackunit/text-ui)

(define (addIfNew x xs)
  (if (member x xs)
      xs
      (cons x xs)))

(define addIfNew-tests
  (test-suite "Tests for addIfNew"
              (check-equal? (addIfNew 1 '(2 3 4 5)) '(1 2 3 4 5))
              (check-equal? (addIfNew 1 '(2 3 1 4 5)) '(2 3 1 4 5))))

(run-tests addIfNew-tests)

(define (annotate db annotators)
  (map (lambda (annotated-data)
         (let ((data (car annotated-data))
               (labels (cdr annotated-data)))
           (cons data
                 (foldl
                  (lambda (label acc) (addIfNew label acc))
                  labels
                  (apply append (map (lambda (annotator) (annotator data)) annotators)))))
         )
       db))

; test-data
(define db (list (cons "scheme" (list (cons "typing" "dynamic") (cons "evaluation" "strict")))
                 (cons "haskell" (list (cons "typing" "static")))
                 (cons "c++" (list))))

(define (evaluation lang)
  (case lang (("scheme") (list (cons "evaluation" "strict") (cons "macros" "true")))
             (("haskell") (list (cons "evaluation" "lazy")))
             (("c++") (evaluation "scheme"))))

(define (purity lang)
  (if (eqv? lang "haskell")
      (list (cons "pure" "true"))
      (list)))


(define db-test
  (test-suite "Tests updating of db"
              (check-equal? (annotate db (list evaluation purity))
                            (list (cons "scheme" (list (cons "macros" "true") (cons "typing" "dynamic") (cons "evaluation" "strict")))
                                  (cons "haskell" (list (cons "pure" "true") (cons "evaluation" "lazy") (cons "typing" "static")))
                                  (cons "c++" (list (cons "macros" "true") (cons "evaluation" "strict")))))))

(run-tests db-test)
