#lang racket
(require rackunit rackunit/text-ui)

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

; solution
(define (addIfNew x xs)
  (if (member x xs)
      xs
      (cons x xs)))

(define addIfNew-tests
  (test-suite "Tests for addIfNew"
              (check-equal? (addIfNew 1 '(2 3 4 5)) '(1 2 3 4 5))
              (check-equal? (addIfNew 1 '(2 3 1 4 5)) '(2 3 1 4 5))))

(run-tests addIfNew-tests)

(define (addNewToList xs ys)
  (if (null? ys)
      xs
      (addNewToList (addIfNew (car ys) xs) (cdr ys))))

(define addNewToList-tests
  (test-suite "Tests for addNewToList"
              (check-equal? (addNewToList '(1 2 3 4) '(5 4 2 1 69)) '(69 5 1 2 3 4))))

(run-tests addNewToList-tests)

(define (addNewFromAnnotator annotated-data annotator)
  (let (
        (data (car annotated-data)) ; "scheme"
        (labels (cdr annotated-data)) ; <<"typing";"dynamic">, ...>
        (list-to-add (annotator (car annotated-data)))
        )
    (cons data (addNewToList labels list-to-add))))

; works on each row of the db
(define (addNewFromAnnotators annotated-data annotators)
  (if (null? annotators)
      annotated-data
      (addNewFromAnnotators
       (addNewFromAnnotator annotated-data (car annotators))
       (cdr annotators))))

(define (annotate db annotators)
  (if (null? db)
      '()
      (append
       (list (addNewFromAnnotators (car db) annotators)) ; updated row
       (annotate (cdr db) annotators))))

(define db-test
  (test-suite "Tests updating of db"
              (check-equal? (annotate db (list evaluation purity))
                            (list (cons "scheme" (list (cons "macros" "true") (cons "typing" "dynamic") (cons "evaluation" "strict")))
                                  (cons "haskell" (list (cons "pure" "true") (cons "evaluation" "lazy") (cons "typing" "static")))
                                  (cons "c++" (list (cons "macros" "true") (cons "evaluation" "strict")))))))

(run-tests db-test)