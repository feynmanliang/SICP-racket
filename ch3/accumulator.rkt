#lang planet neil/sicp

(define (make-accumulator count)
  (define (dispatch m)
    (begin (set! count (+ count m))
           count))
  dispatch)

(define acc (make-accumulator 5))
(acc 10)
(acc 10)