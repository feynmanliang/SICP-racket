#lang planet neil/sicp

(define (sqrt2 x)
  (begin
   (define (good-enough? guess)
     (< (abs (- (square guess) x)) 0.001))
   (define (improve guess)
     (average guess (/ x guess)))
   (define (sqrt-iter guess)
     (if (good-enough? guess)
         guess
         (sqrt-iter (improve guess))))
   (define (average y z)
     (/ (+ y z) 2))
   (define (square z)
     (* z z))
   (sqrt-iter 1.0)))