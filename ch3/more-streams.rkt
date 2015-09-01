#lang racket

(require racket/stream)
(require math/number-theory)

; sieve of Eratosthenes
(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define (sieve stream)
  (stream-cons
   (stream-first stream)
   (sieve (stream-filter
           (λ (x) (not (divides? (stream-first stream) x)))
           (stream-rest stream)))))
(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 10)

; iterations as streams
(define (average x y) (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
                 (stream-map (λ (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

; accelerating convergence using streams
; pi / 4 = 1 - 1/3 + 1/5 - 1/7 + ...
(define (pi-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define (partial-sums stream)
  (stream-cons (stream-first stream)
               (stream-map (λ (x) (+ (stream-first stream) x))
                           (partial-sums (stream-rest stream)))))
(define pi-stream
  (stream-map (λ (x) (* 4 x)) (partial-sums (pi-summands 1))))

(stream-ref pi-stream 10)

(define (square x) (* x x))
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-rest s)))))
(stream-ref (euler-transform pi-stream) 10)

(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-first
              (make-tableau transform s)))
(stream-ref (accelerated-sequence euler-transform pi-stream) 9)