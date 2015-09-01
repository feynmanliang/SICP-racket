#lang racket

(require racket/stream)

; hide internal state of RNG using streams
; use RNG to do MC estimate of Pi

(define random-numbers
  (stream-cons random-init
               (stream-map rand-update random-numbers)))

(define cesaro-stream
  (map-successive-pairs (λ (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))
(define (map-successive-pairs f s)
  (stream-cons
   (f (stream-first s) (stream-first (stream-rest s)))
   (map-successive-pairs f (stream-rest (stream-rest s)))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (stream-cons
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-rest experiment-stream) passed failed)))
  (if (stream-first experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (λ (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))