#lang racket
(require racket/stream)

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                    (interleave s2 (stream-rest s1)))
  )
)

(define (pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
      t)
    (pairs (stream-rest s) (stream-rest t))
  )
)

(define (make-integers)
  (define (make-integers-inner i)
    (stream-cons i (make-integers-inner (+ i 1)))
  )
  (make-integers-inner 1)
)

(define integers (make-integers))

(define integer-pairs (pairs integers integers))

; it will run infinitily as soon as the pairs function is evaluated
; cuz no lazy-evalution in the main route. pairs -> interleave -> pair -> interleave -> ...