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
  (stream-cons
    (list (stream-first s) (stream-first t))
    (interleave
      (stream-map (lambda (x) (list (stream-first s) x))
        (stream-rest t)
      )
      (pairs (stream-rest s) (stream-rest t))
    )
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

(define (triplet s t u)
  (stream-cons
    (list (stream-first s) (stream-first t) (stream-first u))
    (interleave
      (stream-map (lambda (x) (cons (stream-first s) x))
                (stream-rest (pairs t u)))
      (triplet (stream-rest s) (stream-rest t) (stream-rest u))
    )
  )
)

(define integer-triplets (triplet integers integers integers))

(define (square x) (* x x))
(define (pythgorean-triplet? xyz) 
  (eq? 
    (+ (square (car xyz)) (square (cadr xyz))) 
    (square (caddr xyz))
  )
)

(define pythgorean-triplets 
  (stream-filter 
    (lambda (x) (pythgorean-triplet? x))
    integer-triplets
  ))

; need some other work to rule out non-primitive triplet...

(display "on ") (display 0) (display " = ") (stream-ref pythgorean-triplets 0) 
(display "on ") (display 1) (display " = ") (stream-ref pythgorean-triplets 1) 
(display "on ") (display 2) (display " = ") (stream-ref pythgorean-triplets 2) 
(display "on ") (display 3) (display " = ") (stream-ref pythgorean-triplets 3) 
(display "on ") (display 4) (display " = ") (stream-ref pythgorean-triplets 4) 
(display "on ") (display 5) (display " = ") (stream-ref pythgorean-triplets 5) 


