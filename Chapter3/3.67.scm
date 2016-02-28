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
      (interleave
        (stream-map (lambda (x) (list (stream-first s) x))
          (stream-rest t)
        )
        (stream-map (lambda (x) (list x (stream-first s)))
          (stream-rest t)
        )
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

; no any restriction ?
; then interleave the stream of reverse pair should work

(display "on ") (display 0) (display " = ") (stream-ref integer-pairs 0) 
(display "on ") (display 1) (display " = ") (stream-ref integer-pairs 1) 
(display "on ") (display 2) (display " = ") (stream-ref integer-pairs 2) 
(display "on ") (display 3) (display " = ") (stream-ref integer-pairs 3) 
(display "on ") (display 4) (display " = ") (stream-ref integer-pairs 4) 
(display "on ") (display 5) (display " = ") (stream-ref integer-pairs 5) 
(display "on ") (display 6) (display " = ") (stream-ref integer-pairs 6) 
(display "on ") (display 7) (display " = ") (stream-ref integer-pairs 7) 
(display "on ") (display 8) (display " = ") (stream-ref integer-pairs 8) 
(display "on ") (display 9) (display " = ") (stream-ref integer-pairs 9) 
(display "on ") (display 10) (display " = ") (stream-ref integer-pairs 10) 
(display "on ") (display 11) (display " = ") (stream-ref integer-pairs 11) 
(display "on ") (display 12) (display " = ") (stream-ref integer-pairs 12) 
(display "on ") (display 13) (display " = ") (stream-ref integer-pairs 13) 
(display "on ") (display 14) (display " = ") (stream-ref integer-pairs 14) 
(display "on ") (display 15) (display " = ") (stream-ref integer-pairs 15) 
(display "on ") (display 16) (display " = ") (stream-ref integer-pairs 16) 
(display "on ") (display 17) (display " = ") (stream-ref integer-pairs 17) 
(display "on ") (display 18) (display " = ") (stream-ref integer-pairs 18) 
(display "on ") (display 19) (display " = ") (stream-ref integer-pairs 19) 