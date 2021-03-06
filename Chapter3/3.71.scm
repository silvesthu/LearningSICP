#lang racket
(require racket/stream)

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                    (interleave s2 (stream-rest s1)))
  )
)

(define (merge-weighted s1 s2 w)
  (cond 
    ((stream-empty? s1) s2)
    ((stream-empty? s2) s1)
    (else 
      (let 
        ((s1car (stream-first s1))
         (s2car (stream-first s2)))
        (cond 
          ((< (w s1car s2car) 0)
            (stream-cons s1car (merge-weighted (stream-rest s1) s2 w)))
          (else
            (stream-cons s2car (merge-weighted s1 (stream-rest s2) w)))
        )
      )
    )
  )
)

(define (pairs s t w)
  (stream-cons
    (list (stream-first s) (stream-first t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-first s) x))
        (stream-rest t)
      )
      (pairs (stream-rest s) (stream-rest t) w)
      w
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
(define (pairs-compare x y)
  (- (+ (car x) (cadr x)) (+ (car y) (cadr y))))


(define (Ramanujan-value pair)
  (define (i pair) (car pair))
  (define (j pair) (cadr pair))
  (+ (* (i pair) (i pair) (i pair)) (* (j pair) (j pair) (j pair)))
)

(define (Ramanujan-compare x y)
  (- (Ramanujan-value x) (Ramanujan-value y))
)

(define Ramanujan-ordered-pairs (pairs integers integers Ramanujan-compare))

(define (Ramanujan-stream-make pair)
  (define nexts (stream-rest pair))
  (if (eq? (Ramanujan-value (stream-first pair)) (Ramanujan-value (stream-first nexts)))
    (stream-cons (Ramanujan-value (stream-first pair)) (Ramanujan-stream-make nexts))
    (Ramanujan-stream-make nexts)
  )
)

(define Ramanujan-stream (Ramanujan-stream-make Ramanujan-ordered-pairs))

(display "on ") (display 0) (display " = ") (stream-ref Ramanujan-stream 0) 
(display "on ") (display 1) (display " = ") (stream-ref Ramanujan-stream 1) 
(display "on ") (display 2) (display " = ") (stream-ref Ramanujan-stream 2) 
(display "on ") (display 3) (display " = ") (stream-ref Ramanujan-stream 3) 
(display "on ") (display 4) (display " = ") (stream-ref Ramanujan-stream 4) 