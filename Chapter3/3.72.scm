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


(define (TwoSquare-value pair)
  (define (i pair) (car pair))
  (define (j pair) (cadr pair))
  (+ (* (i pair) (i pair)) (* (j pair) (j pair)))
)

(define (TwoSquare-compare x y)
  (- (TwoSquare-value x) (TwoSquare-value y))
)

(define TwoSquare-ordered-pairs (pairs integers integers TwoSquare-compare))

(define (TwoSquare-stream-make pair)
  (define nexts (stream-rest pair))
  (define nexts-nexts (stream-rest nexts))
  (if (and (eq? (TwoSquare-value (stream-first pair)) (TwoSquare-value (stream-first nexts)))
          (eq? (TwoSquare-value (stream-first nexts)) (TwoSquare-value (stream-first nexts-nexts))))
    (stream-cons (TwoSquare-value (stream-first pair)) (TwoSquare-stream-make nexts))
    (TwoSquare-stream-make nexts)
  )
)

(define TwoSquare-stream (TwoSquare-stream-make TwoSquare-ordered-pairs))

(display "on ") (display 0) (display " = ") (stream-ref TwoSquare-stream 0) 
(display "on ") (display 1) (display " = ") (stream-ref TwoSquare-stream 1) 
(display "on ") (display 2) (display " = ") (stream-ref TwoSquare-stream 2) 
(display "on ") (display 3) (display " = ") (stream-ref TwoSquare-stream 3) 
(display "on ") (display 4) (display " = ") (stream-ref TwoSquare-stream 4) 