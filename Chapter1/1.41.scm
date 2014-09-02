#lang scheme

(define (inc i) (+ i 1))

(inc 5)

(define (double p)
	(lambda (x) (p (p x))))

((double inc) 5)

(((double (double double)) inc) 5) ; 21

; first guess 9 but wrong...

; answer: 21

; Why ?

; even 2^2^2 + 5 = 13...

((double inc) 5); 7
(((double double) inc) 5); 9

; outside double -> inside double * 2 -> (inc * 2) * 2 -> 4
; 4 + 5 = 9

; so it's actually 2^2^2^2 + 5 = 21
; missed the inner-most double

