#lang racket
(require racket/stream)

(define (expand num den radix)
	(stream-cons
		(quotient (* num radix) den)
		(expand (remainder (* num radix) den) den radix)))

(quotient (* 1 10) 7) ; 10 / 7
(quotient (* (remainder (* 1 10) 7) 10) 7) ; 30 / 7
; 20 / 7
; 60 / 7
; 40 / 7
; 50 / 7
; 10 / 7
; ....

; it is the division on base radix
; 10.000000 / 7
; = 1.0 * 7 + 3.0
; = 1.0 * 7 + 0.4 * 7 + 0.2
; = ...

(newline)

(define S (expand 1 7 10))

(stream-ref S 0)
(stream-ref S 1)
(stream-ref S 2)
(stream-ref S 3)
(stream-ref S 4)
(stream-ref S 5)
(stream-ref S 6)
(stream-ref S 7)
(stream-ref S 8)
(stream-ref S 9)
(stream-ref S 10)

(newline)

(set! S (expand 3 8 10))

(stream-ref S 0)
(stream-ref S 1)
(stream-ref S 2)
(stream-ref S 3)
(stream-ref S 4)
(stream-ref S 5)
(stream-ref S 6)
(stream-ref S 7)
(stream-ref S 8)
(stream-ref S 9)
(stream-ref S 10)

