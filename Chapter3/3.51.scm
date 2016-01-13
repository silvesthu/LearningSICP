#lang racket
(require racket/stream)

(define (show x)
	(display x)
	(newline)
	x
)

(define x (stream-map show (in-range 0 10)))

(stream-ref x 5)

; 5 print in show
; 5 print the return value

(stream-ref x 7)

; 7 print in show
; 7 print the return value

; nothing redundant is shown since stream-ref skip to specified element without call show on others