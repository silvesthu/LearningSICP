#lang racket

(define (a-plus-abs-b a b)
	((if (> b 0) + -) a b))

; + / - as result of if selection

(a-plus-abs-b 1 2)
(a-plus-abs-b 1 -2)