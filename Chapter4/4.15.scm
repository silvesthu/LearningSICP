#lang scheme

(define (run-forever) (run-forever))

(define (halts? p a)
	...)

(define (try p)
	(if (halts? p p)
		(run-forever)
		'halted))

(try try)

; if (halts? try) give #f then try return 'halted which violate the #f result
; if (halts? try) give #t then try never return which violate the #t result