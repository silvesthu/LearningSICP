#lang scheme

; (include "2.18.scm")

(define (square-list-map items)
	(map (lambda (x) (* x x)) items)
)

(square-list-map (list 1 2 3 4 5))

; ---------------------------------

(define (square x) (* x x))

(define (square-list-22v1 items)
	(define (iter things answer)
		(if (null? things)
			answer
			(iter (cdr things)
				  (cons (square (car things))
				  		answer))))
	(iter items null))

(square-list-22v1 (list 1 2 3 4 5)) ; error -> produce reverse order (25 16 9 4 1)

(define (square-list-22v2 items)
	(define (iter things answer)
		(if (null? things)
			answer
			(iter (cdr things)
				  (cons answer
				  		(square (car things))))))
	(iter items null))

(square-list-22v2 (list 1 2 3 4 5)) ; error -> (((((() . 1) . 4) . 9) . 16) . 25)

; Correction

(define (square-list-22v3 items)
	(reverse (square-list-22v1 items))
)

(square-list-22v3 (list 1 2 3 4 5))

; POSSIBLE ANSWER ?
; list is constructed as a stack with cons
; while getting iterative process requires access to the inner-most element first
; so a reverse before or after is necessary
