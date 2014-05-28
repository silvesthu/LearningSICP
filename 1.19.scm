#lang scheme

(define COUNT 0)

(define (fib n)
	(fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
	(set! COUNT (+ COUNT 1))
	(cond ((= count 0) b)
		  ((even? count)
		   (fib-iter a
		   			 b
		   			 (+ (* p p) (* q q))
		   			 (+ (* q q) (* 2 p q))
		   			 (/ count 2)))
		   (else (fib-iter (+ (* b q) (* a q) (* a p))
		   				   (+ (* b p) (* a q))
		   				   p
		   				   q
		   				   (- count 1)))))

; O(logn) steps

(set! COUNT 0)(display "Result = ")(fib 20)(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(fib 40)(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(fib 80)(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(fib 160)(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(fib 320)(display "Count = ")(display COUNT)(newline)

; Solution : 	just expand the hint.
; 				a <- bq + aq + ap
;				b <- bp + aq
;
;				replace a,b into the same formula again
;				a <- b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2)
;				b <- b(p^2 + q^2) + a(q^2 + 2pq)
;
;				which is exact the same form
;
;				then we know p' = p^2 + q^2 , q' = q^2 + 2pq

; Test 

;(fib 0)
;(fib 1)
;(fib 2)
;(fib 3)
;(fib 4)
;(fib 5)
;(fib 6)
;(fib 7)
;(fib 8)

; fib : 0 1 1 2 3 5 8 13 21

; -------------------

; Haven't figure out why that weird form is correct... maybe look it up later

; Have a basic idea here. But don't know how it connected to the p,q form

; f(n+2) = f(n+1) + f(n)
; f(n+3) = (f(n+1) + f(n)) + f(n+1)
; ...
; f(n+x) = f(n+1)f(x) + f(x-1)f(n)
; f(n+n) = f(n+1)f(n) + f(n-1)f(n) ; -> which means if we have 3 in a row, we can double it

; e.g.
(- 
	(fib 200) 
	(* (fib 99)(fib 100)) 
	(* (fib 101)(fib 100))
) ; = 0

; THEN .... ? HOW TO GET TO f(n/2) ?