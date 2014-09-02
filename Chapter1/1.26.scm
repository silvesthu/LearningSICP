#lang scheme

(define (square x) (* x x))

(define (divides? a b)
	(= (remainder b a) 0))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
			((divides? test-divisor n) test-divisor)
			(else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
	(find-divisor n 2))

; above from 1.21

(define (expmod base exp m)
	(cond ((= exp 0) 1)
			 	((even? exp)
			 	 (remainder (* (expmod base (/ exp 2) m)
			 	 						   (expmod base (/ exp 2) m))
			 	 					m))
			 	(else
			 	 (remainder (* base (expmod base (- exp 1) m))
			 	 m))))

(expmod 2 100 100)

; the original expmod gives O(logn)
; expanding square to duplicate call to expmod
; results in a binary tree of recursive call to expmod
; which incresed n to 2^n (tree depth -> tree node count)
; combine O(logn) and O(2^n), gets O(n)



