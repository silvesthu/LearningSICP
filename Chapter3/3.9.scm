#lang scheme

; recursive ver

;(define (factorial n)
;	(if (= n 1)
;		1
;		(* n (factorial (- n 1)))))

;(factorial 5)

; global env

; 	factorial

; E1 -> f : 5
; E2 -> f : 4
; ...

; ----------

(define (fact-iter product counter max-count)
	(if (> counter max-count)
		product
		(fact-iter  (* counter product)
					(+ counter 1)
					max-count)))

(define (factorial n)
	(fact-iter 1 1 n))

(factorial 5)

; global env

;	factorial, fact-iter

; E1 -> factorial : 5
; E2 -> fact-iter : 1 1 5
; E3 -> fact-iter : 1 2 5
; E4 -> fact-iter : 2 3 5
; ...