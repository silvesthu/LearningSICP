#lang scheme

(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))))

(gcd 206 40)

; normal-order

; l0: gcd 206 40
; l1: gcd 40 (remainder 206 40), let P0 = (remainder 206 40) = 6
; l2: if P0, gcd P0 (remainder 40 P0), let P1 = (remainder 40 P0) = 4
; l3: if P1, gcd P1 (remainder P0 P1) let P2 = (remainder P0 P1) = 2
; l4: if P2, gcd P2 (remainder P1 P2) let P3 = (remainder P1 P2) = 0
; l5: if P3, <<< evaluate end, gcd P3 (remainder P2 P3)

(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
	(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
	; the code below will not be evalutated
	(gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

; Total
; 11 times of remainder evaluation

; -------------------

; applicative-order

; gcd 206 40
; gcd 40 (remainder 206 40)
; gcd 6 (remainder 40 6)
; gcd 4 (remainder 6 4)
; gcd 2 (remainder 4 2)
; end

; Total
; 4 times of remainder evaluation