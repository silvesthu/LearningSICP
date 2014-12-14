#lang scheme

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))
	)
)

(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest))
				  (cdr rest)
			)
		)
	)
	(iter initial sequence)
)

;(fold-left cons null '(1 2 3))
;(accumulate cons null '(1 2 3))

(define fold-right accumulate)

(fold-right * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(fold-right list null (list 1 2 3))
(fold-left list null (list 1 2 3))

; property -> order of parameters for op does not affect result of op
; a / b != b / a
; (list a b) != (list b a)
; a * b = b * a
; maybe three or more parameters

; -----------------------
;　fold-left forms tail-recursion while fold-right does not
; switch position in op will let fold-left form similar result as form-right
; but iterate from the last element to the first one

; -----------------------
; wiki : http://en.wikipedia.org/wiki/Fold_(higher-order_function)

; a.k.a. accumulate, aggregate, reduce, etc.