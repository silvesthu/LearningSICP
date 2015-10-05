#lang scheme

(define (mystery x)
	(define (loop x y)
		(if (null? x)
			y
			(let ((temp (cdr x)))
				(set-cdr! x y)
				(loop temp x))))
	(loop x '()))

; what mystery does

; -> reverse

; skip drawing...

; (define v (list 'a 'b 'c 'd))
; (define w (mystery v))
; v
; w

; Global : v = (a b c d)
; E1 (mystery) : x = v
; E2 (loop) : x = v, y = '()
; E3 (loop-let) : temp = (b c d), x = (a), y = '()
; E4 (loop) : x = (b c d), y = (a)
; ...
; (d c b a)