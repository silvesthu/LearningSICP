#lang scheme

(define (make-frame variables values)
	(cons variables values)
)

; ↓

(define (make-frame variables values)
	(map (lambda (var val) (cons var val)) variables values)
)

; skip...

