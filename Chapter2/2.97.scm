#lang scheme

(require srfi/1)
(include "./algebra.scm")

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

; (x + 1) / (x^3 - 1) + x / (x^2 - 1)
;(apply-generic 'numer rf1)
;(apply-generic 'denom rf2)

;(apply-generic 'reduce p1 p1)


;(apply-generic 'mul (apply-generic 'numer rf1) (apply-generic 'denom rf2))

(newline)
(add rf1 rf2)

; Spent almost 2 hours to find out there is a bug in 'sub' impl....
; I forgot to apply op on L2 when L1 is empty term list

; More test case with a good test framework will make things better I suppose