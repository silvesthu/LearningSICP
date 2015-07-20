#lang scheme

(require srfi/1)
(include "./algebra.scm")

; (1x^4+-1x^3+-2x^2+2x^1)
(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
; (1x^3+-1x^1)
(define p2 (make-polynomial 'x '((3 1) (1 -1))))
(apply-generic 'gcd p1 p2) ; (-1x^2+1x^1)


; http://www.wolframalpha.com/input/?i=%281x%5E4%2B-1x%5E3%2B-2x%5E2%2B2%5Ex1%29%2F%281x%5E3%2B-1x%5E1%29