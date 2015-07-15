#lang scheme

(include "./algebra.scm")

; http://www.wolframalpha.com/input/?i=%28x%5E7+-+1%29+%2F+%28x%5E2+-+1%29
(define A (make-polynomial 'x '((7 1) (0 -1))))
(define B (make-polynomial 'x '((2 1) (0 -1))))
(add A B)
(div A B)