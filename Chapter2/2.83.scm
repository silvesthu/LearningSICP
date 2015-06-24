#lang scheme

; raise in type tower

; integer(number), rational, real, complex

(include "./algebra.scm")

(apply-generic 'raise 7)
(apply-generic 'raise (apply-generic 'raise 7))
(apply-generic 'raise (apply-generic 'raise (apply-generic 'raise 7)))

(newline)