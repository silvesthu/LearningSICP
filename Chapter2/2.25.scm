#lang scheme

(define A (list 1 3 (list 5 7) 9))
A
(car (cdaddr A))

(newline)

(define B (list (list 7)))
B
(caar B)

(newline)

; note : all sub lists are list.. they all contains an null end
(define C (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
C
(cadadr (cadadr (cadadr C)))