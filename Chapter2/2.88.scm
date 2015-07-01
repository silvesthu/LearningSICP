
#lang scheme

(include "./algebra.scm")

; we already have many "sub"s....
; why we need a negate ?

; let me try extend add for sub...

(define yp11 (make-polynomial 'y (list (list 1 1))))
(define yp21 (make-polynomial 'y (list (list 2 1))))
;(add yp11 yp11)
(sub yp11 yp21)
(sub yp21 yp21)