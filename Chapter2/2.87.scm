#lang scheme

(include "./algebra.scm")

; embeded into header
; and since I handle scheme-number as it is... so eq? 0 will be enough

(define yp (make-polynomial 'y (list (list 1 1))))
yp
(add yp yp)
(define xp (make-polynomial 'x (list (list 1 yp))))
xp
(add xp xp)