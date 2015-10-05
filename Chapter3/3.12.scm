#lang scheme

(define x (list 'a 'b))

(define y (list 'c 'd))

(define z (append x y))

z

(cdr x) ; (b)

; (define w (append! x y))

; w ; (a b c d)

; (cdr x) ; (b c d)

; skip drawing...

; append! modified x (same as w), y remains intact, z remains intact either but different node from x (or w)


