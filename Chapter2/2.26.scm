#lang scheme

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; (1 2 3 4 5 6) -> one list
(cons x y) ; ((1 2 3) 4 5 6) -> two list
(list x y) ; ((1 2 3) (4 5 6)) -> three list