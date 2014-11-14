#lang scheme

(define (fringe root)
	(newline)
)

(define x (list (list 1 2) (list 3 4)))

(fringe x)

(fringe (list x x))