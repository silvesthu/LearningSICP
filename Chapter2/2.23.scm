#lang scheme

; (map (lambda (x) (newline) (display x)) (list 1 2 3 4 5))

(define (for-each action data)
	(map action data)
	(cond (#f #f)) ; do nothing
)

(for-each (lambda (x) (newline) (display x)) (list 1 2 3 4 5))