#lang scheme

;(require srfi/1)
(require racket/base)

(define (rand command . args)
	(cond 
		((eq? command 'generate) (random))
		((eq? command 'reset) (lambda (x) (random-seed x)))
	)
)

((rand 'reset) 117) ; set interal state (like a seed)
(rand 'generate)
(rand 'generate)

((rand 'reset) 117) ; set interal state (like a seed)
(rand 'generate)
(rand 'generate)

