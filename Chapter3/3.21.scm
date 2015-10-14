#lang scheme

; why ? it print the whole list following front-ptr, then print rear-ptr

(define (print-queue queue)
	(if (pair? queue)
		(display (car queue))
		(error "not queue")
	)
)