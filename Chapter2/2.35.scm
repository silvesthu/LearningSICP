#lang scheme

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))
	)
)

(define (count-leaves t)
	(accumulate + 0 (map 
		(lambda (x) 
			(cond ((null? x) 0)
				  ((not (pair? x)) 1)
				  (else (count-leaves x))
			)
		) t))
)

(count-leaves null)
(count-leaves (list))
(count-leaves (list 1 2))
(count-leaves (list 1 2 (list 2 3 (list 4 5))))
(count-leaves (list (list 1 2) (list 1 2 3)))

; compare to 2.2.2

(define (count-leaves-2.2.2 x)
   (cond ((null? x) 0)
         ((not (pair? x)) 1)
         (else (+ (count-leaves-2.2.2 (car x))
                  (count-leaves-2.2.2 (cdr x))))))

; not so much different ?