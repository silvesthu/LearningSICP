#lang scheme

(define (square x) (* x x))

(define (divides? a b)
	(= (remainder b a) 0))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
			((divides? test-divisor n) test-divisor)
			(else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
	(find-divisor n 2))

; above from 1.21

(define (prime? n)
	(= n (smallest-divisor n)))

(define (timed-prime-test n)
	(newline)
	(display "Begin Test on ")
	(display n)
	(start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
	(let ((r (prime? n)))
		;(report-prime (- (current-milliseconds) start-time))
		(if r
			(report-prime (- (current-milliseconds) start-time))
			#f
		)
	)
)

(define (report-prime elapsed-time)
	(newline)
	(display "End Test")
	(newline)
	(display " *** ")
	(display elapsed-time)
	(newline)
  #t)

; found large prime here http://www.arachnoid.com/prime_numbers/
;(timed-prime-test 9007199254740881)
;(timed-prime-test 11)

; profiling function "time"
;(time (prime? 9007199254740881))

(define (search-for-prime n)
	(if (= (remainder n 2) 0) 
		(search-for-prime (+ n 1))
		(unless (timed-prime-test n) ; use when as "if only", use unless as "else only"
			(search-for-prime (+ n 2))
		)
	)
)

;(search-for-prime 1000)
;(search-for-prime 10000)
;(search-for-prime 100000)
;(search-for-prime 1000000)
;(search-for-prime 10000000)
; Can not take reasonable time on small number

(search-for-prime 1000000000000000) ; 577
(search-for-prime 10000000000000000) ; 1916
(search-for-prime 100000000000000000) ; 5975
(search-for-prime 1000000000000000000) ; 
