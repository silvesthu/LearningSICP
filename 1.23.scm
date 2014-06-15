#lang scheme

(define (square x) (* x x))

(define (divides? a b)
	(= (remainder b a) 0))

(define (next test-divisor)
	(if (= test-divisor 2)
		3
		(+ test-divisor 2)
	)
)

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
			((divides? test-divisor n) test-divisor)
			; (else (find-divisor n (+ test-divisor 1)))))
			(else (find-divisor n (next test-divisor)))))

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
	(if (prime? n)
		(report-prime (- (current-milliseconds) start-time))
		#f
	)
)

(define (report-prime elapsed-time)
	(newline)
	(display "End Test")
	(newline)
	(display " *** ")
	(display elapsed-time)
	(newline)
  #t
)

(define (search-for-prime n)
	(if (= (remainder n 2) 0) 
		(search-for-prime (+ n 1))
		(unless (timed-prime-test n) ; use when as "if only", use unless as "else only"
			(search-for-prime (+ n 2))
		)
	)
)

; above from 2.22 except "next" and "smallest-divisor"

; result comparison

; 1.22

; (search-for-prime 1000000000000000) ; 577
; (search-for-prime 10000000000000000) ; 1916
; (search-for-prime 100000000000000000) ; 5975

; 1.23

(search-for-prime 1000000000000000) ; 577 -> 318
(search-for-prime 10000000000000000) ; 1916 -> 996
(search-for-prime 100000000000000000) ; 5975 -> 3247
(search-for-prime 1000000000000000000) ; 20424 -> 10060

; so,  about 2 times fast ?

; # from instructor's manual

; the ratio may depend on whether the "next" is internally implemented,
; as well as the version of scheme