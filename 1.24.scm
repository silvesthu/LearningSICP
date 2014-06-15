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

(define (expmod base exp m)
	(cond ((= exp 0) 1)
				((even? exp)
		 		 (remainder (square (expmod base (/ exp 2) m)) m))
				(else
		 		 (remainder (* base (expmod base (- exp 1) m)) m))))

(define (big-random n)
	(random (min n 4294967087)))

(define (fermat-test n)
  (define (try-it a)
  	(= (expmod a n n) a))
  (try-it (+ 1 (big-random (- n 1)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
				((fermat-test n) (fast-prime? n (- times 1)))
				(else false)))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (timed-prime-test n)
	(newline)
	(display "Begin Test on ")
	(display n)
	(start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
	;(let ((r (prime? n)))
	(let ((r (fast-prime? n n)))
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

; 1.22

; (search-for-prime 1000000000000000) ; 1000000000000037 : 577
; (search-for-prime 10000000000000000) ; 10000000000000061 : 1916
; (search-for-prime 100000000000000000) ; 100000000000000003 : 5975
; (search-for-prime 1000000000000000000) ; 1000000000000000003 : 20424

; random by 1000 times

;(search-for-prime 1000000000000000) ; 1000000000000037 : 47
;(search-for-prime 10000000000000000) ; 10000000000000061 : 34 
;(search-for-prime 100000000000000000) ; 100000000000000003 : 45
;(search-for-prime 1000000000000000000) ; 1000000000000000003 : 36

; random by 100000 times

;(search-for-prime 1000000000000000) ; 1000000000000037 : 3643
;(search-for-prime 10000000000000000) ; 10000000000000061 : 3920 
;(search-for-prime 100000000000000000) ; 100000000000000003 : 4055
;(search-for-prime 1000000000000000000) ; 1000000000000000003 : 4310

; Since the amount of try is a fixed number,
; the observed time is on same order for a fixed number (finish all tries)

; so it fits the O(logn) ?
; since n >> 10, O(logn) and O(1) gives the similar result.
; further reading :
; http://stackoverflow.com/questions/1491795/olog-n-o1-why-not

; random by n times => if n varies depending on n, it need some other explaination.

(search-for-prime 10000) ; 10007 : 8
(search-for-prime 100000) ; 100003 : 107 
(search-for-prime 1000000) ; 1000003 : 1164
(search-for-prime 10000000) ; 10000019 : 14083
