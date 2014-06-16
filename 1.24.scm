#lang scheme

(require profile)
(require srfi/27)

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

(define (fermat-test n)
  (define (try-it a)
  	(= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

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
	(start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
	;(let ((r (prime? n)))
	(let ((r (fast-prime? n 100)))
		;(report-prime (- (current-inexact-milliseconds) start-time))
		(if r
			(report-prime (- (current-inexact-milliseconds) start-time))
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

; random by 100 times

;(search-for-prime 10000) ; 0.094970703125
;(search-for-prime 10000000) ; 0.195068359375 		   
;(search-for-prime 10000000000) ; 2.10498046875	 
;(search-for-prime 10000000000000) ; 3.3251953125	 
;(search-for-prime 10000000000000000) ; 3.985107421875	 
;(search-for-prime 10000000000000000000) ; 4.18115234375

;(search-for-prime (expt 10 5 )) ; 0.11279296875
;(search-for-prime (expt 10 10)) ; 2.01708984375
;(search-for-prime (expt 10 20)) ; 11.430908203125
;(search-for-prime (expt 10 40)) ; 37.94091796875
;(search-for-prime (expt 10 80)) ; 88.363037109375
;(search-for-prime (expt 10 160)) ; 224.590087890625

;(search-for-prime (expt 2 100)) ; 15.14599609375
;(search-for-prime (expt 2 200)) ; 49.4599609375
;(search-for-prime (expt 2 400)) ; 103.661865234375
;(search-for-prime (expt 2 800)) ; 318.169189453125
;(search-for-prime (expt 2 1600)) ; 1320.11083984375

; why ?

; extra : random-integer with out limitation 
; ref: http://practical-scheme.net/gauche/man/gauche-refj_118.html
; ref: http://www.billthelizard.com/2010/02/sicp-exercise-124-fermat-test.html
