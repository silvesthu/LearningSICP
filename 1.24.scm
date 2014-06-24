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
	;(newline)
	;(display "Begin Test on ")
	;(display n)
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
	;(newline)
	;(display "End Test")
	;(newline)
	;(display " *** ")
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

(define (for-each-begin-end begin end next action)
	(when (not (= begin end))
	 	 ;(display begin)
	 	 ;(display " ")
		 (action begin)
		 (for-each-begin-end (next begin) end next action)
	)
)

(define (next i) (* i 10))

(for-each-begin-end 10000000000 100000000000000000000000000000000000000000000000 next search-for-prime)
(display ">>>>>>") (newline)
(for-each-begin-end 10000000000 100000000000000000000000000000000000000000000000 next search-for-prime)
;(search-for-prime 10000000000000)

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

; extra : random-integer with out limitation 
; ref: http://practical-scheme.net/gauche/man/gauche-refj_118.html
; ref: http://www.billthelizard.com/2010/02/sicp-exercise-124-fermat-test.html

;1E+10	2.00390625
;1E+11	2.521972656
;1E+12	2.768798828
;1E+13	20.80004883 // weird result, disappear on second run
;1E+14	4.001220703
;1E+15	3.065185547
;1E+16	4.645996094
;1E+17	3.897949219
;1E+18	3.968994141
;1E+19	3.58203125
;1E+20	11.51782227
;1E+21	12.49609375
;1E+22	11.88110352
;1E+23	12.94604492
;1E+24	13.57397461
;1E+25	12.74584961
;1E+26	14.50415039
;1E+27	15.66992188
;1E+28	16.20703125
;1E+29	18.07202148
;1E+30	17.91113281
;1E+31	18.25610352
;1E+32	19.48486328
;1E+33	22.04101563
;1E+34	21.09594727
;1E+35	21.37304688
;1E+36	23.37597656
;1E+37	22.57495117
;1E+38	22.95092773
;1E+39	26.25415039
;1E+40	27.58105469
;1E+41	29.03100586
;1E+42	29.6340332
;1E+43	29.33105469
;1E+44	34.47094727
;1E+45	31.35107422
;1E+46	31.921875