#lang scheme

(define table 1)
(set! table '())

; with a simple list of list (not a table actually...)

(define (put op type item) (set! table (append table (list (list op type item)))))
(define (get op type) 
	(define (get-inner op type t)
		;(display t) (newline)
		(cond 
			((null? t) null)
			((and (equal? op (caar t)) (equal? type (cadar t))) (caddar t))
			(else (get-inner op type (cdr t)))
		)
	)
	(get-inner op type table)
)

; a.

; file should be attached with a division-tag
(define (get-division-tag file)
	(car file)
)

(define (get-record key file)
	((get 'get-record (get-division-tag file)) key file)
)

; each division should provide a register method like this to install functions
(define (register-division-A)
	(define (get-record key file) (newline))
	(put 'get-record 'division-tag-A get-record)
)

; b.

; attach division-tag to every record

(define data 1)

(define (register-division-B)
	(define (attach-division tag data) (cons tag data))
	(define (get-record key file) (attach-division 'division-tag-B data))
	(define (get-salary record) data)
	(put 'get-record 'division-tag-B get-record)
	(put 'get-salary 'division-tag-B get-salary)
)

; c. something like this...

(define (find-employee-record name files)
	(define (find-employee-record-inner name files)
		(append (get-record name (car files)) (find-employee-record-inner (cdr files)))
	)
	(find-employee-record-inner name files)
)

; d. call to new company's register function