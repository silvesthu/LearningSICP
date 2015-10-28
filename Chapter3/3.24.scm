#lang scheme

(require scheme/mpair)

(define (make-table same-key?)
	(let ((local-table (mlist '*table*)))
		(define assoc ; return pair with same key in pair list
		  (lambda (key records)
		    (cond ((null? records) #f)
			  ((same-key? key (mcar (mcar records))) (mcar records))
			  (else (assoc key (mcdr records))))))
		(define (lookup key-1 key-2)
			(let ((subtable (assoc key-1 (mcdr local-table))))
				(if subtable
					(let ((record (assoc key-2 (mcdr subtable))))
						(if record
							(mcdr record)
							false))
				false)))
		(define (insert! key-1 key-2 value)
			(let ((subtable (assoc key-1 (mcdr local-table))))
				(if subtable
					(let ((record (assoc key-2 (mcdr subtable))))
						(if record
							(set-mcdr! record value)
							(set-mcdr! subtable
								(mcons (mcons key-2 value)
										(mcdr subtable)))))
					(set-mcdr! local-table
						(mcons (mlist key-1
									(mcons key-2 value))
								(mcdr local-table)))))
			'ok)
		(define (dispatch m)
			(cond 	((eq? m 'lookup-proc) lookup)
					((eq? m 'insert-proc!) insert!)
					(else (error "Unknown operation -- TABLE" m))))
	dispatch))

(define T (make-table (lambda (x y) (eq? x y))))
(define get (T 'lookup-proc))
(define put (T 'insert-proc!))

(put "a" "1" "value-a-1")
(get "a" "1")

(define T-float (make-table (lambda (x y) (or (< x (+ y 0.1)) (> x (- y 0.1))))))
(define get-float (T-float 'lookup-proc))
(define put-float (T-float 'insert-proc!))

(put-float 1.0 2.0 "1.0 2.0")
(put-float 1.01 2.01 "1.01 2.01")
(get-float 1.01 2.01)
(get-float 1.0 2.0) ; overwritten