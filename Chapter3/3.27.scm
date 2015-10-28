#lang scheme

(require scheme/mpair)

(define (make-entry name type data)
	(mcons (mcons name type) (mcons data '()))
)
(define (name-entry entry) (mcar (mcar entry)))
(define (set!-name-entry entry value) (set-mcar! (mcar entry) value))
(define (type-entry entry) (mcdr (mcar entry)))
(define (set!-type-entry entry value) (set-mcdr! (mcar entry) value))
(define (data-entry entry) (mcar (mcdr entry)))
(define (set!-data-entry entry value) (set-mcar! (mcdr entry) value))
(define (next-entry entry) (mcdr (mcdr entry)))
(define (set!-next-entry entry value) (set-mcdr! (mcdr entry) value))
(define (find-entry name type root)
	(if (null? root) 
		'()
		(if (and (eq? (name-entry root) name) (eq? (type-entry root) type)) 
			root
			(find-entry name type (next-entry root))
		)
	)
)

(define (make-table)
	(let ((local-table (make-entry 'root 'root '())))
		; let's make ourselves a struct first
		(define (peek)
			(display local-table)
		)
		(define (insert!-dir name subroot)
			(if (null? (find-entry name 'dir subroot))
				(let (
						(next (next-entry subroot))
					  	(new-entry (make-entry name 'dir '()))
					  	(dir-root (make-entry name 'root '()))
					)
					(set!-data-entry new-entry dir-root)
					(set!-next-entry subroot new-entry)
					(set!-next-entry new-entry next)
					dir-root
				)
				(data-entry (find-entry name 'dir subroot)) ; find twice...
			)
		)
		(define (insert!-file name value subroot)
			;(display "Finding ") (display name) (display " ") (display value) (display " ") (display subroot) (newline)
			(if (null? (find-entry name 'file subroot))
				(let (
						(next (next-entry subroot))
					  	(new-entry (make-entry name 'file value))
					)
					(set!-next-entry subroot new-entry)
					(set!-next-entry new-entry next)
					new-entry
				)
				(error "duplication " name value)				
			)
		)
		(define (insert!-inner keys value subroot)
			(let (
				  (key 
					(if (null? keys) 
						#f
					    (mcar keys)))
				  (keys-left
				  	(if (null? keys) 
				  		#f
				  		(mcdr keys)))
				)
				(if (null? keys-left)
					(insert!-file key value subroot)
					(insert!-inner (mcdr keys) value (insert!-dir key subroot))
				)
			)
		)
		(define (insert! keys value) (insert!-inner keys value local-table))
		(define (lookup-inner keys subroot)
			;(display "Looking up ") (display keys) (display " ") (display subroot) (newline)
			(let (
				  (key 
					(if (null? keys) 
						#f
					    (mcar keys)))
				  (keys-left
				  	(if (null? keys) 
				  		#f
				  		(mcdr keys)))
				)
				(if (null? keys-left)
					(let ((found (find-entry key 'file subroot)))
						(if (null? found)
							#f
							(data-entry found)
						)
					)
					(let ((found (find-entry key 'dir subroot)))
						(if (null? found)
							#f
							(lookup-inner keys-left (data-entry found))
						)
					)	
				)
			)
		)
		(define (lookup keys)
			(lookup-inner keys local-table)
		)
		(define (dispatch m)
			(cond 
				((eq? m 'lookup-proc) lookup)
				((eq? m 'insert-proc!) insert!)
				((eq? m 'peek) peek)
				(else (error "unknown command"))
			)
		)
		dispatch
	)
)

; from 3.25 above

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memorize f)
  (let ((table (make-table)))
    (let ((get (table 'lookup-proc))
          (put (table 'insert-proc!)))
      (lambda (x)
        (let ((previously-computed-result (get (mlist x))))
          (or previously-computed-result
              (let ((result (f x)))
                (put (mlist x) result)
                result)))))))

(define memo-fib
  (memorize (lambda (n)
              (cond ((= n 0) 0)
                    ((= n 1) 1)
                    (else (+ (memo-fib (- n 1))
                             (memo-fib (- n 2))))))))

(time (fib 35))
(time (memo-fib 35)) ; duplicate computation is eliminated, every number is calculated only once -> O(n)

(define memo-fib-2 (memorize fib))
(time (memo-fib-2 35)) 
; this work out correct result, but memorization is not working
; since fib recursively calls fib after entering memo-fib-2, memorize will be only called once for the last result

; (memo-fib 3)
; (memorize f) | x = 3, table = new table
; f = 3 | table = new table
; f = 2 | table = new table
; f = 1 | table = 1/1
; f = 0 | table = 0/0
; f = 1 | table = 0/0 1/1 (cache read)
; f = 2 | table = 0/0 1/1 2/2
; f = 3 | table = 0/0 1/1 2/2 3/3
