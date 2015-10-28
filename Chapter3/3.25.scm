#lang scheme

; something like a directory/file structure...

; It's really astonishing how easily I forget (a, (b, c)) is just same thing as (a, b, c) in pair-world...

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

;(define root (make-entry 'root 'root '()))
;(define a (make-entry 'a 'file "a"))
;(define b (make-entry 'b 'file "b"))
;(define c (make-entry 'c 'file "c"))
;(set!-next-entry root a)
;(set!-next-entry a b)
;(set!-next-entry b c)

;(display root) (newline)
;(name-entry root)
;(type-entry root)
;(data-entry root)
;(next-entry root)
;(find-entry 'c 'file root)

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
				((eq? m 'insert-proc) insert!)
				((eq? m 'peek) peek)
				(else (error "unknown command"))
			)
		)
		dispatch
	)
)

(define T (make-table))
(define get (T 'lookup-proc))
(define put (T 'insert-proc))

(put (mlist "a" "a" "a") "aaa")
(put (mlist "a" "b" "c" "d") "abcd")
(put (mlist "a" "b" "c" "c") "abcc")

((T 'peek))

(get (mlist "a" "b"))
(get (mlist "a" "a" "a"))
(get (mlist "a" "b" "c" "c"))
(get (mlist "a" "b" "c" "d"))

; ah... totally OOP