#lang scheme

; cuz all dependencies are in same direction, scenario those depending on each other won't happen

(define (serialized-exchange account1 account2)
	(let (
			(serializer1 (account1 'serializer))
			(serializer2 (account2 'serializer))
			(uid1 (account1 'uid))
			(uid2 (account2 'uid))
		)
		(if (uid1 < uid2)
			((serializer1 (serializer2 exchange)) account1 account2)
			((serializer2 (serializer1 exchange)) account1 account2)
		)
	)
)

(define (make-account balance)
	; ...
	(define uid-lock (make-mutex))
	(define (make-uid) ; or just let user to specify a universal id....
		(define next-uid 0)
		(uid-lock 'acquire)
		(let ((current-uid next-uid))
			(set! next-uid (+ next-uid 1))
			(uid-lock 'release)
			(current-uid)
		)		
	)
	(let 
		((balance-serializer (make-serializer)))
		((uid (make-uid)))
		(define (dispatch m)
			(cond 
				; ...
				((eq? m 'uid) uid)
				; ...
			)
		)
	)
)