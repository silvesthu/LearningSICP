(define profile
  (let ((cache (make-hash)))  ; the cache memorizing call info
    (lambda (cmd . pargs)     ; parameters of profile procedure
      (case cmd
        ((def) (lambda args                  ; the function returned for 'def
                 (hash-update! cache (car pargs) add1 0) ; prepend cache update
                 (apply (cadr pargs) args))) ; call original procedure
        ((dmp) (hash-ref cache (car pargs))) ; return cache info for one procedure
        ((all) cache)                        ; return all cache info
        ((res) (set! cache (make-hash)))     ; reset cache
        ((allres) (display cache) (set! cache (make-hash))) ; show all and reset
        (else  (error "wot?"))))))           ; unknown parameter

(define-syntax define!
  (syntax-rules ()
    ((_ (name args ...) body ...)
     (define name (profile 'def 'name (lambda (args ...) body ...))))))