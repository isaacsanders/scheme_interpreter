(load "chez-init.ss")

(define (scheme-value? x) #t)

(define-datatype environment environment?
                 [lexically-addressed-environment
                   (env (list-of scheme-value?))])

; (define empty-env
;   (lambda ()
;     (empty-env-record)))

(define extend-env
  (lambda (vals env)
    (cases environment env
           [lexically-addressed-environment (env)
                                            (lexically-addressed-environment (cons vals env))])))

(define-datatype procedure procedure?
                 [primitive
                   (id symbol?)]
                 [closure
                   (formals formals?)
                   (bodies (list-of expression?))
                   (env environment?)])

(define apply-env
  (lambda (env depth position)
    (cases environment env
           [lexically-addressed-environment (env)
                                            (list-ref (list-ref env depth) position)])))