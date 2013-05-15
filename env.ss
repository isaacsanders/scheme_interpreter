(define extend-env cons)

(define-datatype procedure procedure?
                 [primitive
                   (id symbol?)]
                 [closure
                   (formals formals?)
                   (bodies (list-of expression?))
                   (env list?)])

(define apply-env
  (lambda (env depth position)
    (list-ref (list-ref env depth) position)))
