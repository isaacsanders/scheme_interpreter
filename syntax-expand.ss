(load "parser.ss")

(define syntax-expand
  (lambda (expr)
    (cases expression expr
           (lambda-exp (formals bodies)
                       (lambda-exp formals (map syntax-expand bodies)))
           (let-exp (syms vals bodies)
                    (app-exp (lambda-exp (param-list syms)
                                         (map syntax-expand bodies))
                             (map syntax-expand vals)))
           (begin-exp (bodies)
                      (begin-exp (map syntax-expand bodies)))
           (if-exp (condition if-true)
                   (if-exp (syntax-expand condition)
                           (syntax-expand if-true)))
           (if-else-exp (condition if-true if-false)
                        (if-else-exp (syntax-expand condition)
                                     (syntax-expand if-true)
                                     (syntax-expand if-false)))
           (app-exp (operator operands)
                    (app-exp (syntax-expand operator)
                             (map syntax-expand operands)))
           (vector-exp (datum)
                       (vector-exp (map syntax-expand datum)))
           (else expr))))


