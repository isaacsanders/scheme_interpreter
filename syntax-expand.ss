(load "parser.ss")

(define syntax-expand
  (lambda (expr)
    (cases expression expr
           (lambda-exp (formals bodies)
                       (lambda-exp formals (map syntax-expand bodies)))
           (let-exp (syms vals bodies)
                    (syntax-expand (app-exp (lambda-exp (param-list syms)
                                          bodies)
                             vals)))
           (let*-exp (syms vals bodies)
                     (syntax-expand (cond
                                      ((null? syms) (begin-exp bodies))
                                      ((null? (cdr syms)) (app-exp (lambda-exp (param-list (list (car syms))) bodies)
                                                                   (list (car vals))))
                                      (else (app-exp (lambda-exp (param-list (list (car syms)))
                                                                 (list (let*-exp (cdr syms)
                                                                                 (cdr vals)
                                                                                 bodies)))
                                                     (list (car vals)))))))
           (begin-exp (bodies)
                      (begin-exp (map syntax-expand bodies)))
           (cond-exp (conds cond-trues)
                     (syntax-expand (let [[first-cond (car conds)]
                                          [first-cond-true (car cond-trues)]]
                                      (cond
                                        ((equal? first-cond (free-variable 'else)) (begin-exp first-cond-true))
                                        ((null? (cdr conds)) (if (null? first-cond-true)
                                                               (if-exp first-cond
                                                                       first-cond)
                                                               (if-exp first-cond
                                                                       (begin-exp first-cond-true))))
                                        ((null? first-cond-true) (if-else-exp first-cond
                                                                              first-cond
                                                                              (cond-exp (cdr conds) (cdr cond-trues))))
                                        (else (if-else-exp first-cond
                                                           (begin-exp first-cond-true)
                                                           (cond-exp (cdr conds) (cdr cond-trues))))))))
           (and-exp (bodies)
                    (syntax-expand (if (null? bodies)
                                     (constant-exp (boolean-literal #t))
                                     (let [[first (car bodies)]]
                                       (cond
                                         ((null? (cdr bodies)) (if-else-exp first
                                                                            first
                                                                            (constant-exp (boolean-literal #f))))
                                         (else (if-else-exp first
                                                            (and-exp (cdr bodies))
                                                            (constant-exp (boolean-literal #f)))))))))
           (or-exp (bodies)
                   (syntax-expand (if (null? bodies)
                                    (constant-exp (boolean-literal #f))
                                    (let [[first (car bodies)]]
                                      (cond
                                        ((null? (cdr bodies)) (if-else-exp first
                                                                           first
                                                                           (constant-exp (boolean-literal #f))))
                                        (else (let [[syms (syntax->datum (generate-temporaries '(a)))]]
                                                (let-exp syms
                                                         (list first)
                                                         (list (if-else-exp (free-variable (car syms))
                                                                            (free-variable (car syms))
                                                                            (or-exp (cdr bodies))))))))))))
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


