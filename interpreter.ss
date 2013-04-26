(load "env.ss")
(load "parser.ss")
(load "syntax-expand.ss")

(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (lexical-address (syntax-expand (parse-expression exp)))]
           [initial-environment (lexically-addressed-environment (list))]
           [result (eval-expression parse-tree initial-environment)])
      result)))

(define eval-expression
  (lambda (expr env)
    (cases expression expr
           [free-variable (id) (if (member id primitives)
                                 (primitive id)
                                 (eopl:error 'eval-expression "Variable ~s not bound" id))]
           [lexical-addressed-variable (depth position) (apply-env env depth position)]
           [constant-exp (val) (cases constant val
                                      [boolean-literal (val) val]
                                      [character-literal (val) val]
                                      [string-literal (val) val]
                                      [number-literal (val) val])]
           [quote-exp (datum) datum]
           [lambda-exp (formals bodies)
                       (make-closure formals bodies env)]
           [if-exp (condition if-true)
                   (if (eval-expression condition env)
                     (eval-expression if-true env))]
           [if-else-exp (condition if-true if-false)
                        (if (eval-expression condition env)
                          (eval-expression if-true env)
                          (eval-expression if-false env))]
           [vector-exp (datum)
                       (list->vector (map (eval-expression-env env) datum))]
           [while-exp (test-exp bodies)
                      (let loop [[test (eval-expression test-exp env)]]
                        (if test
                          (begin (eval-expression (begin-exp bodies) env)
                                 (loop (eval-expression test-exp env)))))]
           [begin-exp (bodies) (cond
                                 ((null? (cdr bodies)) (eval-expression (car bodies) env))
                                 (else (begin (eval-expression (car bodies) env)
                                              (eval-expression (begin-exp (cdr bodies)) env))))]
           [app-exp (operator operands)
                    (let ([procedure (eval-expression operator env)]
                          [args (map (eval-expression-env env) operands)])
                      (apply-proc procedure args))]
           [else (eopl:error 'eval-expression "Evaluation error with: ~s" expr)])))

(define eval-expression-env
  (lambda (env)
    (lambda (expr)
      (eval-expression expr env))))

(define make-closure
  (lambda (formals bodies env)
    (closure formals bodies env)))

(define apply-primitive-proc
  (lambda (id args)
    (case id
      [(+)     (apply +     args)]
      [(-)     (apply -     args)]
      [(*)     (apply *     args)]
      [(/)     (apply /     args)]
      [(add1)  (apply add1  args)]
      [(sub1)  (apply sub1  args)]
      [(zero?) (apply zero? args)]
      [(not)   (apply not   args)]
      [(=)     (apply =     args)]
      [(<)     (apply <     args)]
      [(<=)    (apply <=    args)]
      [(>)     (apply >     args)]
      [(>=)    (apply >=    args)]

      [(cons)         (apply cons         args)]
      [(car)          (apply car          args)]
      [(cdr)          (apply cdr          args)]
      [(list)         (apply list         args)]
      [(null?)        (apply null?        args)]
      [(eq?)          (apply eq?          args)]
      [(equal?)       (apply equal?       args)]
      [(atom?)        (apply atom?        args)]
      [(length)       (apply length       args)]
      [(list->vector) (apply list->vector args)]
      [(list?)        (apply list?        args)]
      [(pair?)        (apply pair?        args)]
      [(procedure?)   (apply procedure?   args)]
      [(vector->list) (apply vector->list args)]
      [(vector)       (apply vector       args)]
      [(make-vector)  (apply make-vector  args)]
      [(vector-ref)   (apply vector-ref   args)]
      [(vector?)      (apply vector?      args)]
      [(number?)      (apply number?      args)]
      [(symbol?)      (apply symbol?      args)]
      [(set-car!)     (apply set-car!     args)]
      [(set-cdr!)     (apply set-cdr!     args)]
      [(vector-set!)  (apply vector-set!  args)]

      [(cadr)  (apply cadr  args)]
      [(caar)  (apply caar  args)]
      [(cddr)  (apply cddr  args)]
      [(cdar)  (apply cdar  args)]
      [(cadar) (apply cadar args)]
      [(caddr) (apply caddr args)]
      [(caaar) (apply caaar args)]
      [(caadr) (apply caadr args)]
      [(cddar) (apply cddar args)]
      [(cdddr) (apply cdddr args)]
      [(cdaar) (apply cdaar args)]
      [(cdadr) (apply cdadr args)]
      [(exit)  (apply exit  args)]

      [(map)    (map (lambda arg (apply-proc (car args) arg)) (cadr args))]
      [(apply)  (apply-proc (car args) (cadr args))]
      [(assq)   (apply assq   args)]
      [(assv)   (apply assv   args)]
      [(append) (apply append args)]
      [(member) (apply member args)]

      [else (eopl:error 'apply-primitive-proc "invalid primitive ~s" id)])))

(define primitives
  '(
    +
    -
    *
    /
    add1
    sub1
    zero?
    not
    =
    <
    <=
    >
    >=
    cons
    car
    cdr
    list
    null?
    eq?
    equal?
    atom?
    length
    list->vector
    list?
    pair?
    procedure?
    vector->list
    vector
    make-vector
    vector-ref
    vector?
    number?
    symbol?
    set-car!
    set-cdr!
    vector-set!
    cadr
    caar
    cddr
    cdar
    cadar
    caddr
    caaar
    caadr
    cddar
    cdddr
    cdaar
    cdadr
    exit
    map
    apply
    assq
    assv
    append
    member
    ))

(define apply-proc
  (lambda (proc args)
    (if (procedure? proc)
      (cases procedure proc
             [primitive (id) (apply-primitive-proc id args)]
             [closure (frmls bodies env)
                      (cases formals frmls
                             [unary (param)
                                    (eval-expression
                                      (begin-exp bodies)
                                      (extend-env (list args) env))]
                             [param-list (params)
                                         (eval-expression
                                           (begin-exp bodies)
                                           (extend-env args env))]
                             [list-with-var-args (params var-args)
                                                 (eval-expression
                                                   (begin-exp bodies)
                                                   (extend-env (append
                                                                 (list-head args (length params))
                                                                 (list (list-tail args (length params))))
                                                               env))])])
      (proc args))))
