(load "env.ss")
(load "parser.ss")
(load "syntax-expand.ss")
(load "cont.ss")

(define eval-one-exp
  (lambda (expr)
    (let* ([parse-tree (lexical-address (syntax-expand (parse-top-expression expr)))]
           [initial-environment (lexically-addressed-environment (list))]
           [cont (halt-cont)]
           [result (eval-expression parse-tree cont initial-environment)])
      result)))

(define eval-expression
  (lambda (expr cont env)
    (cases expression expr
           [free-variable (id) (let [[found (assq id *global-env*)]]
                                 (if found
                                   (apply-cont cont (cdr found))
                                   (eopl:error 'eval-expression "Variable ~s not bound" id)))]
           [lexical-addressed-variable (depth position) (apply-cont cont (apply-env env depth position))]
           [constant-exp (val) (cases constant val
                                      [boolean-literal   (val) (apply-cont cont val)]
                                      [character-literal (val) (apply-cont cont val)]
                                      [string-literal    (val) (apply-cont cont val)]
                                      [number-literal    (val) (apply-cont cont val)])]
           [quote-exp (datum) (apply-cont cont datum)]
           [global-define-exp (sym body)
                              (set! *global-env* (cons (cons sym
                                                             (eval-expression body
                                                                              cont
                                                                              env
                                                                              ))
                                                       *global-env*))]
           [lambda-exp (formals bodies)
                       (apply-cont cont (make-closure formals bodies env))]
           [if-exp (condition if-true)
                   (eval-expression condition (if-cont if-true cont env) env)]
           [if-else-exp (condition if-true if-false)
                        (eval-expression condition (if-else-cont if-true if-false cont env) env)]
           [vector-exp (datum)
                       (apply-cont cont (list->vector (eval-expressions datum (halt-cont) env)))]
           [begin-exp (bodies) (cond
                                 ((null? (cdr bodies)) (eval-expression (car bodies) cont env))
                                 (else (eval-expression (car bodies)
                                                        (begin-cont (begin-exp (cdr bodies)) env cont) env)))]
           [while-exp (test-exp bodies)
;                      (let loop [[test (eval-expression test-exp cont env)]]
;                        (if test
;                          (begin (eval-expression (begin-exp bodies) cont env)
;                                 (loop (eval-expression test-exp cont env)))))]
							(eval-expression test-exp (while-cont test-exp bodies env cont) env)]
           [set!-exp (variable value)
                     (cases expression variable
                            (lexical-addressed-variable (depth position)
                                                        (set-car! (list-tail
                                                                    (car
                                                                      (list-tail (cadr env) depth))
                                                                    position)
                                                                  (eval-expression value cont env)))
                            (free-variable (name) ;(set! *global-env*
                                                   ; (cons (cons name
                                                    ;            (eval-expression value cont env))
                                                     ;     *global-env*)))
													(eval-expression value (define-cont name cont) env)) 
                            (else (eopl:error 'eval-expression "Error in set! expression: ~s" expr)))]
           [define-exp (sym body)
		   				 (display "yo grandma?")

                       (cases expression sym
                              (lexical-addressed-variable (depth position)
							  (display "yo mama")
                                                          (set-car! (list-tail
                                                                      (car
                                                                        (list-tail (cadr env) depth))
                                                                      position)
                                                                    (eval-expression body cont env)))
                              (free-variable (name) ;(set! *global-env*
                                                    ; (cons (cons name
                                                   ;               (eval-expression body cont env))
                                                    ;        *global-env*)))
													(display "yo daddy")
													(eval-expression body (define-cont name cont) env))
                              (else (eopl:error 'eval-expression "Error in set! expression: ~s" expr)))]
           [app-exp (exps)
                    (eval-expressions exps (proc-cont cont) env)]
           [else (eopl:error 'eval-expression "Evaluation error with: ~s" expr)])))

(define make-closure
  (lambda (formals bodies env)
    (closure formals bodies env)))

(define apply-primitive-proc
  (lambda (id args cont)
    (case id
      [(+)     (apply-cont cont (apply +     args))]
      [(-)     (apply-cont cont (apply -     args))]
      [(*)     (apply-cont cont (apply *     args))]
      [(/)     (apply-cont cont (apply /     args))]
      [(add1)  (apply-cont cont (apply add1  args))]
      [(sub1)  (apply-cont cont (apply sub1  args))]
      [(zero?) (apply-cont cont (apply zero? args))]
      [(not)   (apply-cont cont (apply not   args))]
      [(=)     (apply-cont cont (apply =     args))]
      [(<)     (apply-cont cont (apply <     args))]
      [(<=)    (apply-cont cont (apply <=    args))]
      [(>)     (apply-cont cont (apply >     args))]
      [(>=)    (apply-cont cont (apply >=    args))]

      [(cons)         (apply-cont cont (apply cons         args))]
      [(car)          (apply-cont cont (apply car          args))]
      [(cdr)          (apply-cont cont (apply cdr          args))]
      [(list)         (apply-cont cont (apply list         args))]
      [(null?)        (apply-cont cont (apply null?        args))]
      [(eq?)          (apply-cont cont (apply eq?          args))]
      [(equal?)       (apply-cont cont (apply equal?       args))]
      [(atom?)        (apply-cont cont (apply atom?        args))]
      [(length)       (apply-cont cont (apply length       args))]
      [(list->vector) (apply-cont cont (apply list->vector args))]
      [(list?)        (apply-cont cont (apply list?        args))]
      [(pair?)        (apply-cont cont (apply pair?        args))]
      [(procedure?)   (apply-cont cont (apply procedure?   args))]
      [(vector->list) (apply-cont cont (apply vector->list args))]
      [(vector)       (apply-cont cont (apply vector       args))]
      [(make-vector)  (apply-cont cont (apply make-vector  args))]
      [(vector-ref)   (apply-cont cont (apply vector-ref   args))]
      [(vector?)      (apply-cont cont (apply vector?      args))]
      [(number?)      (apply-cont cont (apply number?      args))]
      [(symbol?)      (apply-cont cont (apply symbol?      args))]
      [(set-car!)     (apply-cont cont (apply set-car!     args))]
      [(set-cdr!)     (apply-cont cont (apply set-cdr!     args))]
      [(vector-set!)  (apply-cont cont (apply vector-set!  args))]

      [(cadr)  (apply-cont cont (apply cadr  args))]
      [(caar)  (apply-cont cont (apply caar  args))]
      [(cddr)  (apply-cont cont (apply cddr  args))]
      [(cdar)  (apply-cont cont (apply cdar  args))]
      [(cadar) (apply-cont cont (apply cadar args))]
      [(caddr) (apply-cont cont (apply caddr args))]
      [(caaar) (apply-cont cont (apply caaar args))]
      [(caadr) (apply-cont cont (apply caadr args))]
      [(cddar) (apply-cont cont (apply cddar args))]
      [(cdddr) (apply-cont cont (apply cdddr args))]
      [(cdaar) (apply-cont cont (apply cdaar args))]
      [(cdadr) (apply-cont cont (apply cdadr args))]
      [(exit)  (apply-cont cont (apply exit  args))]

      [(map)    (apply-cont cont (apply map (cons (lambda arg (apply-proc (car args) arg cont)) (cdr args))))]
      [(apply)  (apply-proc (car args) (cadr args) cont)]
      [(assq)    (apply-cont cont (apply assq      args))]
      [(assv)    (apply-cont cont (apply assv      args))]
      [(append)  (apply-cont cont (apply append    args))]
      [(member)  (apply-cont cont (apply member    args))]
      [(nil)     (apply-cont cont (apply nil       args))]
      [(max)     (apply-cont cont (apply max       args))]
      [(display) (apply-cont cont (apply display   args))]
      [(load)    (apply-cont cont (load-file (car args)))]

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
    nil
    max
    display
    load
    ))

(define *global-env*
  (map cons primitives (map primitive primitives)))

(define reset-global-env
  (lambda ()
    (set! *global-env* (map cons primitives (map primitive primitives)))))

(define apply-proc
  (lambda (proc args cont)
    (if (procedure? proc)
      (cases procedure proc
             [primitive (id) (apply-cont cont (apply-primitive-proc id args cont))]
             [closure (frmls bodies env)
                      (cases formals frmls
                             [unary (param)
                                    (eval-expression
                                      (begin-exp bodies)
                                      cont
                                      (extend-env (list args) env))]
                             [param-list (params)
                                         (eval-expression
                                           (begin-exp bodies)
                                           cont
                                           (extend-env args env))]
                             [list-with-var-args (params var-args)
                                                 (eval-expression
                                                   (begin-exp bodies)
                                                   cont
                                                   (extend-env (append
                                                                 (list-head args (length params))
                                                                 (list (list-tail args (length params))))
                                                               env))])])
      (proc args))))

(define eval-expressions
  (lambda (exps cont env)
    (if (null? exps)
      (apply-cont cont '())
      (eval-expression (car exps) (eval-expressions-cont (cdr exps) env cont) env))))


