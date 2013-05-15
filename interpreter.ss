(load "env.ss")
(load "parser.ss")
(load "syntax-expand.ss")
(load "cont.ss")

(define eval-one-exp
  (lambda (expr)
    (let* ([parse-tree (lexical-address (syntax-expand (parse-top-expression expr)))]
           [initial-environment (list)]
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
           [app-exp (exps)
                    (eval-expressions exps (proc-cont cont) env)]
           [global-define-exp (sym body)
                              (set! *global-env* (cons (cons sym
                                                             (eval-expression body
                                                                              cont
                                                                              env
                                                                              ))
                                                       *global-env*))]
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
           [else (eopl:error 'eval-expression "Evaluation error with: ~s" expr)])))

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

      [(map)    (apply map (cons (lambda arg (apply-proc (car args) arg)) (cdr args)))]
      [(apply)  (apply-proc (car args) (cadr args))]
      [(assq)   (apply assq   args)]
      [(assv)   (apply assv   args)]
      [(append) (apply append args)]
      [(member) (apply member args)]
      [(nil)    (apply nil args)]
      [(max)    (apply max args)]
      [(display) (apply display args)]
      [(load)   (load-file (car args))]

      [(printf) (apply printf args)]
      [(pretty-print) (apply pretty-print args)]
      [(break) (apply break args)]

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
    printf
    pretty-print
    break
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
             [primitive (id) (apply-cont cont (apply-primitive-proc id args))]
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


