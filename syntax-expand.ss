(load "parser.ss")

(define expand-lambda-exp
  (lambda (formals bodies)
    (lambda-exp formals (map syntax-expand bodies))))

(define expand-if-else-exp
  (lambda (condition if-true if-false)
    (if-else-exp (syntax-expand condition)
                 (syntax-expand if-true)
                 (syntax-expand if-false))))

(define expand-let-exp
  (lambda (syms vals bodies)
    (app-exp (lambda-exp (param-list syms) bodies) vals)))

(define expand-let*-exp
  (lambda (syms vals bodies)
    (cond
      ((null? syms)
       (begin-exp bodies))
      ((null? (cdr syms))
       (app-exp (lambda-exp
                  (param-list (list (car syms)))
                  bodies)
                (list (car vals))))
      (else (app-exp (lambda-exp
                       (param-list (list (car syms)))
                       (list (let*-exp (cdr syms)
                                       (cdr vals)
                                       bodies)))
                     (list (car vals)))))))

(define expand-letrec-exp
  (lambda (syms vals bodies)
    (let-exp syms (make-list (length syms)
                             (app-exp (free-variable 'nil) (list)))
             (list (begin-exp (append (map set!-exp
                                           (map free-variable syms)
                                           vals)
                                      bodies))))))

(define expand-named-let-exp
  (lambda (name syms vals bodies)
    (let* [[name-val (lambda-exp (param-list syms)
                                 bodies)]]
      (letrec-exp (cons name syms)
                  (cons name-val vals)
                  bodies))))

(define expand-cond-exp
  (lambda (conds cond-trues)
    (let [[first-cond (car conds)]
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
                                              (cond-exp (cdr conds)
                                                        (cdr cond-trues))))
        (else (if-else-exp first-cond
                           (begin-exp first-cond-true)
                           (cond-exp (cdr conds) (cdr cond-trues))))))))

(define expand-and-exp
  (lambda (bodies)
    (if (null? bodies)
      (constant-exp (boolean-literal #t))
      (let [[first (car bodies)]
            [syms (syntax->datum (generate-temporaries '(a)))]]
        (cond
          ((null? (cdr bodies)) 
           (let-exp syms
                    (list first)
                    (list (if-else-exp (free-variable (car syms))
                                       (free-variable (car syms))
                                       (constant-exp (boolean-literal #f))))))
          (else (if-else-exp first
                             (and-exp (cdr bodies))
                             (constant-exp (boolean-literal #f)))))))))

(define expand-or-exp
  (lambda (bodies)
    (if (null? bodies)
      (constant-exp (boolean-literal #f))
      (let [[first (car bodies)]
            [syms (syntax->datum (generate-temporaries '(a)))]]
        (let-exp syms
                 (list first)
                 (list (if-else-exp (free-variable (car syms))
                                    (free-variable (car syms))
                                    (cond
                                      ((null? (cdr bodies)) (constant-exp (boolean-literal #f)))
                                      (else (or-exp (cdr bodies)))))))))))

(define expand-if-exp
  (lambda (condition if-true)
    (if-exp (syntax-expand condition)
            (syntax-expand if-true))))

(define expand-case-exp
  (lambda (test-val vals actions)
    (let [[first-case (car vals)]
          [first-action (car actions)]]
      (if (equal? first-case (quote-exp 'else))
        first-action
        (if-else-exp (app-exp (free-variable 'member)
                              (list test-val first-case))
                     first-action
                     (case-exp test-val (cdr vals) (cdr actions)))))))

(define syntax-expand
  (lambda (expr)
    (cases expression expr
           (lambda-exp (formals bodies)               (expand-lambda-exp formals bodies))
           (if-exp (condition if-true)                (expand-if-exp condition if-true))
           (begin-exp (bodies)                        (begin-exp (map syntax-expand bodies)))
           (if-else-exp (condition if-true if-false)  (expand-if-else-exp condition if-true if-false))
           (app-exp (operator operands)               (app-exp (syntax-expand operator) (map syntax-expand operands)))
           (vector-exp (datum)                        (vector-exp (map syntax-expand datum)))
           (define-exp (sym body)                     (define-exp sym (syntax-expand body)))
           (set!-exp (variable value)                 (set!-exp variable (syntax-expand value)))
           (global-define-exp (sym body)
                              (global-define-exp sym (syntax-expand body)))
           (define-to-expand-exp (names values following-bodies)
                                 (syntax-expand (letrec-exp names values following-bodies)))

           (let-exp (syms vals bodies)            (syntax-expand (expand-let-exp syms vals bodies)))
           (let*-exp (syms vals bodies)           (syntax-expand (expand-let*-exp syms vals bodies)))
           (letrec-exp (syms vals bodies)         (syntax-expand (expand-letrec-exp syms vals bodies)))
           (named-let-exp (name syms vals bodies) (syntax-expand (expand-named-let-exp name syms vals bodies)))
           (cond-exp (conds cond-trues)           (syntax-expand (expand-cond-exp conds cond-trues)))
           (and-exp (bodies)                      (syntax-expand (expand-and-exp bodies)))
           (or-exp (bodies)                       (syntax-expand (expand-or-exp bodies)))
           (case-exp (test-val vals actions)      (syntax-expand (expand-case-exp test-val vals actions)))
           (else expr))))

(define create-define-expression-list
  (lambda (syms vals bodies)
    (if (null? syms) bodies
      (cons (define-exp (car syms) (syntax-expand (car vals)))
            (create-define-expression-list (cdr syms) (cdr vals) bodies)))))
