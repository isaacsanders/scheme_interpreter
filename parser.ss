(load "chez-init.ss")
(load "lexical-address.ss")
(load "functional-utils.ss")

(define-datatype formals formals?
                 (unary
                   (param symbol?))
                 (param-list
                   (params (list-of symbol?)))
                 (list-with-var-args
                   (params (list-of symbol?))
                   (var-args symbol?)))

(define-datatype constant constant?
                 (boolean-literal
                   (value boolean?))
                 (number-literal
                   (value number?))
                 (character-literal
                   (value char?))
                 (string-literal
                   (value string?)))

(define quoteable?
  (lambda (val) #t))

(define variable?
  (lambda (expr)
    (cases expression expr
           (lexical-addressed-variable (depth position) #t)
           (free-variable (name) #t)
           (else #f))))

(define-datatype expression expression?
                 (lexical-addressed-variable
                   (depth number?)
                   (position number?))
                 (free-variable
                   (name symbol?))
                 (constant-exp
                   (value constant?))
                 (quote-exp
                   (datum quoteable?))
                 (lambda-exp
                   (formals formals?)
                   (bodies (list-of expression?)))
                 (begin-exp
                   (bodies (list-of expression?)))
;                 (app-exp
;                   (operator expression?)
;                   (operands (list-of expression?)))
				 (app-exp
					(exps (list-of expression?)))
                 (let-exp
                   (syms (list-of symbol?))
                   (vals (list-of expression?))
                   (bodies (list-of expression?)))
                 (let*-exp
                   (syms (list-of symbol?))
                   (vals (list-of expression?))
                   (bodies (list-of expression?)))
                 (letrec-exp
                   (syms (list-of symbol?))
                   (vals (list-of expression?))
                   (bodies (list-of expression?)))
                 (named-let-exp
                   (name symbol?)
                   (syms (list-of symbol?))
                   (vals (list-of expression?))
                   (bodies (list-of expression?)))
                 (cond-exp
                   (conds (list-of expression?))
                   (cond-trues (list-of (list-of expression?))))
                 (and-exp
                   (bodies (list-of expression?)))
                 (or-exp
                   (bodies (list-of expression?)))
                 (if-exp
                   (condition expression?)
                   (if-true expression?))
                 (if-else-exp
                   (condition expression?)
                   (if-true expression?)
                   (if-false expression?))
                 (while-exp
                   (test-exp expression?)
                   (bodies (list-of expression?)))
                 (set!-exp
                   (variable variable?)
                   (value expression?))
                 (case-exp
                   (test-val expression?)
                   (vals (list-of expression?))
                   (actions (list-of expression?)))
                 (vector-exp
                   (datum (list-of expression?)))
                 (define-exp
                   (name symbol?)
                   (value expression?))
                 (define-to-expand-exp
                   (names (list-of symbol?))
                   (values (list-of expression?))
                   (following-bodies (list-of expression?)))
                 (global-define-exp
                   (name symbol?)
                   (value expression?)))

(define report-parse-error
  (lambda (msg datum)
    (eopl:error 'parse-expression msg datum)))

(define list-of
  (lambda (pred)
    (letrec
      ((L (lambda (lat)
            (and (list? lat)
            (cond
              ((null? lat) #t)
              (else (and (pred (car lat)) (L (cdr lat)))))))))
      L)))

(define parse-lambda
  (lambda (datum)
    (define split-param-list-and-var-args
      (lambda (param-list)
        (letrec
          ((S (lambda (old-list split-list)
                (cond
                  ((not (pair? old-list)) (cons split-list old-list))
                  (else (S (cdr old-list) (cons (car old-list) split-list)))))))
          (S param-list '()))))
    (cond
      ((null? (cddr datum)) (report-parse-error "Invalid lambda syntax ~s" datum))

      ((symbol? (cadr datum))
       (lambda-exp (unary (cadr datum))
                   (let ([bodies (map parse-expression (cddr datum))])
                     (if (define-look-ahead bodies)
                       (list (compose-define-to-expand-exp bodies))
                       bodies))))

      ((and (not (list? (cadr datum))) (pair? (cadr datum)))
       (let* ((params (split-param-list-and-var-args (cadr datum)))
              (param-list (car params))
              (var-args (cdr params)))
         (lambda-exp (list-with-var-args param-list var-args)
                     (let ([bodies (map parse-expression (cddr datum))])
                       (if (define-look-ahead bodies)
                         (list (compose-define-to-expand-exp bodies))
                         bodies)))))

      (((list-of symbol?) (cadr datum))
       (lambda-exp (param-list (cadr datum))
                   (let ([bodies (map parse-expression (cddr datum))])
                     (if (define-look-ahead bodies)
                       (list (compose-define-to-expand-exp bodies))
                       bodies))))
      (else (report-parse-error "Invalid parameter syntax ~s" datum)))))

(define parse-if
  (lambda (datum)
    (cond
      ((null? (cdr datum))
       (report-parse-error "No condition for if-exp ~s" datum))
      ((null? (cddr datum))
       (report-parse-error "No true-case for if-exp ~s" datum))
      ((null? (cdddr datum))
       (if-exp (parse-expression (cadr datum))
               (parse-expression (caddr datum))))
      ((null? (cddddr datum))
       (if-else-exp (parse-expression (cadr datum))
                    (parse-expression (caddr datum))
                    (parse-expression (cadddr datum))))
      (else (report-parse-error "Invalid if-exp ~s" datum)))))

(define parse-set!
  (lambda (datum)
    (cond
      ((null? (cddr datum))
       (report-parse-error "No value to set ~s to" (cadr datum)))
      ((not (null? (cdddr datum)))
       (report-parse-error "Too many bodies in ~s" datum))
      (else (set!-exp (free-variable (cadr datum)) (parse-expression (caddr datum)))))))

(define parse-letrec
  (lambda (datum)
    (cond
      ((or (null? (cddr datum))
           (not (list? (cadr datum))))
       (eopl:error 'parse-expression
                   "No body for letrec-exp ~s" datum))
      (else (letrec-exp (map car (cadr datum))
                        (map (compose
                               parse-expression
                               cadr)
                             (cadr datum))
                        (map parse-expression (cddr datum)))))))

(define parse-top-expression
  (lambda (datum)
    (if (list? datum)
      (cond [(eq? (car datum) 'define) (global-define-exp (cadr datum) (parse-expression (caddr datum)))]
            [(eq? (car datum) 'begin) (begin-exp (map parse-top-expression (cdr datum)))]
            [else (parse-expression datum)])
      (parse-expression datum))))

(define parse-let
  (lambda (datum)
    (cond
      ((symbol? (cadr datum))
       (named-let-exp (cadr datum)
                      (map car (caddr datum))
                      (map (compose parse-expression cadr) (caddr datum))
                      (map parse-expression (cdddr datum))))
      (else (let-exp (map car (cadr datum))
                     (map (compose parse-expression cadr) (cadr datum))
                     (let ([bodies (map parse-expression (cddr datum))])
                       (if (define-look-ahead bodies)
                         (list (compose-define-to-expand-exp bodies))
                         bodies)))))))
(define parse-cond
  (lambda (datum)
    (cond-exp (map (compose parse-expression car) (cdr datum))
              (map (compose cdr (partial map parse-expression)) (cdr datum)))))

(define parse-let*
  (lambda (datum)
    (let*-exp (map car (cadr datum))
              (map (compose parse-expression cadr) (cadr datum))
              (map parse-expression (cddr datum)))))

(define parse-expression
  (lambda (datum)
    (cond [(symbol? datum) (free-variable datum)]
          [(char? datum) (constant-exp (character-literal datum))]
          [(boolean? datum) (constant-exp (boolean-literal datum))]
          [(number? datum) (constant-exp (number-literal datum))]
          [(string? datum) (constant-exp (string-literal datum))]
          [(vector? datum) (vector-exp (map parse-expression (vector->list datum)))]
          [(list? datum)
           (cond
             [(null? datum) (app-exp (free-variable 'list))]
             [(eq? (car datum) 'define) (define-exp (cadr datum) (parse-expression (caddr datum)))]
             [(eq? (car datum) 'case) (case-exp (parse-expression (cadr datum))
                                                (map (compose quote-exp car) (cddr datum))
                                                (map (compose parse-expression cadr) (cddr datum)))]
             [(eq? (car datum) 'while) (while-exp (parse-expression (cadr datum)) (map parse-expression (cddr datum)))]
             [(eq? (car datum) 'cond) (parse-cond datum)]
             [(eq? (car datum) 'and) (and-exp (map parse-expression (cdr datum)))]
             [(eq? (car datum) 'or) (or-exp (map parse-expression (cdr datum)))]
             [(eq? (car datum) 'let) (parse-let datum)]
             [(eq? (car datum) 'let*) (parse-let* datum)]
             [(eq? (car datum) 'letrec) (letrec-exp (map car (cadr datum))
                                                    (map (compose parse-expression cadr) (cadr datum))
                                                    (map parse-expression (cddr datum)))]
             [(eq? (car datum) ':) (lexical-addressed-variable (cadr datum) (caddr datum))]
             [(eq? (car datum) 'quote) (quote-exp (cadr datum))]
             [(eq? (car datum) 'lambda) (parse-lambda datum)]
             [(eq? (car datum) 'if)     (parse-if datum)]
             [(eq? (car datum) 'letrec) (parse-letrec datum)]
             [(eq? (car datum) 'set!)   (parse-set! datum)]
             [(eq? (car datum) 'begin) (begin-exp (map parse-expression (cdr datum)))]
             [(and (pair? (car datum))
                   (eq? (caar datum) 'quote)) (quote-exp (car datum))]
;             [else (app-exp (parse-expression (car datum))
;                            (map parse-expression (cdr datum)))])]
			 [else (app-exp (map parse-expression datum))])]
          [else (report-parse-error "Invalid concrete syntac ~s" datum)])))

(define define-look-ahead
  (lambda (bodies)
    (if (null? bodies) #f
      (cases expression (car bodies)
             [define-exp (name value) #t]
             [else (define-look-ahead (cdr bodies))]))))

(define compose-define-to-expand-exp
  (lambda (bodies)
    (let ([vals (compose-define-to-expand-exp-helper bodies '(() () ()))])
      (define-to-expand-exp (car vals) (cadr vals) (caddr vals)))))


(define compose-define-to-expand-exp-helper
  (lambda (bodies symsValsBodies)
    (if (null? bodies) symsValsBodies
      (cases expression (car bodies)
             [define-exp (name value) (compose-define-to-expand-exp-helper (cdr bodies)
                                                                           (list (cons name (car symsValsBodies)) (cons value (cadr symsValsBodies)) (caddr symsValsBodies)))]
             [else (compose-define-to-expand-exp-helper (cdr bodies)
                                                        (list (car symsValsBodies) (cadr symsValsBodies) (cons (car bodies) (caddr symsValsBodies))))]))))
