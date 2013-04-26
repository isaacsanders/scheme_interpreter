(load "chez-init.ss")
(load "lexical-address.ss")

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
                 (app-exp
                   (operator expression?)
                   (operands (list-of expression?)))
                 (let-exp
                   (syms (list-of symbol?))
                   (vals (list-of expression?))
                   (bodies (list-of expression?)))
                 (let*-exp
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
                 (case-exp
                   (test-val expression?)
                   (vals (list-of expression?))
                   (actions (list-of expression?)))
                 (vector-exp
                   (datum (list-of expression?))))

(define report-parse-error
  (lambda (msg datum)
    (eopl:error 'parse-expression msg datum)))

(define list-of
  (lambda (pred)
    (letrec
      ((L (lambda (lat)
            ; (and (list? lat)
            (cond
              ((null? lat) #t)
              (else (and (pred (car lat)) (L (cdr lat))))))))
      L)))

(define binding?
  (lambda (datum)
    (and (list? datum)
         (eq? (length datum) 2)
         (symbol? (car datum)))))

(define parse-lambda
  (lambda (datum)
    (define split-param-list-and-var-args
      (lambda (param-list)
        (letrec
          ((S (lambda (old-list split-list)
                (cond
                  ((not (pair? (cdr param-list))) (cons (append split-list
                                                                (list (car param-list)))
                                                        (cdr param-list)))
                  (else (S (cdr old-list) (cons (car old-list) split-list)))))))
          (S param-list '()))))
    (cond
      ((null? (cddr datum)) (report-parse-error "Invalid lambda syntax ~s" datum))

      ((symbol? (cadr datum))
       (lambda-exp (unary (cadr datum))
                   (map parse-expression (cddr datum))))

      ((and (not (list? (cadr datum)))
            (pair? (cadr datum))) (let* ((params (split-param-list-and-var-args (cadr datum)))
                                         (param-list (car params))
                                         (var-args (cdr params)))
            (lambda-exp (list-with-var-args param-list var-args)
                        (map parse-expression (cddr datum)))))

      (((list-of symbol?) (cadr datum))
       (lambda-exp (param-list (cadr datum))
                   (map parse-expression (cddr datum))))
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

; (define parse-binding
;   (lambda (binding)
;     (cond
;       ((binding? binding) (list (car binding) (parse-expression (cadr binding))))
;       (else (eopl:error 'parse-expression
;                         "Invalid binding ~s" binding)))))
; 
; (define unparse-binding
;   (lambda (binding)
;     (list (car binding) (unparse-expression (cadr binding)))))
; 
; (define parse-set
;   (lambda (datum)
;     (cond
;       ((null? (cddr datum))
;        (eopl:error 'parse-expression
;                    "No value to set ~s to" (cadr datum)))
;       ((not (null? (cdddr datum)))
;        (eopl:error 'parse-expression
;                    "Too many bodies in ~s" datum))
;       (else (set!-exp (cadr datum) (parse-expression (caddr datum)))))))
; 
; (define parse-bindings
;   (lambda (bindings)
;     (map parse-binding bindings)))
; 
; (define parse-letrec
;   (lambda (datum)
;     (cond
;       ((or (null? (cddr datum))
;            (not (list? (cadr datum))))
;        (eopl:error 'parse-expression
;                    "No body for letrec-exp ~s" datum))
;       (else (letrec-exp (parse-bindings (cadr datum))
;                         (map parse-expression (cddr datum)))))))

(define compose-parse-expression
  (lambda (proc)
    (compose parse-expression proc)))

(define parse-expression
  (lambda (datum)
    (cond [(symbol? datum) (free-variable datum)]
          [(boolean? datum) (constant-exp (boolean-literal datum))]
          [(number? datum) (constant-exp (number-literal datum))]
          [(string? datum) (constant-exp (string-literal datum))]
          [(vector? datum) (vector-exp (map parse-expression (vector->list datum)))]
          [(list? datum)
           (cond
             [(null? datum) (app-exp (free-variable 'list) '())]
             [(eq? (car datum) 'case) (case-exp ((compose-parse-expression cadr) datum)
                                                (map (compose quote-exp car) (cddr datum))
                                                (map (compose-parse-expression cadr) (cddr datum)))]
             [(eq? (car datum) 'while) (while-exp (parse-expression (cadr datum)) (map parse-expression (cddr datum)))]
             [(eq? (car datum) 'cond) (cond-exp (map (compose-parse-expression car) (cdr datum))
                                                (map (compose cdr (partial map parse-expression)) (cdr datum)))]
             [(eq? (car datum) 'and) (and-exp (map parse-expression (cdr datum)))]
             [(eq? (car datum) 'or) (or-exp (map parse-expression (cdr datum)))]
             [(eq? (car datum) 'let) (let-exp (map car (cadr datum))
                                              (map (compose-parse-expression cadr) (cadr datum))
                                              (map parse-expression (cddr datum)))]
             [(eq? (car datum) 'let*) (let*-exp (map car (cadr datum))
                                                (map (compose-parse-expression cadr) (cadr datum))
                                                (map parse-expression (cddr datum)))]
             [(eq? (car datum) ':) (lexical-addressed-variable (cadr datum) (caddr datum))]
             [(eq? (car datum) 'quote) (quote-exp (cadr datum))]
             [(eq? (car datum) 'lambda) (parse-lambda datum)]
             [(eq? (car datum) 'if)     (parse-if datum)]
             ; [(eq? (car datum) 'letrec) (parse-letrec datum)]
             ; [(eq? (car datum) 'set!)   (parse-set datum)]
             [(eq? (car datum) 'begin) (begin-exp (map parse-expression (cdr datum)))]
             [(and (pair? (car datum))
                   (eq? (caar datum) 'quote)) (quote-exp (car datum))]
             [(eq? (cadr datum) 'free) (free-variable (car datum))]
             [else (app-exp (parse-expression (car datum))
                            (map parse-expression (cdr datum)))])]
          [else (eopl:error 'parse-expression
                            "Invalid concrete syntac ~s" datum)])))
