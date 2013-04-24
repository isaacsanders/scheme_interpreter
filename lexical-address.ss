(define lexical-address
  (lambda (expr)
    (begin
      (define apply-procs-to-parallel-list
             (lambda (plist lat)
               (cond
                 ((null? plist) '())
                 ((list? (car plist))
                  (cons (apply-procs-to-parallel-list (car plist)
                                                      (car lat))
                        (apply-procs-to-parallel-list (cdr plist)
                                                      (cdr lat))))
                 (else (cons ((car plist) (car lat))
                             (apply-procs-to-parallel-list (cdr plist)
                                                           (cdr lat)))))))
           (define id
             (lambda (x) x))

           (define increase-depth
             (lambda (env)
               (cond
                 ((null? env) '())
                 (else (cons (apply-procs-to-parallel-list
                               (list id id add1 id)
                               (car env))
                             (increase-depth (cdr env)))))))

           (define extend-env
             (lambda (env vars)
               (letrec
                 ((E (lambda (env vars position)
                       (cond
                         ((null? vars) env)
                         ((symbol? vars) (cons `(,vars : 0 ,position) env))
                         ((list? vars) (E (cons `(,(car vars) : 0 ,position) env)
                                                (cdr vars)
                                                (add1 position)))
                         (else (cons* `(,(car vars) : 0 ,position)
                                      `(,(cdr vars) : 0 ,(add1 position))
                                      env))))))
                 (E (increase-depth env) vars 0)))))
    (letrec
      ((L (lambda (expr env)
            (cond
              ((symbol? expr) (let [[found (assq expr env)]]
                                (if found
                                  (cdr found)
                                  `(,expr free))))
              ((or (number? expr)
                   (boolean? expr)
                   (vector? expr)
                   (char? expr)
                   (string? expr)) expr)
              ((null? expr) '())
              ((pair? expr) (case (car expr)
                              [(if) (if (eq? 3 (length expr))
                                      `(if ,(L (cadr expr) env)
                                         ,(L (caddr expr) env))
                                      `(if ,(L (cadr expr) env)
                                        ,(L (caddr expr) env)
                                        ,(L (cadddr expr) env)))]
                              [(quote) `(quote ,(cadr expr))]
                              [(begin) (cons 'begin (cdr expr))]
                              [(lambda) (let* [[new-env (extend-env env (cadr expr))]
                                               [helper (lambda (expr)
                                                        (L expr new-env))]]
                                        `(lambda
                                        ,(cadr expr)
                                        ,(map helper (cddr expr))))]
                              [else (map (lambda (expr) (L expr env)) expr)]))))))
      (L expr '()))))
