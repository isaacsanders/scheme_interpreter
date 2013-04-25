; <expression>                   ::= <constant>
;                                  | <variable>
;                                  | (quote <datum>)
;                                  | (lambda <formals> <expression> <expression>*)
;                                  | (if <expression> <expression> <expression>)
;                                  | <application>
; <variable>                     ::= <lexically-addressed-variable>
;                                  | <free-variable>
; <lexically-addressed-variable> ::= (<symbol> : <number> <number>)
; <free-variable>                ::= (<symbol> free)
; <constant>                     ::= <boolean> | <number> | <character> | <string>
; <formals>                      ::= <variable>
;                                  | (<variable>*)
;                                  | (<variable> <variable>* . <variable>)
; <application>                  ::= (<expression> <expression>*)

(load "interpreter.ss")
(load "forAndReturnFirst.ss")

(define (rl) (load "main.ss"))

(define (rep)
  (begin
    (display "--> ")
    (write (eval-expression
             (lexical-address
               (syntax-expand
                 (parse-expression
                   (read))))
             (lexically-addressed-environment '())))
    (newline)
    (rep)))
