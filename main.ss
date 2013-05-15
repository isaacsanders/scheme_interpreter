; <form>                         ::= <definition>
;                                  | <expression>
; <definition>                   ::= <variable definition>
;                                  | (begin <definition>*)
; <variable definition>          ::= (define <variable> <expression>)
; <expression>                   ::= <constant>
;                                  | <variable>
;                                  | (quote <datum>)
;                                  | (lambda <formals> <expression> <expression>*)
;                                  | (if <expression> <expression> <expression>)
;                                  | (set! <variable> <expression>)
;                                  | (let ((<variable> <expression>)*) <expression>*)
;                                  | (let <variable> ((<variable> <expression>)*) <expression>*)
;                                  | (let* ((<variable> <expression>)*) <expression>*)
;                                  | (letrec ((<variable> <expression>)*) <expression>*)
;                                  | (cond ((<expression> <expression>*)*))
;                                  | (and <expression>*)
;                                  | (or <expression>*)
;                                  | (case <expression> ((<variable> <expression>*)*))
;                                  | (while <expression> <expression>*)
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
(load "chez-init.ss")
(load "parser.ss")
(load "syntax-expand.ss")
(load "cont.ss")
(load "interpreter.ss")

(define (rl) (load "main.ss"))

(define rep
  (lambda ()
    (display "--> ")
    (eval-expression ((compose lexical-address
                               syntax-expand
                               parse-top-expression)
                      (read))
                     (rep-cont)
                     (lexically-addressed-environment (list)))))
