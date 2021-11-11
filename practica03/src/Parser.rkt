#lang nanopass
#|
Compiladores 2022-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Naomi Itzel Reyes Granados
Laboratorio: Nora Hilda Hernández Luna

Parser
Integrantes de equipo/Autores:
Nombre | No.cuenta| correo
Juan García Lugo, 316161013, juanlugo@ciencias.unam.mx
Nestor Semer Vazquez Cordero, 316041625, nestor2502@ciencias.unam.mx
Angel Christian Pimentel Noriega, 316157995, cristianp@ciencias.unam.mx
|#

(require "Lexer.rkt" ;; Here goes your lexer file. In my case I called "Compiler.rkt".
         (prefix-in : parser-tools/lex-sre)
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
         parser-tools/lex)

;; An abstraction of the grammar minHS.
(define-struct num-exp (n) #:transparent) ; For the numbers.
(define-struct bool-exp (b) #:transparent) ; For the booleans.
(define-struct var-exp (i) #:transparent) ; For the variables.

(define-struct par-exp (exp) #:transparent)     ; For the parenthesis.
(define-struct brack-exp (exp) #:transparent)   ; For the brackets.
(define-struct key-exp (exp) #:transparent)     ; For the keys.
(define-struct app-t-exp (e1 e2) #:transparent) ; For multiple arguments ][

(define-struct prim-exp (op e1 e2) #:transparent) ; For the arithmetic/boolean operations.

(define-struct begin-exp (exp) #:transparent) ; For code block

(define-struct if-then-else-exp (g e1 e2) #:transparent) ; For the if-then-else two branch conditionals.
(define-struct if-then-exp (g e1) #:transparent) ; For the if-then one branch conditionals.

(define-struct fun-exp (sign body) #:transparent) ; For anonymous functions.
(define-struct fun-f-exp (name sign body) #:transparent) ; For named functions.
(define-struct app-exp (e1 e2) #:transparent) ;; For function application: e1 app e2

(define-struct let-exp (l body) #:transparent) ; For let expresion
(define-struct assign-exp (var value) #:transparent) ; For assignment in let expression

(define-struct typeof-exp (v e) #:transparent) ; For the type of operator ":".
(define-struct int-exp () #:transparent) ; For the Int type.
(define-struct boole-exp () #:transparent) ; For the Bool type.

;; Side effects for errors on parser
(define parse-error
  (lambda (tok-ok? tok-name tok-value)
    (print (string-append "Token inesperado: " (symbol->string tok-name)))))

(define minHS-parser
  (parser
   (start exp) ; start clause. The exp is the initial symbol where the parser begins the analysis. 
   (end EOF) ; end clause. The parser ends when it reads the given symbol. In our case, EOF.
   (error parse-error) ; error clause. Here can be some errors presented in the anlysis.
   (tokens a b) ; tokens clause. Here goes our tokens. In our case, we defined the tokens in the lexer script.
   (precs (left APPT) ; precs clause. Here we can give some precedence of our language operators.
          (left ASSIGN) 
          (left TYPEOF)
          (left OR)
          (left AND)
          (left - +)
          (left * /))
   (grammar ; grammar clause. Here goes the grammar of minHS.
    (exp [(NUM) (num-exp $1)]
         [(BOOL) (bool-exp $1)]
         [(VAR) (var-exp $1)]
         [(exp + exp) (make-prim-exp + $1 $3)]
         [(exp - exp) (make-prim-exp - $1 $3)]
         [(exp / exp) (make-prim-exp / $1 $3)]
         [(exp * exp) (make-prim-exp * $1 $3)]
         [(exp AND exp) (make-prim-exp 'and $1 $3)]
         [(exp OR exp) (make-prim-exp 'or $1 $3)]
         [(LP exp RP) (make-par-exp $2)]
         [(LB exp RB) (make-brack-exp $2)]
         [(LP exp RP) (make-key-exp $2)]
         [(BEGIN LK exp RK) (make-begin-exp $3)]
         [(IF LP exp RP THEN LK exp RK ELSE LK exp RK) (make-if-then-else-exp $3 $7 $11)]
         [(IF LP exp RP THEN LK exp RK) (if-then-exp $3 $7)]
         [(FUN LP exp RP ARROW exp) (make-fun-exp $3 $6)]
         [(FUNF LP exp exp RP ARROW exp) (make-fun-f-exp $3 $4 $7)]
         [(exp APP exp) (make-app-exp $1 $3)]
         [(LET LP exp RP IN exp END) (make-let-exp $3 $6)]
         [(INT) (int-exp)]
         [(BOOLE) (boole-exp)]
         [(exp TYPEOF exp) (make-typeof-exp $1 $3)]
         [(LP exp RP TYPEOF exp) (make-typeof-exp $2 $5)]
         [(exp ASSIGN exp) (make-assign-exp $1 $3)]
         [(exp APPT exp) (make-app-t-exp $1 $3)]
    ))))



; A function that stores our lexer into a lambda function without arguments.
(define (lex-this lexer input) (lambda () (lexer input)))

; Exercise 3

;; Return the string representation of prim
;; select-prim: procedure | symbol -> string
(define (select-prim prim)
  (cond
    [(equal? prim +) "+"]
    [(equal? prim -) "-"]
    [(equal? prim *) "*"]
    [(equal? prim /) "/"]
    [(equal? prim 'and) "and"]
    [(equal? prim 'or) "or"]))

; Function that returns the string representation of a ASA
(define (expr->string e)
  (match e
    ; Basic
    [(var-exp e) (symbol->string e)]
    [(num-exp e) (number->string e)]
    [(bool-exp e) (format "~a" e)]

    ; Casos especiales
    [(typeof-exp (brack-exp e1) e2) (string-append "(" (expr->string e1) ") " (expr->string e2))]
    [(par-exp (prim-exp p e1 e2)) (expr->string (prim-exp p e1 e2))]
    [(brack-exp (app-t-exp e1 e2)) (expr->string (app-t-exp e1 e2))]
    [(app-t-exp (app-t-exp es1 es2) e2)
     (string-append (expr->string es1) " " (expr->string es2) " " (expr->string e2))]
    [(assign-exp (typeof-exp v e) e2)
     (string-append "[" (expr->string v) " " (expr->string e) " = " (expr->string e2) "]")]
    
    ; Containers
    [(par-exp e) (string-append "(" (expr->string e) ")")]
    [(brack-exp e) (string-append "[" (expr->string e) "]")]
    [(app-t-exp e1 e2) (string-append (expr->string e1) " " (expr->string e2))]

    ; Operations
    [(prim-exp pr e1 e2) (string-append "(" (select-prim pr) " " (expr->string e1) " " (expr->string e2) ")")]

    ; Types
    [(int-exp) "Int"]
    [(boole-exp) "Bool"]
    [(typeof-exp v e) (string-append "[" (expr->string v) " " (expr->string e) "]")]

    ; Let
    [(let-exp x body) (string-append "(let (" (expr->string x) ") " (expr->string body) ")")]
    [(assign-exp var value) (string-append (expr->string var) " = " (expr->string value))]
    
    ; Func
    [(fun-exp sign body) (string-append "(fun " (expr->string sign) " " (expr->string body) ")")]
    [(fun-f-exp f sign body)
     (string-append "(funF " (expr->string f) " " (expr->string sign) " " (expr->string body) ")")]
    ; Begin
    [(begin-exp e) (string-append "(begin " (expr->string e) ")")]

    ; Ifs
    [(if-then-exp g e)
     (string-append "(if " (expr->string g) " " (expr->string e) ")")]
    [(if-then-else-exp g e1 e2)
     (string-append "(if " (expr->string g) " " (expr->string e1) " " (expr->string e2) ")")]

    ; Function application
    [(app-exp e1 e2) (string-append "("(expr->string e1) " " (expr->string e2) ")")]))