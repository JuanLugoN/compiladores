#lang nanopass
(require "Language.rkt")
(provide (all-defined-out))
#|
Compiladores 2022-1

Integrantes de equipo/Autores:
Nombre | No.cuenta| correo
Juan García Lugo, 316161013, juanlugo@ciencias.unam.mx
Nestor Semer Vazquez Cordero, 316041625, nestor2502@ciencias.unam.mx
Angel Christian Pimentel Noriega, 316157995, cristianp@ciencias.unam.mx
|#

; Pass that removes one-armed if statements
(define-pass remove-one-armed-if : LF(ir) -> LNI()
  (Expr : Expr (ir) -> Expr()
        [(if ,[e0] ,[e1])
         `(if ,e0 ,e1 (void))]))

;; Remove string of LF lenguage
(define-pass remove-string : LNI(ir) -> LNS ()
  (Expr : Expr (ir) -> Expr()
        [,s `(list, (string->list s))]))

;; Pass definition for currying let and letrec expressions.
(define-pass curry-let : LNS (ir) -> L7 ()
  (Expr : Expr (ir) -> Expr ()
        [(let ((,x* ,t* ,[e*]) ...) ,[body])
         (let bindings ([binding-x* x*]
                        [binding-t* t*]
                        [binding-e* e*])
           (if (= (length binding-x*) 1)
               `(let ([,(car binding-x*) ,(car binding-t*) ,(car binding-e*)]) ,body)
               `(let ([,(car binding-x*) ,(car binding-t*) ,(car binding-e*)])
                  ,(bindings(cdr binding-x*)(cdr binding-t*)(cdr binding-e*)))))]
        
        [(letrec ((,x* ,t* ,[e*]) ...) ,[body*] ... ,[body])
         (let bindings ([binding-x* x*]
                        [binding-t* t*]
                        [binding-e* e*])
           (if (= (length binding-x*) 1)
               `(letrec ([,(car binding-x*) ,(car binding-t*) ,(car binding-e*)]) ,body)
               `(letrec ([,(car binding-x*) ,(car binding-t*) ,(car binding-e*)])
                  ,(bindings(cdr binding-x*)(cdr binding-t*)(cdr binding-e*)))))]))

;; Pass that identifies let expressions used to define functions
;; and replaces them for letrec.
(define-pass identify-assigments : L7 (ir) -> L7 ()
  (Expr : Expr (ir) -> Expr()
        [(let ([,x ,t ,[e]]) ,[body])
         (if (equal? t 'Lambda)
             `(letrec ([,x ,t ,e]) ,body)
             ir)]))

;;Given a lambda it returns a letfun expression
(define-pass un-anonymous : L7 (ir) -> L8 ()
  (Expr : Expr (ir [index 0]) -> Expr ()
        [(lambda ([,[Expr : x* (set! index (add1 index)) -> x*] ,t*] ...) ,[body])
         (let ([i (string->symbol (format "foo~a" index))])
           `(letfun [,i 'Lambda (lambda ([,x* ,t*] ...) ,body)] ,i))]))

;; Pass that verifies the arity of a primitive operations
;; Checks if arity is at least 2 for binary operations.
;; Checks if airty is 1 for not and list operations.
(define-pass verify-arity : L8 (ir) -> L8 ()
  (Expr : Expr (ir) -> Expr ()
        [(primapp ,pr ,[e*] ...)
         (match pr
           ['+ (if (not (= 2 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['- (if (not (= 2 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['* (if (not (= 2 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['/ (if (not (= 2 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['and (if (< (length e*) 2 ) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['or (if (< (length e*) 2 ) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['car (if (not (= 1 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['cdr (if (not (= 1 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['length (if (not (= 1 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['++ (if (not (= 1 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['-- (if (not (= 1 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['< (if (not (= 2 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['> (if (not (= 2 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['iszero? (if (not (= 1 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)]
           ['equal? (if (not (= 2 (length e*))) (error 'verify-arity "Discrepancia de aridad") ir)])]))

;; Function that returns the list of free variables from an expression in L8
(define (free-vars e)
  (nanopass-case (L8 Expr) e
                 [,x (list x)]
                 [(begin ,e* ... ,e) (let f ([e* e*] [e e])
                                       (if (equal? (length e*) 0)
                                           (free-vars e)
                                           (let ([fe (car e*)])
                                             (nanopass-case (L8 Expr) fe
                                                            [(define ,x ,e0) (remove* (list x) (append (f (cdr e*) e) (free-vars e0)))]
                                                            [else (append (free-vars fe) (f (cdr e*) e))]))))]
                 [(primapp ,pr ,e* ... ,e) (append (append-map free-vars e*) (free-vars e))]
                 [(if ,e0 ,e1 ,e2) (append (free-vars e0) (free-vars e1) (free-vars e2))]
                 [(list ,e* ,e) (append (append-map free-vars e*) (free-vars e))]
                 [(lambda ([,x* ,t*] ... ) ,body* ... ,body) (remove* x* (append (append-map free-vars body*) (free-vars body)))]
                 [(let ([,x* ,t* ,e*]) ,body* ... ,body) (remove* (list x*) (append (free-vars e*) (append-map free-vars body*) (free-vars body)))]
                 [(letrec ([,x* ,t* ,e*]) ,body* ... ,body) (remove* (list x*) (append (free-vars e*) (append-map free-vars body*) (free-vars body)))]
                 [(letfun ([,x* ,t* ,e*]) ,body) (remove* (list x*) (append (free-vars e*)(free-vars body)))]
                 [(define ,x ,e) (free-vars e)]
                 [(while [,e0] ,e1) (append (free-vars e0) (free-vars e1))]
                 [(for [,x ,e0] ,e1) (append (free-vars e0) (remove* (list x)(free-vars e1)))]
                 [else '()]))

;; Pass that verifies that expressions in L8 don't contain free variables
(define-pass verify-vars : L8 (e) -> L8 ()
  (Expr : Expr (e) -> Expr ()
        [,x (error "Free variable: " x)]
        [,c c]
        [else (let* ([l (free-vars e)])
                (if (empty? l)
                    e
                    (error "Free variables: " (remove-duplicates l))))]))


