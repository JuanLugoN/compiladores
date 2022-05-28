#lang nanopass
(require "Language.rkt")
#|
Compiladores 2022-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Naomi Itzel Reyes Granados
Laboratorio: Nora Hilda Hernández Luna, Fernando Abigail Galicia Mendoza

Integrantes de equipo/Autores:
Nombre | No.cuenta| correo
Juan García Lugo, 316161013, juanlugo@ciencias.unam.mx
Nestor Semer Vazquez Cordero, 316041625, nestor2502@ciencias.unam.mx
Angel Christian Pimentel Noriega, 316157995, cristianp@ciencias.unam.mx
|#

;; Predicado para expresiones lambda.
;; L10(e) -> Boolean
(define-pass lambda? : (L10 Expr) (e) -> * (bool)
  (Expr : Expr (e) -> * (bool)
    [(lambda ([,x ,t]) ,body) #t]
    [else #f])
  (Expr e))

;; Devuelve un par  a partir de las asignaciones y el cuerpo de la expresión lambda.
;; uncurry-aux L10(e) -> List
(define (uncurry-aux expr)
        (nanopass-case (L10 Expr) expr
                       [(lambda ([,x ,t]) ,body) (cons (cons x t) body)]
                       [else (error "Expected lambda expression.")]))

;; Genera la tabla de símbolos de una expresión del lenguaje
;; stv: L11(e) Hashtable -> Hashtable
(define (stv expr ht)
  (nanopass-case (L11 Expr) expr
                 [(let ([,x ,t ,e]) ,body) (begin
                                             (hash-set! ht x (cons t e))
                                             (stv body ht))]
                 [(letrec ([,x ,t ,e]) ,body) (begin
                                                (hash-set! (stv body ht) x (cons t e))
                                                (stv body ht))]
                 [(,e0 ,e1) (begin
                              (define ht0 ht)
                              (set! ht0 (stv e0 ht0))
                              (define ht1 ht0)
                              (set! ht1 (stv e1 ht1))
                              ht1)]
                 [(primapp ,pr ,[e*] ...) (let f ([e* e*])
                                            (if (null? e*)
                                                ht
                                                (stv (first e*) (f (rest e*)))))]
                 [(begin ,e* ... ,e) (begin
                                       (map (lambda (e) (stv e ht)) e*)
                                       (stv e ht))]
                 [(if ,e0 ,e1 ,e2) (begin
                                     (stv e0 ht)
                                     (stv e1 ht)
                                     (stv e2 ht))]
                 [(lambda ([,x* , t*]) ,body) (stv body ht)]
                 [(list ,e* ... ,e) (begin
                                      (map (lambda (e) (stv e ht)) e*)
                                      (stv e ht))]
                 [else ht]))

;; Regresa el tipo de la lista
(define (typeof exp)
  (nanopass-case (L12 Expr) exp
                 [(list ,[e*] ... )
                   (let ([type-list (map unparse-L12 e*)])
                         (car type-list))]
                 [(const, t, c) t]))

;; Descurrifica las funciones lambda.
;; uncurry: L10(e) -> L11(e)
(define-pass uncurry : L10 (ir) -> L11 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda ([,x ,t]) ,body)
         (if (lambda? body)
             (let* ([bindings (car (uncurry-aux body))]
                    [ax (car bindings)]
                    [at (cdr bindings)]
                    [newbody (cdr (uncurry-aux body))])
               `(lambda ([,x ,t] [,ax ,at]) ,(uncurry newbody)))
             `(lambda ([,x ,t]) ,(uncurry body)))]))

;; Genera la tabla de símbolos de una expresión del lenguaje
;; L11(e) -> HashTable
(define (symbol-table-var expr)
  (nanopass-case (L11 Expr) expr
                 [else (stv expr (make-hash))]))

;; Modifica los constructores let, letrec y letfun, eliminando el valor asociado a los identificadores
;; y el tipo correspondiente
;; assigment: L11(e) -> L12(e)
(define-pass assigment : L11 (ir) -> L12 (hash)
  (Expr : Expr (ir) -> Expr ()
        [(let ([,x ,t ,e]) ,[body]) `(let ,x ,body)]
        [(letrec ([,x ,t ,e]) ,[body]) `(letrec ,x ,body)]
        [(letfun ([,x ,t ,e]) ,[body]) `(letfun ,x ,body)])
  (values (Expr ir) (symbol-table-var ir)))

;; Traduce listas en arreglos
;; list-to-array: L12(e) -> L13(e)
(define-pass list-to-array : L12 (ir) -> L13 ()
  (Expr : Expr (ir) -> Expr ()
        [(list ,[e*] ... ) `(array ,(length e*) ,(typeof ir) ,e* ...)]))

(display "\nEjemplo 1: (uncurry ’(lambda ([x Int]) (lambda ([y Int]) (+ x y))))\n")
(uncurry (parse-L10 '(lambda ([x Int]) (lambda ([y Int]) (+ x y)))))

(display "\nEjemplo 2: (symbol-table-var '(letrec ([foo Lambda (lambda ([x Int]) x)]) (foo (const Int 5))))\n")
(symbol-table-var (parse-L11 '(letrec ([foo Lambda (lambda ([x Int]) x)]) (foo (const Int 5)))))

(display "\nEjemplo 3: (assigment '(letrec ([foo (Int → Int) (lambda ([x Int]) x)]) (foo 5)))\n")
(assigment (parse-L11 '(letrec ([foo (Int → Int) (lambda ([x Int]) x)]) (foo (const Int 5)))))

(display "\nEjemplo 4: (assigment '(list (const Int 2) (const Int 4) (list (const Int 5))))\n")
(list-to-array (parse-L12 '( list (const Int 1) (const Int 2) (const Int 3) (const Int 4) (const Int 5))))
