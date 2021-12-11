#lang nanopass
(require "Language.rkt")
#|
Compiladores 2022-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Naomi Itzel Reyes Granados
Laboratorio: Nora Hilda Hernández Luna

Integrantes de equipo/Autores:
Nombre | No.cuenta| correo
Juan García Lugo, 316161013, juanlugo@ciencias.unam.mx
Nestor Semer Vazquez Cordero, 316041625, nestor2502@ciencias.unam.mx
Angel Christian Pimentel Noriega, 316157995, cristianp@ciencias.unam.mx
|#

;; Lenguaje LF con expresiones currificadas
;; curry-let: LF(e) -> LF-curry(e)
(define-pass curry-let : LF (ir) -> L7 ()
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
                  ,(bindings(cdr binding-x*)(cdr binding-t*)(cdr binding-e*)))
               ))]))

;; Se detecten los let utilizados para definir funciones y se remplazan por letrec.
;; identify-assigments: L7(e) -> L7(e)
(define-pass identify-assigments : L7 (ir) -> L7 ()
  (Expr : Expr (ir) -> Expr()
        [(let ([,x ,t ,[e]]) ,[body])
         (if (equal? t 'Lambda)
             `(letrec ([,x ,t ,e]) ,body)
             ir)]))

;; Asigna un identificador a las funciones anónimas.
;; un-anonymous: L7(e) -> L8(e)
(define-pass un-anonymous : L7 (ir) -> L8 ()
  (Expr : Expr (ir [index 0]) -> Expr ()
        [(lambda ([,[Expr : x* (set! index (add1 index)) -> x*] ,t*] ...) ,[body])
         (let ([i (string->symbol (format "foo~a" index))])
           `(letfun [,i 'Lambda (lambda ([,x* ,t*] ...) ,body)] ,i))]))

;; Verifica la aridad de las primitivas.
;; verify-arity: LF(e) -> LF(e)
(define-pass verify-arity : LF (ir) -> LF ()
  (Expr : Expr (ir) -> Expr ()
        [(primapp ,pr ,[e*] ...)
         (match pr
           ['+ (if (< (length e*) 2 ) (error 'verify-arity "Arity mismatch") ir)]
           ['- (if (< (length e*) 2 ) (error 'verify-arity "Arity mismatch") ir)]
           ['* (if (< (length e*) 2 ) (error 'verify-arity "Arity mismatch") ir)]
           ['/ (if (< (length e*) 2 ) (error 'verify-arity "Arity mismatch") ir)]
           ['car (if (not (= 1 (length e*))) (error 'verify-arity "Arity mismatch") ir)]
           ['cdr (if (not (= 1 (length e*))) (error 'verify-arity "Arity mismatch") ir)]
           ['length (if (not (= 1 (length e*))) (error 'verify-arity "Arity mismatch") ir)])]))

;; Verifica que la expresión no tenga variables libres.
;; verify-vars: LF(e) -> LF(e)
(define-pass verify-vars : LF (ir) -> LF ()
  (Expr : Expr (ir [bound-vars '()]) -> Expr ()
        [,x
         (if (memq x bound-vars)
              x
             (error 'verify-vars (string-append "Free variable " (symbol->string x))))]
        [(let ([,x* ,t* ,[Expr : e* (append x* bound-vars) -> e*]] ...) ,[Expr : body (append x* bound-vars) -> body])
         `(let ([,x* ,t* ,e*] ... ) ,body) ]
        [(letrec ([,x* ,t* ,[Expr : e* (append x* bound-vars) -> e*]] ...) ,[Expr : body* (append x* bound-vars) -> body*] ...,[Expr : body (append x* bound-vars) -> body])
         `(letrec ([,x* ,t* ,e*] ...) ,body* ...,body)]
        [(lambda ([,x* ,t*] ...) , [Expr : body (append x* bound-vars) -> body])
         `(lambda ([,x* ,t* ] ...) ,body)]))

(display "\nEjercicio 1: '(let ([x Int 4] [y Int 6]) (primapp + x y ) )\n")
(curry-let (parse-LF '(let ([ x Int 4] [ y Int 6]) (primapp + x y ) )))

(display "\nEjercicio 2: '(let ([foo Lambda (lambda ([x Int ]) x)]) (foo 5))\n")
(identify-assigments (parse-L7 '(let ([foo Lambda (lambda ([x Int ]) x)]) (foo 5))))

(display "\nEjercicio 3: '(lambda ([x Bool]) (if x 1 2) )\n")
(un-anonymous (parse-L7 '(lambda ([x Bool]) (if x 1 2))))

(display "\nEjercicio 4. '(primapp car 2 3)\n")
(with-handlers ([exn:fail? (lambda (exn) (displayln (exn-message exn)))])
  (verify-arity (parse-LF '(primapp car 2 3))))

(display "\nEjercicio 5. '(primapp + 2 x)\n")
(with-handlers ([exn:fail? (lambda (exn) (displayln (exn-message exn)))])
  (verify-vars (parse-LF '(primapp car 2 x))))

