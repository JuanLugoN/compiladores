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

;; Currifica las funciones lambda y las aplicaciones de funciones.
;; curry: L8(e) -> L9(e)
(define-pass curry : L8(ir) -> L9()
  (Expr : Expr (ir) -> Expr ()
        [(lambda ((,x* ,t*) ...) ,[body])
         (let bindings ([binding-x* x*]
                        [binding-t* t*])
           (if (= (length binding-x*) 1)
               `(lambda ([,(car binding-x*) ,(car binding-t*)]) ,body)
               `(lambda ([,(car binding-x*) ,(car binding-t*)])
                  ,(bindings (cdr binding-x*) (cdr binding-t*)))))]
        [(,[e0] ,[e1] ...)
         (let fun-app([fun e0]
                      [arg e1])
           (if (= (length arg) 0)
               `,fun
               (fun-app `(,fun ,(car arg)) (cdr arg))))]))

;; Colocar las anotaciones de tipos correspondientes a las constantes
(define-pass type-const : L9 (ir) -> L10 ()
  (Expr : Expr (e) -> Expr ()
        [(quot ,c)
         (cond
           [(boolean? c) `(const Bool ,c)]
           [(number? c) `(const Int ,c)]
           [(char? c) `(const Char ,c)])]))

;; Verifica si t1 es unificable con t2 sin regresar el unificador
(define (unify t1 t2)
  (if (and (type? t1) (type? t2))
      (cond 
        [(equal? t1 t2) #t]
        [(and (equal? 'List t1) (list? t2)) (equal? (car t2) 'List)]
        [(and (equal? 'List t2) (list? t1)) (equal? (car t1) 'List)]
        [(and (list? t1) (list? t2))
         (and (unify (car t1) (car t2)) (unify (caddr t1) (caddr t2)))]
        [else #f])
      (error "Se esperaban 2 tipos")))

;; Regresa el tipo de una variable dado un contexto.
(define (get x ctx)
  (if (empty? ctx)
      (error "No existe una declaración de la variable dentro del contexto.")
      (let ([d (car ctx)])
        (if (equal? x (car d))
            (cdr d)
            (get x (cdr d))))))

;; Algoritmo de inferencia J.
;; L10(e) -> typeof(e)
(define (J expr ctx)
  (nanopass-case (L10 Expr) expr
                 [,x (get x ctx)]
                 [(const ,t ,c) t]
                 [(begin ,e* ... ,e) (J e ctx)]
                 ;; Verificamos operadores primarios
                 [(primapp ,pr ,e* ...)
                  (let* ([args-type (map (lambda (x) (J x ctx)) e*)]
                         [correct-type?
                          (case pr
                            [(+ - / *) (andmap (lambda (x) (equal? x 'Int)) args-type)]
                            [(length car cdr) (andmap (lambda (x) (and (list? x) (equal? (car x) 'List))) args-type)])])
                    (if correct-type?
                        (case pr
                          [(+ - / *) 'Int]
                          [(length) 'Int]
                          [(car) (caddr (car args-type))]
                          [(cdr) (car (args-type))])
                        (error "Los tipos de los argumentos no corresponden con la operación")))]
                 
                 [(if ,e0 ,e1 ,e2)
                  (let ([t0 (J e0 ctx)]
                        [t1 (J e1 ctx)]
                        [t2 (J e2 ctx)])
                    (if (and (unify t0 'Bool) (unify t1 t2))
                        t1
                        (error "Las ramas del if no tienen el mismo tipo y/o la condición no es booleana.")))]
                 [(lambda ([,x ,t]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                         [type (J body ctxN)])
                    `(,t → ,type))]
                 [(let ([,x ,t ,e]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                         [t0 (J e ctx)]
                         [t1 (J body ctxN)])
                    (if (unify t t0)
                        t1
                        (error "Los tipos de la varible y su asignación son distintos en 'let'.")))]
                 [(letrec ([,x ,t ,e]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                         [t0 (J e ctxN)]
                         [t1 (J body ctxN)])
                    (if (unify t t0)
                        t1
                        (error "Los tipos de la varible y su asignación son distintos en 'letrec'.")))]
                 [(letfun ([,x ,t ,e]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                         [t0 (J e ctx)]
                         [t1 (J body ctxN)])
                    (if (unify t t0)
                        t1
                        (error "Tipos función de 'letfun' son incorrectos.")))]
                 [(list ,e* ...)
                  (if (empty? e*)
                      'List
                      (let ([t (J (car e*) ctx)])
                        (let f ([e* e*])
                          (let ([ti (J (car e*) ctx)])
                            (if (unify t ti)
                                (if (equal? (length e*) 1)
                                    (list 'List 'of t)
                                    (f (cdr e*)))
                                (error "La lista no es homogenea."))))))]
                 [(,e0 ,e1)
                  (let*
                      ([t0 (J e0 ctx)]
                       [t1 (J e1 ctx)])
                    (if (unify t0 `(,t1 →,(last t0)))
                        (last t0)
                        (error "El dominio y la entrada de la función son distintas")))]))

;; Quita la anotación de tipo Lambda y la sustituye por el tipo ’(T → T) que corresponda a la definición de la función,
;; Y sustituye las anotaciones de tipo List por el tipo (List of T) de ser necesario.
;; L10(e) -> L10(e)
(define-pass type-infer : L10 (ir) -> L10 ()
  (Expr : Expr (e) -> Expr ()
        [(letrec ([,x ,t ,e]) ,body)
         (let ([s (J e '())])
           `(letrec ([,x ,s ,e]) ,body))]
        [(letfun ([,x ,t ,e]) ,body)
         (let ([s (J e '())])
           `(letfun ([,x ,s ,e]) ,body))]
        [(let ([,x ,t ,e]) ,body)
         (if (equal? t 'List)
             (let ([s (J e '())])
               `(let ([,x ,s ,e]) ,body))
             `(let ([,x ,t ,e]) ,body))]))

(display "\nEjemplo 1: (curry '(lambda ([x Int] [y Int]) (primapp + x y)))\n")
(curry (parse-L8 '(lambda ([x Int] [y Int]) (primapp + x y))))

(display "\nEjemplo 2: (curry '(foo x y)))\n")
(curry (parse-L8 '(foo x y)))

(display "\nEjemplo 3: (curry ’(quot 5)) \n")
(type-const (parse-L9 '(quot 5)))

(display "\nEjemplo 4: (J ’(lambda ([x Int]) x) ’()) \n")
(J (parse-L10 '(lambda ([x Int]) x)) '())

(display "\nEjemplo 5: (type-infier '(letrec ([foo Lambda (lambda ([x Int]) x)]) (foo 5)))) \n")
(type-infer (parse-L10 '(letrec ([foo Lambda (lambda ([x Int]) x)]) (foo (const Int 5)))))

(display "\nEjemplo 6: (type-infer '(let ([x List (list)]) x)) \n")
(type-infer (parse-L10 '(let ([x List (list)]) x)))

(display "\nEjemplo 7: (type-infer '(let([x List (list 1 2 3 4)]) x)) \n")
(type-infer (parse-L10 '(let ([x List (list (const Int 1) (const Int 2) (const Int 3) (const Int 4))]) x)))

(display "\nEjemplo 8: (J '(if (const Bool #t) (const Int 5) (const Int 3))  '()) \n")
 (J (parse-L10 '(if (const Bool #t ) (const Int 5) (const Int 3)))  '() ) 
; Desired response:
; 'Int

(display "\nEjemplo 2.  (J '(list) '())\n")
 (J (parse-L10 '(list)) '())
; Desired response:
;'List

;;Ejemplo 3
(display "\nEjemplo 3.  (J '(list(const Char h) (const Char o) (const Char 1) (const Char a)) '()) \n")
 (J (parse-L10 '(list (const Char #\h) (const Char #\o) (const Char #\1) (const Char #\a))) '()) 
; Desired response:
; '(List of Char)

;;Ejemplo 4
(display "\nEjemplo 4.  (J '(letrec ([x Int (primapp + (const Int 2) (const Int 3))])(primapp * (const Int 4) x)) '()) \n")
 (J (parse-L10 '(letrec ([x Int (primapp + (const Int 2) (const Int 3))])(primapp * (const Int 4) x)))'())
; Desired response:
; 'Int

;;Ejemplo 5
(display "\nEjemplo 5.  (J '(letfun ([foo (Int → Int) (lambda ([x Int]) x)]) (foo (const Int 5))) '()) \n")
 (J (parse-L10 '(letfun ([foo (Int → Int) (lambda ([x Int]) x)]) (foo (const Int 5)))) '())
; Desired response:
; 'Int

;;Ejemplo 6
(display "\nEjemplo 6.  (J '(lambda ([x Bool]) (primapp + x (const Int 7))) '()) \n")
(with-handlers ([exn:fail? (lambda (exn) (displayln (exn-message exn)))])
(J (parse-L10 '(lambda ([x Bool]) (primapp + x (const Int 7)))) '()))
; Desired response:
; error: Los tipos de los argumentos no corresponden con la operación