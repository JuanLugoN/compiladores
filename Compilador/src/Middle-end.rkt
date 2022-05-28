#lang nanopass
(require "Language.rkt")
(require "Front-end.rkt")
(provide (all-defined-out))
#|
Compiladores 2022-1

Integrantes de equipo/Autores:
Nombre | No.cuenta| correo
Juan García Lugo, 316161013, juanlugo@ciencias.unam.mx
Nestor Semer Vazquez Cordero, 316041625, nestor2502@ciencias.unam.mx
Angel Christian Pimentel Noriega, 316157995, cristianp@ciencias.unam.mx
|#
;; Pass for currying lambda expressions and function applications
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

;; Pass that adds type anotations to language's constants.
(define-pass type-const : L9 (ir) -> L10 ()
  (Expr : Expr (e) -> Expr ()
        [,c
         (cond
           [(boolean? c) `(const Bool ,c)]
           [(number? c) `(const Int ,c)]
           [(char? c) `(const Char ,c)])]))

;; Function that verifies if a type t1 is unfiable with a type t2.
;; It doesn't return the unifier, it returns a boolean.
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

;; Function 'get' that gets the type of a variable given a context.
(define (get x ctx)
  (if (empty? ctx)
      (error "No existe una declaración de la variable dentro del contexto.")
      (let ([d (car ctx)])
        (if (equal? x (car d))
            (cdr d)
            (get x (cdr d))))))

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
                        (error "El dominio y la entrada de la función son distintas")))]
                 [(define ,x ,e) (J e ctx)]
                 [(while [,e0] ,e1) (J e1 ctx)]
                 [(for [,x ,e0] ,e1)
                  (let ([t (typeof e0)])
                    (J e1 (set-add ctx (cons x t))))]))

;; Function that given a list expression in L10
;; returns the type of the elements of the list.
(define (typeof expr)
  (let ([t (J expr '())]) 
    (if (c-type? t)
        (let* ([l (car t)]
               [s (cadr t)]
               [lt (caddr t)])
          (if (and (equal? l 'List) (equal? s 'of))
              lt
              (error "No es una lista.")))
        (error "No es una lista."))))

;; Pass for L10 that substitutes type Lambda references for type T->T
;; and type List references for type (List of T) if necessary.
(define-pass type-infer : L10 (ir) -> L10 ()
  (Expr : Expr (e) -> Expr ()
        [(letrec ([,x ,t ,e]) ,body)
         (let ([s (J e '())])
           `(letrec ([,x ,s ,e]) ,body))]
        [(letfun ([,x ,t ,e]) ,body)
         (let ([s (J e '())])
           `(letfun ([,x ,s ,e]) ,body))]
        [(let ([,x ,t ,e]) ,body)
         (if (and (b-type? t) (equal? t 'List))
             (let ([s (J e '())])
               `(let ([,x ,s ,e]) ,body))
             `(let ([,x ,t ,e]) ,body))]))

;; Function that given a lambda expression from L10 returns a pair
;; made from the assignments and the body of the expression.
(define (uncurry-aux expr)
  (nanopass-case (L10 Expr) expr
                 [(lambda ([,x ,t]) ,body) (cons (cons x t) body)]
                 [else (error "Expected lambda expression.")]))

;; Predicate for lambda expressions.
(define-pass lambda? : (L10 Expr) (e) -> * (bool)
  (Expr : Expr (e) -> * (bool)
        [(lambda ([,x ,t]) ,body) #t]
        [else #f])
  (Expr e))

;; Pass that uncurryfies lambda expressions from L10.
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