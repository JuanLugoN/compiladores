#lang nanopass
#|
Compiladores 2022-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Naomi Itzel Reyes Granados
Laboratorio: Nora Hilda Hernández Luna

Parser Nanopass
Integrantes de equipo/Autores:
Nombre | No.cuenta| correo
Juan García Lugo, 316161013, juanlugo@ciencias.unam.mx
Nestor Semer Vazquez Cordero, 316041625, nestor2502@ciencias.unam.mx
Angel Christian Pimentel Noriega, 316157995, cristianp@ciencias.unam.mx
|#
(require nanopass/base)

; The definition of our language
(define-language LF
  (terminals
   (variable (x)) ;;Done
   (primitive (pr))
   (constant (c))
   (cadena (s))
   (caracter (c1))
   (type (t)) ;; Done
   (lista (l))) ;; Done
  ;; e ::= x | pr | c | s | t | l | begin (e,...,e) | if(e,e) | if(e,e,e)
  (Expr (e body)
    x
    pr
    c
    s
    c1
    t
    l
    (begin e* ... e)
    (if e0 e1)
    (if e0 e1 e2)
    (fun ((x* t*) ...) t body* ... body)
    (let ((x* t* e*) ...) body* ... body)
    (funF x ((x* t*) ...) t body* ... body)
    (e0 e1 ...)
    (pr e* ... e)))

; Exercise 3

; Define predicates
(define (variable? x)
  (symbol? x))

(define (primitive? x)
  (or (procedure? x) (memq x '(and or + * - /))))

(define (constant? x)
  (or (number? x) (boolean? x)))

(define (cadena? x)
  (string? x))

(define (caracter? x)
  (char? x))


(define (lista? x)
  (list? x))

(define (type? t)
  ;; T ::= Int | Bool | String | Char
 (or (equal? t 'Int) (equal? t 'Bool) (eq? t 'String) (eq? t 'Char))) 

; The parser of LF
(define-parser parse-LF LF)

; Exercise 4

;; aux. dado numero n, regresa 'xn
(define (format-varname n)
  (string->symbol (string-append "x" (number->string n))))

;; ctx = [(var1, idx1), ..., (varn, idxn)]
;; aux. dado un contexto `ctx` y un simbolo `x`, regresa el simbolo 'xidxi si existe `i` tal que
;; `vari = x`, si no existe regresa el simbolo 'x0 (si x es variable libre)
(define (get-varname x ctx)
  (if (empty? ctx)
      'x0
      (if (equal? x (car (first ctx)))
          (format-varname (cdr (first ctx)))
          (get-varname x (rest ctx)))))

;; ctx = [(var1, idx1), ..., (varn, idxn)]
;; aux. regresa el mismo contexto con los indices aumentados por uno, es decir, regresa
;; [(var1, idx1+1), ..., (varn, idxn+1)]. esta funcion es para actualizar el contexto cuando se
;; encuentra una funcion las variables "aumentan en profundidad"
(define (bump-ctx ctx)
  (if (empty? ctx)
      null
      (cons (cons (car (first ctx)) (+ 1 (cdr (first ctx)))) (bump-ctx (rest ctx)))))

;; old-ctx = [(var1, idx1), ..., (varn, idxn)]
;; new-var = (var, idx)
;; aux. regresa el contexto `old-ctx` pero agerando la nueva variable. en caso de que la variable
;; `var` ya exista en old-ctx, se actualizara el indice a `idx` para que nuevas ocurrencias de `var`
;; esten asociadas a la funcion que la declara mas reciente
(define (merge-ctx old-ctx new-var)
  (if (empty? old-ctx )
      (cons new-var null)
      (if (equal? (car new-var) (car (first old-ctx)))
          (cons new-var (rest old-ctx))
          (cons (first old-ctx) (merge-ctx (rest old-ctx) new-var)))))

;; old-ctx = [(var1, idx1), ..., (varn, idxn)];;
;; vars = [x1, ..., xm]
;; aux. regresa un nuevo contexto aumentando las variables en `vars`, en donde `x1` se agregara
;; perimero al contexto, es decir, se considerara mas atras que `x2`...`xm` para ocurrencias
;; de las variables mas adelante
(define (update-ctx vars old-ctx)
  (if (empty? vars)
      old-ctx
      (update-ctx (rest vars) (bump-ctx (merge-ctx old-ctx (cons (first vars) 0))))))

;; aux. regresa las variables en `vars` renombradas segun el contexto `ctx`
(define (renames vars ctx)
  (if (empty? vars)
      null
      (cons (get-varname (first vars) ctx) (renames (rest vars) ctx))))

;; nuevo lenguaje para indices de bruijn en donde se quitan los parametros de las variables en
;; las funciones, sus tipos y los tipos que regresan, de este modo se puede implementar los indices
;; de bruijn de manera estandar
(define-language LBruijn
  (extends LF)
  (Expr (e body)
        (+ (fun body* ... body))
        (+ (funF x body* ... body))
        (- (fun ((x* t*) ...) t body* ... body))
        (- (funF x ((x* t*) ...) t body* ... body))))

;; parser bruijn
(define-parser parse-bruijn LBruijn)

;; regresa el simbolo 'fun tantas veces como `types` lo indica
(define (fun-bruijn types)
  (if (<= (length types) 1)
      'fun
      (string->symbol (string-append "fun " (symbol->string (fun-bruijn (rest types)))))))

(define-pass rename-var : LF (ir) -> LBruijn ()
  (Expr : Expr (ir [ctx* null]) -> Expr ()
    [,x (get-varname x ctx*)]
    [(fun ([,x* ,t*] ...) ,t ,[Expr : body* (update-ctx x* ctx*) -> e1*] ... ,[Expr : body (update-ctx x* ctx*) -> e2])
     `(fun ,(fun-bruijn (rest t*)) ,e1* ... ,e2)]
    [(funF ,x ([,x* ,t*] ...) ,t ,[Expr : body* (update-ctx x* ctx*) -> e1*] ... ,[Expr : body (update-ctx x* ctx*) -> e2])
     `(funF ,x ,(fun-bruijn (rest t*)) ,e1* ... ,e2)]
    ))

; A function that make explicit the ocurrences of the begin
(define-pass make-explicit : LF (ir) -> LF ()
  (Expr : Expr (ir) -> Expr ()
    [,c `',c]
    [(fun ([,x* ,t*] ...) ,t ,[body*] ... ,[body])
     `(fun ([,x* ,t*] ...) t (begin ,body* ... ,body))]
    [(let ([,x* ,t* ,[e*]] ...) ,[body*] ... ,[body])
     `(let ([,x* ,t* ,e*] ...) (begin ,body* ... ,body))]
    [(funF ,x ([,x* ,t*] ...) ,t ,[body*] ... ,[body])
     `(funF x ([,x* ,t*] ...) t (begin ,body* ... ,body))]))

; Exercise 5

; Define a new language without if single branch
(define-language LNI
  (extends LF)
  (Expr (e body)
        (- (if e0 e1))))

; Parser LNI 
(define-parser parse-LNI LNI)

; Remove single branch if amd convert them into two branch if
(define-pass rm-one-armed-if : LF (ir) -> LNI ()
  (Expr : Expr (ir) -> Expr ()
    [,c `',c]
    [(if ,[e0] ,[e1])
     `(if ,e0 ,e1 (void))]))

; Exercise 6

;; Define a new language without strings
(define-language LNI2
  (extends LNI)
  (terminals
   (- (cadena (s))))
  (Expr (e body)
        (- s)))

;; Parser LNI2
(define-parser parse-LNI2 LNI2)

;; Remove string of LF lenguage
(define-pass remove-string : LNI(ir) -> LNI2 ()
  (Expr : Expr (ir) -> Expr()
        [,s `(lista, (string->list s))]))