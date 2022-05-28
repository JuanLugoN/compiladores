#lang nanopass
(require nanopass/base)
(provide (all-defined-out))
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

;; Definición del lenguaje L10
(define-language L10
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (type (t)))
  (Expr (e body)
        x
        (const t c)
        (begin e* ... e)
        (primapp pr e* ...)
        (if e0 e1 e2)
        (lambda ([x t]) body)
        (let ([x t e]) body)
        (letrec ([x t e]) body)
        (letfun ([x t e]) body)
        (list e* ...)
        (e0 e1 ...)))

;; Definición de predicados
(define (variable? x)
  (symbol? x))

(define (primitive? pr)
  (or (procedure? pr) (memq pr '(length car cdr + * - /))))

(define (constant? c)
  (or (number? c) (boolean? c) (char? c)))

(define (b-type? x) (memq x '(Bool Char Int List Lambda)))

(define (c-type? x)
  (if (list? x)
      (let* ([f (car x)]
             [s (cadr x)]
             [t (caddr x)])
        (or (and (equal? f 'List) (equal? s 'of) (type? t))
            (and (type? f) (equal? s '→) (type? t))))
      #f))

(define (type? x) (or (b-type? x) (c-type? x)))

;; Parser L10
(define-parser parse-L10 L10)

; Lenguaje L11 con lambdas descurrificadas
(define-language L11
  (extends L10)
  (Expr (e body)
        (- (lambda ([x t]) body))
        (+ (lambda ([x* t*] ...) body))))

;; Parser L11
(define-parser parse-L11 L11)

;; Lenaguaje L12 sin el valor asociado a los identificadores y el tipo correspondiente
(define-language L12
  (extends L11)
  (Expr (e body)
        (- (let ([x t e]) body)
           (letrec ([x t e]) body)
           (letfun ([x t e]) body))
        (+ (let x body)
           (letrec x body)
           (letfun x body))))

;; Parser L12
(define-parser parse-L12 L12)

(define-language L13
  (extends L12)
  (terminals
        (+ (len (l))))
  (Expr (e)
        (- (list e* ...))
        (+ (array l t e* ...))))

(define (len? x)
  (number? x))

(define-parser parse-L13 L13)