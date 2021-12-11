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

;; Definición del lenguaje L8
(define-language L8
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (type (t)))
  (Expr (e body)
        x
        (quot c)
        (begin e* ... e)
        (primapp pr e* ...)
        (if e0 e1 e2)
        (lambda ([x* t*] ...) body)
        (let ([x t e]) body)
        (letrec ([x t e]) body)
        (letfun ([x t e]) body)
        (letrec ([x t e]) body)
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

;; Parser L8
(define-parser parse-L8 L8)

;; Lenguaje L9 con expresiones lambda y aplicaciones de funciones currificadas.
(define-language L9
  (extends L8)
  (Expr (e body)
        (- (lambda ([x* t*] ...) body)
           (e0 e1 ...))
        (+ (lambda ([x t]) body)
           (e0 e1))))

;; Parser L9
(define-parser parse-L9 L9)

;; Lenguaje L10 un constructor (conts t c)
(define-language L10
  (extends L9)
  (Expr (e body)
        (- (quot c))
        (+ (const t c))))

;; Parser L10
(define-parser parse-L10 L10)