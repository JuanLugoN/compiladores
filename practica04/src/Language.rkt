#lang nanopass
(require nanopass/base)
(provide (all-defined-out))
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

;; Definición del lenguaje LF
(define-language LF
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (type (t)))
  (Expr (e body)
        x
        c
        (quot c)
        (begin e* ... e)
        (primapp pr e* ...)
        (if e0 e1 e2)
        (lambda ([x* t*] ...) body)
        (let ([x* t* e*] ...) body)
        (letrec ([x* t* e*] ...) body* ... body)
        (list e* e)
        (e0 e1 ...)))

; Definición de predicados
(define (variable? x)
  (symbol? x))

(define (primitive? pr)
  (or (procedure? pr) (memq pr '(length car cdr + * - /))))

(define (constant? c)
  (or (number? c) (boolean? c)))

(define (type? t)
 (or (equal? t 'Bool) (equal? t 'Int) (eq? t 'Char) (eq? t 'List) (eq? t 'Lambda)))

; Parser LF
(define-parser parse-LF LF)

;; Lenguaje LF con expresiones currificadas.
(define-language L7
  (extends LF)
  (Expr (e body)
        (-  (let ([x* t* e*] ...) body)
            (letrec ([x* t* e*] ...) body* ... body))
        (+  (let ([x t e]) body)
            (letrec ([x t e]) body* ... body))))

;; Parser LF.
(define-parser parse-L7 L7)

;; Lenguaje L7 con identificador en funciones anonimas (lambda)
(define-language L8
  (extends L7)
  (Expr (e body)
        (+  (letfun ([x t e]) body*))))

;; Parser L8
(define-parser parse-L8 L8)
