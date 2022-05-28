#lang nanopass
(provide (all-defined-out))
#|
Compiladores 2022-1

Integrantes de equipo/Autores:
Nombre | No.cuenta| correo
Juan García Lugo, 316161013, juanlugo@ciencias.unam.mx
Nestor Semer Vazquez Cordero, 316041625, nestor2502@ciencias.unam.mx
Angel Christian Pimentel Noriega, 316157995, cristianp@ciencias.unam.mx
|#

;; Definition of source language
(define-language LF
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (list (l))
   (string (s))
   (type (t)))
  (Expr (e body)
        x
        pr
        c
        l
        s
        t
        (begin e* ... e)
        (if e0 e1)
        (if e0 e1 e2)
        (lambda ([x* t*] ...) body* ... body)
        (let ([x* t* e*] ...) body* ... body)
        (letrec ([x* t* e*] ...) body* ... body)
        (e0 e1 ...)
        (primapp pr e* ...)
        (list e* ...)
        (define x e)
        (while [e0] e1)
        (for [x e0] e1)))

;; Predicate for the variables
(define (variable? x)
  (and (symbol? x)
       (not (primitive? x))
       (not (constant? x))))

;; Predicate for the types
;; Bool | Int | Char | List | Lambda | List of T | T → T
(define (type? x) (or (b-type? x) (c-type? x)))

;; For Bool, Int, Char and List types
(define (b-type? x) (memq x '(Bool Char Int List Lambda)))

;; For List of T and T → T types
(define (c-type? x) (if (list? x)
                        (let* (
                               [f (car x)]
                               [s (cadr x)]
                               [t (caddr x)])
                          (or (and (equal? f 'List) (equal? s 'of) (type? t))
                              (and (type? f) (equal? s '→) (type? t))))
                        #f))

;; Predicate for the constants
(define (constant? x)
  (or (boolean? x) (number? x) (char? x)))

;; Predicate for primitives
(define (primitive? x)
  (memq x '(+ - * / < > equal? iszero? ++ -- car cdr length and or)))

;; The parser of LF
(define-parser parse-LF LF)

;; Language LNI definition 
;; LNI extends LF, removes one-armed if statements
(define-language LNI (extends LF)
  (Expr (e body)
        (- (if e0 e1))
        ))

; LNI parser
(define-parser parse-LNI LNI)

;; LNS extends LNI, removes strings as terminals
(define-language LNS (extends LNI)
  (terminals
   (- (string (s))))
  (Expr (e body)
        (- s)))

;; LNS parser
(define-parser parse-LNS LNS)

;; Language L7 definition 
;; L7 extends LNS, removes let and letrec from multiple assignments and 
;; adds let and letrec from a single assignment
(define-language L7
  (extends LNS)
  (Expr (e body)
        (- (let ([x* t* e*] ...) body* ... body)
           (letrec ([x* t* e*] ...) body* ... body))
        (+ (let ([x* t* e*]) body* ... body)
           (letrec ([x* t* e*]) body* ... body))))

;; L7 parser
(define-parser parse-L7 L7)

;; Language L8 defintion
;; L8 extends L7, add named functions
(define-language L8
  (extends L7)
  (Expr (e body)
        (+ (letfun ([x* t* e*]) body))))

;; L8 parser
(define-parser parse-L8 L8)

;; Language L9 definition
(define-language L9
  (extends L8)
  (Expr (e body)
        (- (lambda ([x* t*] ...) body* ... body)
           (e0 e1 ...))
        (+ (lambda ([x t]) body)
           (e0 e1))))

;; Language L9 parser
(define-parser parse-L9 L9)

;; Language L10 definition
(define-language L10
  (extends L9)
  (Expr (e body)
        (- c)
        (+ (const t c))))

;; L10 parser
(define-parser parse-L10 L10)

;; Language L11 definition
(define-language L11
  (extends L10)
  (Expr (e body)
        (- (lambda ([x t]) body))
        (+ (lambda ([x* t*] ...) body))))

;; L11 parser
(define-parser parse-L11 L11)

;; Language L12 definition
(define-language L12
  (extends L11)
  (Expr (e body)
        (- (let ([x* t* e*]) body* ... body)
           (letrec ([x* t* e*]) body* ... body)
           (letfun ([x* t* e*]) body))
        (+ (let x body* ... body)
           (letrec x body* ... body)
           (letfun x body))))

;; L12 parser
(define-parser parse-L12 L12)

;; Language L13 definition
(define-language L13
  (extends L12)
  (terminals
   (+ (len (le))))
  (Expr (e)
        (- (list e* ...))
        (+ (array le t e* ...))))

;; Predicate for len
(define (len? x)
  (number? x))

;; L13 parser
(define-parser parse-L13 L13)




















