#lang plai

;; Definición de árbol binario
(define-type Tree
  [none]
  [T (left Tree?) (node any/c)  (right Tree?)])

;; Regresa una lista con la suceción de fibonacci hasta el n-ésimo elemento de la sucesión en orden inverso.
;; fibo: number -> list(number)
(define (fibo n)
  (case n
    [(0) (list 1)]
    [(1) (list 1 1)]
    [else (let ([f (fibo (- n 1))])
            (cons (+ (first f) (second f)) f))]))

;; Regresa las hojas de un árbol
;; get-leaves; Tree - list(number)
(define (get-leaves t)
  (type-case Tree t
    [none () '()]
    [T (l n r) (if (and (none? l) (none? r))
                   (cons n '())
                   (append (get-leaves l) (get-leaves r)))]))

;; Regresa el divisor mas pequeño de un número n si no hay regresa -1
;; div-min: number -> number
(define (div-min n k)
  (if (>= k n)
      -1
      (if (= 0 (modulo n k)) k (div-min n (add1 k)))))

;; Regresa una lista con la suceción de fibonacci hasta el n-ésimo elemento de la sucesión
;; fib: number -> list(number)
(define (fib n)
  (reverse (fibo n)))

;; Regresa un árbol de divisores de un número n tiene como raíz n.
;; div-tree: number -> Tree
(define (div-tree n)
  (let ([k (div-min n 2)])
    (if (= -1 k)
        (T (none) n (none))
        (T (T (none) k (none)) n (div-tree (/ n k))))))

;; Regresa una lista con la representación de un número n como producto de factores primos.
;; prime-fac: number -> list(number)
(define (prime-fac n)
  (get-leaves (div-tree n)))

