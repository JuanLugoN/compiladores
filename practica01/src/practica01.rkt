#lang racket
;---------------------- Definición de árbol binario ----------------------;
;; Definición de árbol binario
;; T := void | T node T
;; node := e
(struct T (left node right)  #:inspector #f)
;---------------------- funciones auxiliares ----------------------;
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
  (cond
    [(void? t) '()]
    [(T? t) (if (and (void? (T-left t)) (void? (T-right t)))
                   (cons (T-node t) '())
                   (append (get-leaves (T-left t)) (get-leaves (T-right t))))]))

;; Regresa el divisor mas pequeño de un número n si no hay regresa -1
;; div-min: number -> number
(define (div-min n k)
  (if (>= k n)
      -1
      (if (= 0 (modulo n k)) k (div-min n (add1 k)))))

;; Regresa la profundidad de un árbol binario
;; tree-height: T -> number
(define (tree-height t)
  (cond
    [(void? t) 0]
    [(T? t) (add1 (max (tree-height (T-left t)) (tree-height (T-right t))))]))

;; Regresa el número de nodos en un árbol binario
;; nodes-number: T -> number
(define (nodes-number t)
  (cond
    [(void? t) 0]
    [(T? t) (+ 1 (nodes-number (T-left t)) (nodes-number (T-right t)))]))

;; Verifica si un árbol binario es completo
;; is-perfect?: T -> boolean
(define (is-perfect? t)
  (= (sub1 (expt 2 (tree-height t))) (nodes-number t)))

;---------------------- Ejercicios ----------------------;
;; Regresa las horas, minutos y segundos pasasados desde hace n años
;; time-since: number -> number
(define (time-since n) '())

;; Regresa una lista con la suceción de fibonacci hasta el n-ésimo elemento de la sucesión
;; fib: number -> list(number)
(define (fib n)
  (reverse (fibo n)))

;; Regresa un árbol de divisores de un número n tiene como raíz n.
;; div-tree: number -> Tree
(define (div-tree n)
  (let ([k (div-min n 2)])
    (if (= -1 k)
        (T (void) n (void))
        (T (T (void) k (void)) n (div-tree (/ n k))))))

;; Regresa una lista con la representación de un número n como producto de factores primos.
;; prime-fac: number -> list(number)
(define (prime-fac n)
  (get-leaves (div-tree n)))

;; Regresa una lista con árboles binarios perfectos
;; perfect-trees: list(T) -> list(T)
(define (perfect-trees l)
  (filter is-perfect? l))