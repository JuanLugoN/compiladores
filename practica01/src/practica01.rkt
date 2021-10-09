#lang racket
;---------------------- Definición de árbol binario ----------------------;
;; Definición de árbol binario
;; T := void | T node T
;; node := e
(define-struct e () #:transparent)
(define-struct tree (n l r)  #:transparent)
;---------------------- funciones auxiliares ----------------------;
;; Obtiene recursivamente el numero de horas que dependen si es un año bisiesto o no
;; fibo: numero de años, año actual -
(define (time-since-aux n a)
  (if (zero? n)
      0
        (if (zero? (modulo a 4))
          (+ (* 366 24) (time-since-aux (- n 1) (- a 1)))
          (+ (* 365 24) (time-since-aux (- n 1) (- a 1))))))
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
    [(e? t) '()]
    [(tree? t) (if (and (e? (tree-l t)) (e? (tree-r t)))
                   (cons (tree-n t) '())
                   (append (get-leaves (tree-l t)) (get-leaves (tree-r t))))]))

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
    [(e? t) 0]
    [(tree? t) (add1 (max (tree-height (tree-l t)) (tree-height (tree-r t))))]))

;; Regresa el número de nodos en un árbol binario
;; nodes-number: T -> number
(define (nodes-number t)
  (cond
    [(e? t) 0]
    [(tree? t) (+ 1 (nodes-number (tree-l t)) (nodes-number (tree-r t)))]))

;; Verifica si un árbol binario es completo
;; is-perfect?: T -> boolean
(define (is-perfect? t)
  (= (sub1 (expt 2 (tree-height t))) (nodes-number t)))

;;función auxiliar para enumerate
(define (agrega-nivel raiz subl subr original resultado)
  (if (empty? subl)
      resultado
      (if (empty? subr)
          (agrega-nivel raiz (cdr subl) original original resultado)
          (agrega-nivel raiz subl (cdr subr) original (cons (tree raiz (car subl) (car subr)) resultado)))))

;;función auxiliar para enumerate
(define (enumerate-aux n l)
  (if (equal? n 0)
      (cons (e) l)
      (let ([l1 (cons (e) l)])
        (enumerate-aux (- n 1) (append (agrega-nivel 1 l1 l1 l1 empty) (agrega-nivel 2 l1 l1 l1 empty))))))

;---------------------- Ejercicios ----------------------;
;; Muestra la cantidad de horas, minutos y segundos que han pasado durante n años
;; Toma en cuenta si han pasado años binarios o no
;; time-since; number
(define (time-since n)
  (let ([horas (time-since-aux n 2021)])
    (display (string-append  (~a n) " año(s) se traducen a: \n"))
    (display (string-append  (~a horas) " horas. \n"))
    (display (string-append  (~a (* horas 60)) " minutos. \n"))
    (display (string-append  (~a (* (* horas 60) 60) " segundos.")))))

;; Regresa una lista con la suceción de fibonacci hasta el n-ésimo elemento de la sucesión
;; fib: number -> list(number)
(define (fib n)
  (reverse (fibo n)))

;; Regresa un árbol de divisores de un número n tiene como raíz n.
;; div-tree: number -> Tree
(define (div-tree n)
  (let ([k (div-min n 2)])
    (if (= -1 k)
        (tree n (e) (e))
        (tree n (tree k (e) (e)) (div-tree (/ n k))))))

;; Regresa una lista con la representación de un número n como producto de factores primos.
;; prime-fac: number -> list(number)
(define (prime-fac n)
  (get-leaves (div-tree n)))

;; Regresa una lista de todos los árboles de nivel a lo más d construidos de abajo hacia arriba.
;; enumerate: number -> list(tree)
(define (enumerate n)
  (enumerate-aux n empty))

;; Regresa una lista con árboles binarios perfectos
;; perfect-trees: list(T) -> list(T)
(define (perfect-trees l)
  (filter is-perfect? l))