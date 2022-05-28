#lang nanopass
(require "Language.rkt")
(require "Middle-end.rkt")
(provide (all-defined-out))
#|
Compiladores 2022-1

Integrantes de equipo/Autores:
Nombre | No.cuenta| correo
Juan García Lugo, 316161013, juanlugo@ciencias.unam.mx
Nestor Semer Vazquez Cordero, 316041625, nestor2502@ciencias.unam.mx
Angel Christian Pimentel Noriega, 316157995, cristianp@ciencias.unam.mx
|#
(define symbol-hashtable-var (make-hash))

;; Auxiliar function to generates the symbol table of a language expression
(define (stv expr ht)
  (nanopass-case (L11 Expr) expr
                 [(let ([,x ,t ,e]) ,body) (begin
                                             (hash-set! ht x (cons t e))
                                             (stv body ht))]
                 [(letrec ([,x ,t ,e]) ,body) (begin
                                                (hash-set! (stv body ht) x (cons t e))
                                                (stv body ht))]
                 [(,e0 ,e1) (begin
                              (define ht0 ht)
                              (set! ht0 (stv e0 ht0))
                              (define ht1 ht0)
                              (set! ht1 (stv e1 ht1))
                              ht1)]
                 [(primapp ,pr ,[e*] ...) (let f ([e* e*])
                                            (if (null? e*)
                                                ht
                                                (stv (first e*) (f (rest e*)))))]
                 [(begin ,e* ... ,e) (begin
                                       (map (lambda (e) (stv e ht)) e*)
                                       (stv e ht))]
                 [(if ,e0 ,e1 ,e2) (begin
                                     (stv e0 ht)
                                     (stv e1 ht)
                                     (stv e2 ht))]
                 [(lambda ([,x* , t*]) ,body) (stv body ht)]
                 [(list ,e* ... ,e) (begin
                                      (map (lambda (e) (stv e ht)) e*)
                                      (stv e ht))]
                 [else ht]))

;; Generates the symbol table of a language expression
(define (symbol-table-var expr)
  (nanopass-case (L11 Expr) expr
                 [else (stv expr symbol-hashtable-var)]))

;; Pass that modify the let, letrec, and letfun constructors, removing them.
(define-pass assigment-aux : L11 (ir) -> L12 ()
  (Expr : Expr (ir) -> Expr ()
        [(let ([,x ,t ,e]) ,[body]) `(let ,x ,body)]
        [(letrec ([,x ,t ,e]) ,[body]) `(letrec ,x ,body)]
        [(letfun ([,x ,t ,e]) ,[body]) `(letfun ,x ,body)]))

(define (assigment e)
  (symbol-table-var e)
  (assigment-aux e))

; Return the type of the list
(define (type-list? exp)
  (nanopass-case (L12 Expr) exp
                 [(list ,[e*] ... )
                   (let ([type-list (map unparse-L12 e*)])
                         (car type-list))]
                 [(const, t, c) t]))

;; Pass that transforms a list into an array with length, the elements' type, and elements of the list.
(define-pass list-to-array : L12 (ir) -> L13 ()
  (Expr : Expr (ir) -> Expr ()
        [(list ,[e*] ... ) `(array ,(length e*) ,(type-list? ir) ,e* ...)]))

(define (join elems sep)
  (if (equal? (length elems) 1)
      (car elems)
      (string-append (car elems) sep (join (cdr elems) sep))))

(define (for-format e)
  (nanopass-case (L13 Expr) e
                 [(array ,le ,t ,e* ...) (list le t (map unparse-L13 e*))]))

(define (var-format x type expr)
  (nanopass-case
   (L13 Expr) expr
   [(const ,t ,c0) (string-append (c type) " " (symbol->string x) " = " (c expr) ";\n")]
   [(lambda ([,x* ,t*] ...) ,body) (string-append "void " (symbol->string x) "(" (foldl (lambda (a b result)
           (string-append result (symbol->string a) " " (c b) ","))
         ""
         x*
         t*) "){\n" (c body) "\n}") ]
   [(array ,le ,t ,e* ...) (string-append (symbol->string t) " " (symbol->string x) "[" (number->string le) "] = {" (join (map c e*) ",") "}")]
   [else (symbol->string x)]))

;; Pass c
(define (c expr)
  (nanopass-case
    (L13 Expr) expr  
    [,t (match t
          ['Int "int"]
          ['Bool "bool"]
          ['Char "char"]
          [else "Lambda"])]
    [,x (let ([info (hash-ref symbol-hashtable-var (unparse-L13 x) #t)])
          (if (pair? info)
              (begin
                (hash-remove! symbol-hashtable-var (unparse-L13 x))
                (var-format x (car info) (parse-L13 (unparse-L11 (cdr info)))))
              (symbol->string x)))]
    [(const ,t ,c)
     (match t
       ['Int (number->string c)]
       ['Bool (if c "1" "0")]
       ['Char (string c)])]
    [(primapp ,pr ,e* ...)
     (match pr
       ['+ (string-append (c (first e*)) "+" (c (second e*)) ";\n")]
       ['- (string-append (c (first e*)) "-" (c (second e*)) ";\n")]
       ['* (string-append (c (first e*)) "*" (c (second e*)) ";\n")]
       ['/ (string-append (c (first e*)) "/" (c (second e*)) ";\n")]
       ['and (string-append (c (first e*)) "&&" (c (second e*)))]
       ['or (string-append (c (first e*)) "||" (c (second e*)))]
       ['length (let ([e (first e*)])
                  (string-append "sizeof(" (c e) ")/sizeof(" (c (parse-L12 `(car e))) ")\n"))]
       ['car (let ([e (first e*)]) (string-append (c e) "[0];\n"))]
       ['cdr (let ([e (first e*)])
               (string-append "for(i=1; i<" (c (parse-L13 `(primapp length e))) "; i++){
                                                                      \n" (c e) "[i]=" (c e) "[i+1];" "}\n" ))]
       ['equal? (string-append (c (first e*)) "==" (c (second e*)))]
       ['iszero? (string-append (c (first e*)) "== 0")]
       ['++ (string-append (c (first e*)) "++;\n")]
       ['-- (string-append (c (first e*)) "--;\n")]
       ['< (string-append (c (first e*)) "<" (c (second e*)))]
       ['> (string-append (c (first e*)) ">" (c (second e*)))])]
    [(begin ,e* ... ,e) (string-append (foldr string-append "" (map c e*))  (c e) ";\n")]
    [(if ,e0 ,e1 ,e2) (string-append "if(" (c e0) ")\n{" (c e1) "}\nelse\n{" (c e2)  "}\n")]
    [(let ,x ,body) (string-append (c x) (c body))]
    [(letrec ,x ,body) (string-append (c x) (c body))]
    [(letfun ,x ,body) (string-append (c x) (c body))]
    [(while [,e0] ,e1) (string-append "while(" (c e0) ")" "{" (c e1) "}")]
    [(for [,x ,e0] ,e1)
     (let* ([info (for-format e0)]
            [len (car info)]
            [tipo (cadr info)]
            [e* (third info)])
       (begin
         (hash-set! symbol-hashtable-var 'aux (list tipo (unparse-L13 e0)))
         (string-append (c e1) (c (parse-L13 'aux)) "\nfor(int " (symbol->string x) " = 0; " (symbol->string x)
                        " < " (number->string (sub1 len)) "; " (symbol->string x) "++){\n    " (symbol->string (second (unparse-L13 e1)))
                        "(aux[" (symbol->string x) "])\n}\n")))]
    [(define ,x ,e) (string-append "#define" (c x) (c e))]
    [(,e0 ,e1)
     (string-append (c e0) "(" (c e1) ")")]))