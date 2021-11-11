#lang nanopass
#|
Compiladores 2022-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Naomi Itzel Reyes Granados
Laboratorio: Nora Hilda Hernández Luna

Lexer
Integrantes de equipo/Autores:
Nombre | No.cuenta| correo
Juan García Lugo, 316161013, juanlugo@ciencias.unam.mx
Nestor Semer Vazquez Cordero, 316041625, nestor2502@ciencias.unam.mx
Angel Christian Pimentel Noriega, 316157995, cristianp@ciencias.unam.mx
|#

;; Bibliotecas chidas para lexear
(require parser-tools/lex
         parser-tools/lex-plt-v200
         (prefix-in : parser-tools/lex-sre);Operadores
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
;Exporta todos los identificadores que están definidos en el  nivel
;de fase relevante dentro del módulo de exportación, y que tienen el mismo contexto léxico
(provide (all-defined-out))

;; Tokens con argumentos
(define-tokens a (NUM  ;Numeros
                  BOOL ;Booleanos
                  VAR  ;Variables
                  ))

(define-empty-tokens b (LP RP LB RB LK RK ;Delimitadores.
                        + - / * ;Operadores aritmeticos.
                        BEGIN ;Bloque de codigo
                        AND OR ;Operadores booleanos.
                        IF THEN ELSE ;Sentencia if then else
                        FUN FUNF ARROW ;Sentencia de funciones
                        APPT ;Multiples argumentos
                        LET ASSIGN IN END ;Sentencia let 
                        APP ;Aplicación de funciones
                        INT BOOLE TYPEOF ; Tipos
                        EOF ;Token final
                        ))

; sre : S-regular expressions
(define minHS-lexer
           (lexer
             [(:: #\] #\[)
              ; =>
              (token-APPT)]

             ["+"
              ; =>
              (token-+)]
             
             ["and"
              ; =>
              (token-AND)]

             ["/"
              ; =>
              (token-/)]
             
             ["or"
              ; =>
              (token-OR)]
             
             ["-"
              ; =>
              (token--)]

             ["*"
              ; =>
              (token-*)]
             
             [(:: #\# (:or #\t #\f))
              ; =>
              (token-BOOL (equal? lexeme "#t"))]
             
             ["=>"
              ; =>
              (token-ARROW)]
             
             [#\:
              ; =>
              (token-TYPEOF)]
             
             [#\=
              ; =>
              (token-ASSIGN)]
             
             ["app"
              ; =>
              (token-APP)]
             
             ["in"
              ; =>
              (token-IN)]
             
             ["end"
              ; =>
              (token-END)]
             
             ["let"
              ; =>
              (token-LET)]
              
             ["funF"
              ; =>
              (token-FUNF)]
             
             ["fun"
              ; =>
              (token-FUN)]
             
             ["if"
              ; =>
              (token-IF)]

             ["then"
              ; =>
              (token-THEN)]

             ["else"
              ; =>
              (token-ELSE)]
             
             [(:: (char-range #\a #\z)
                  (:*(:or (char-range #\a #\z) (char-range #\0 #\9)))) ;[a-z]([a-z] + [0-9])^*
              ; =>
              (token-VAR (string->symbol lexeme))]

             [(:: (:? #\-) (:+ (char-range #\0 #\9)))
              ; =>
              (token-NUM (string->number lexeme))]

             ["Int"
              ; =>
              (token-INT)]

             ["Bool"
              ; =>
              (token-BOOLE)]

             ["Begin"
              ; =>
              (token-BEGIN)]

             [#\(
              ; =>
              (token-LP)]

             [#\)
              ; =>
              (token-RP)]

             [#\[
              ; =>
              (token-LB)]

             [#\]
              ; =>
              (token-RB)]
             
             [#\{
              ; =>
              (token-LK)]

             [#\}
              ; =>
              (token-RK)]
             
             [whitespace
              ; =>
              (minHS-lexer input-port)]

             [(eof)
              (token-EOF)]))


