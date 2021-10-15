#lang nanopass

;; Bibliotecas chidas para lexear
(require parser-tools/lex
         parser-tools/lex-plt-v200
         (prefix-in : parser-tools/lex-sre);Operadores
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(provide (all-defined-out));Exporta todos los identificadores que están definidos en el  nivel
;de fase relevante dentro del módulo de exportación, y que tienen el mismo contexto léxico

(define-tokens a (NUM VAR))
(define-empty-tokens b (LP RP + - EOF))

; sre : S-regular expressions
(define calc-lexer
           (lexer
             [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) ; (a-z | A-Z)^+
              ; =>
              (token-VAR (string->symbol lexeme))]

             [(::  (:or #\- (epsilon)) (:: (:* (char-range #\0 #\9)) (:: (:or (:: #\. (char-range #\0 #\9)) (:: (char-range #\0 #\9)) #\.) (:* (char-range #\0 #\9)))))
              ; =>
              (token-NUM (string->number lexeme))]

             [#\+
              ; =>
              (token-+)]

             [#\-
              ; =>
              (token--)]

             [whitespace
              ; =>
              (calc-lexer input-port)]

             [(eof)
              (token-EOF)]))

(define-struct arith-exp (op e1 e2) #:transparent)
(define-struct num-exp (n) #:transparent)
(define-struct var-exp (i) #:transparent)

