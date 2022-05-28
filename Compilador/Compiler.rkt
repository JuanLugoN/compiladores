#lang nanopass
(require racket/pretty "./src/Language.rkt")
(require racket/pretty "./src/Front-end.rkt")
(require racket/pretty "./src/Middle-end.rkt")
(require racket/pretty "./src/Back-end.rkt")
#|
Compiladores 2022-1

Integrantes de equipo/Autores:
Nombre | No.cuenta| correo
Juan García Lugo, 316161013, juanlugo@ciencias.unam.mx
Nestor Semer Vazquez Cordero, 316041625, nestor2502@ciencias.unam.mx
Angel Christian Pimentel Noriega, 316157995, cristianp@ciencias.unam.mx
|#

;; Path of the file to compile
(define path "examples/example3.mt")

;; Read the specified file
(define read-file
  (read (open-input-file path)))

;; Create and write to a file what you got from applying the processes
;; corresponding to a compilation stage
(define (write-file exp path)
  (with-output-to-file path
   (lambda () (printf "~a" exp))
   #:mode 'text #:exists 'replace))

;; Parse the file content
(define exp-lf (parse-LF read-file))

;; Return the file name without extention
(define file-name (substring path 0 (- (string-length path) 3)))

;; Compile the specified file
(define (compiler exp)
  (write-file (pretty-format (processes-Front-end exp)) (string-append file-name".fe"))
  (write-file (pretty-format (processes-Middle-end exp)) (string-append file-name ".me"))
  (write-file (pretty-format (processes-Back-end exp)) (string-append file-name ".c"))
  )

;; Applies the processes of the Front-end stage
(define (processes-Front-end exp)
  (curry
   (verify-vars
    (verify-arity
     (un-anonymous
      (identify-assigments
       (curry-let
        (remove-string
         (remove-one-armed-if exp)))))))))

;; Applies the processes of the Middle-end stage
(define (processes-Middle-end exp)
  (uncurry
   (type-infer
    (type-const
     (processes-Front-end exp)))))

;; Applies the processes of the Back-end stage
(define (processes-Back-end exp)
  (c
   (list-to-array
    (assigment
     (processes-Middle-end exp)))))

;; run compiler
(compiler exp-lf)
(println "Entrada:")
exp-lf
(println "Front-end:")
(processes-Front-end exp-lf)
(println "Middle-end:")
(processes-Middle-end exp-lf)
(println "Back-end:")
(processes-Back-end exp-lf)