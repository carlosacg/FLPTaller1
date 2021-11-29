#lang eopl

;; Ejercicio 1
;; count-ocurrences :
;; Proposito:
;; Procedimiento que recibe un elemento (elem) y una lista (lst)
;; y retorna la cantidad de veces que aparece (elem) en (lst)

(define (count-occurrences elem lst)
  (if (null? lst) 0
      (if (eqv? elem (car lst)) (+ 1 (count-occurrences elem (cdr lst)))
      (count-occurrences elem (cdr lst))
      )
   )
)
;; Pruebas
(count-occurrences 'a '(a b c a))
(count-occurrences 'a '(a b c h i))
(count-occurrences 'a '(a b c a c a f a))
(count-occurrences 'x '((f x) y (((x z) x))))
;; Ejercicio 2
;; list-tails :
;; Proposito:
;; procedimiento que recibe una lista (lst)
;; y retorna una lista con todas las sublistas de los elementos consecutivos
(define list-tails
  (lambda (L)
    (if (null? L) '() (cons L (list-tails (cdr L)))))
  )

;Pruebas:
(list-tails '(4 5 8 ((1) 2) 8 (7 8 9 (2 4))))
(list-tails '(1 2 3 4 5))
(list-tails '(1 a (e 4) 5 v))


;; Ejercicio 3
;; list-facts :
;; Proposito:
;; Procedimiento que recibe un numero (num)
;; y retorna una lista con incremental de factoriales

(define (factorial num)
  (if (= num 0) 1
      (if (= num 1) 1
          (* num (factorial (- num 1)))
          )
   )
)
;(define (list-facts num))
;; Pruebas
;(list-facts 5)
;(list-facts 4)

