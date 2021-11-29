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

(define list-facts
    (lambda (num)
      (letrec
          (; zona declaracion
           (facto
            (lambda (num-facto calculo)
               (if (eqv? num-facto num)
                   (cons calculo empty)
                   (cons calculo (facto (+ num-facto 1) (* (+ num-facto 1) calculo)))
                )
               )
             )
           )
          ; zona ejecucion, solucion bottom-up
          ; num-facto: número base a calcular el factorial
          ; calculo: factorial ya calculo del num-facto
          ;(facto num-facto calculo)
           (facto 1 1)
          )
      )
  )


(list-facts 5)
(list-facts 8)


;; Ejercicio 4
;; every?
;; Proposito:
;; Procedimiento que recibe un predicado pred y una lista lst.
;; retorna #f si algún elemento de lst falla al satisfacer pred, de lo contrario retorna #t
;; el predicado puede ser un number? o un symbol? 

(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f)
      )
    )
  )

;; Pruebas
(every? number? '(a b c 3 e))
(every? number? '(1 2 3 4 5))
(every? symbol? '(f g 6 7 8)) 
(every? symbol? '(f g h i j k))


;; Ejercicio 5
;; list-set
;; Proposito: 
;; Procedimiento que recibe una lista, un indice y un elemento
;; y retorna una lista similar a la original, excepto que el index-simo elemento
;; (indexado desde cero) sera reemplazado por el elemento dado en la entrada

(define list-set
  (lambda (lst index elem)
    (if (null? lst)
        empty
        (if (zero? index)
            (cons elem (cdr lst));nuevo valor en la posición index-sima
            (cons (car lst) (list-set (cdr lst) (- index 1) elem)) 
        )
     )
  )
)

;Pruebas
(list-set '(a b c d) 3 '(1 5 10)) 
(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 3 '(1 5 10))


