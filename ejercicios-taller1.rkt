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

;; Ejercicio 6 INCOMPLETO **************
;; generic-filter
;; Proposito: 
;; Procedimiento que recibe un predicado (pred) y una lista lst
;; y retorna una lista con los elementos que satisfacen el predicado

;(define generic-filter
;  (lambda (pred lst)
;    (if (null? lst)
;        empty
;        #t ;LOGICA DEL FILTRO GENERICO
;        )
;    )
;  )
;
;;Pruebas
;(generic-filter null? '((1 2) () (((4 5) ()))))
;(generic-filter even? '((1 2) 3 (((4 5) 6))))

;; Ejercicio 7
;; filter-acum
;; Proposito: 
;; Procedimiento que recibe cinco parametros, dos numeros a y b una funcion binaria F, un valor inicial acum y una funcion unaria filter.
;; y retorna todos los elemento que estan en el intervalo [a,b] y que a su vez cumplen con el predicado de la funcion filter
;; el resultado debe ir consevando en acum y finalmente retornal el ultimo valor obtetenido en acum

(define filter-acum
  (lambda(a b F acum filter)
    (cond
      [(<= a b)(cond
                 [(filter a) (+ acum (filter-acum (+ a 1) b F a filter))]
                 [else (filter-acum (+ a 1) b F acum filter)])]
      [else acum])
    )
  )
;pruebas:
(filter-acum 1 5 + 0 odd?)
(filter-acum 1 7 + 0 even?)
(filter-acum 1 9 + 0 even?)

;; Ejercicio 8

;; Ejercicio 9

;; Ejercicio 10

;; Ejercicio 11
;; operate
;; Proposito: 
;; Procedimiento que recibe dos parametros irators (lista de funciones binarias de tamaño n) e irands (lista de numeros de tamaño n+1)
;; y retorna el resultado al aplicar sucesivamente las operaciones entre Irators a los valores Irands.
(define operate (lambda (lrators lrands)
                  (letrec ; zona declaracion
                      ([invert-Lst (lambda (L) (if (null? L) '() (append (invert-Lst (cdr L)) (list (car L)))))]; Invierte lista
                       [link (lambda (L1 L2) (append L1 L2))]; Une dos listas
                       [tail (lambda (L) (if (null? (cdr L)) (car L) (tail (cdr L))))]; Retorna el ultimo elemento de una lista
                       [body (lambda (L) (if (null? (cdr L)) '() (cons (car L) (body (cdr L)))))] ;Elimina el ultimo elemento de una lista
                                                                                                  
                       [execute (lambda (L) (if (null? (cdr L)) (car L) 
                                                ((car L) (execute (cdr (body L))) (tail L))
                                                ))] ; Hace la operacion respectiva segun la lista de operadores tomando: un operador,
                       ; el llamado recursivo a la funcion
                       ; y la cola de la lista. Construyendo asi la expresion a ejecutar.
                       )
                     ; zona ejecucion
                    (execute (link (invert-Lst lrators) lrands)))))

(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list *) '(4 5))

;; Ejercicio 12

;; Ejercicio 13

;; Ejercicio 14
;; carCdr
;; Proposito: 
;; Procedimiento que recibe dos parametros irators (lista de funciones binarias de tamaño n) e irands (lista de numeros de tamaño n+1)
;; y retorna el resultado al aplicar sucesivamente las operaciones entre Irators a los valores Irands.
;(define carCdr
;  (lambda (simbolo lista error)
;    (cond
;      [(eqv? lista '()) error]
;      [(list? (car lista)) (if (eqv? (carCdr simbolo (car lista) error) simbolo ) (car lista) (carCdr simbolo (cdr lista) error)) (carCdr simbolo (cdr lista) error ) ]
;      [(eqv? simbolo (car lista)) (car lista)]
;      [else (carCdr simbolo (cdr lista) error)]
;      )
;   )
;  )
;
;(carCdr 'a '(a b c) 'fail)
;(carCdr 'c '(a b c) 'fail)
;(carCdr 'a '() 'fail)