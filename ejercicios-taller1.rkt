#lang eopl

;; Ejercicio 1
;; count-ocurrences :
;; Proposito:
;; Procedimiento que recibe un elemento (elem) y una lista (lst)
;; y retorna la cantidad de veces que aparece (elem) en (lst)

(define count-occurrences
  (lambda (elem lst)
    (if (null? lst) 0
      (if (eqv? elem (car lst)) (+ 1 (count-occurrences elem (cdr lst)))
          (count-occurrences elem (cdr lst))
          )
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

;; Pruebas
(list-set '(a b c d) 3 '(1 5 10)) 
(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 3 '(1 5 10))

;; Ejercicio 6
;; Entrada: un predicado y una lista
;; Salida: lista similar a lst pero solo con los elementos que satisfacen el predicado pred y debe
;; eliminar los que no lo cumplen
(define generic-filter
  (lambda (pred lst)
    (cond [(null? lst) empty]
          [(and (not (list? (car lst)))(eq? (pred (car lst)) #t)) (cons (car lst) (generic-filter pred (cdr lst)))]
          [(list? (car lst)) (cons (generic-filter pred (car lst))(generic-filter pred (cdr lst)))]
          [else (generic-filter pred (cdr lst))]
    )
  )
 )

;; Pruebas:
(generic-filter even? '((1 2) 3 (((4 5) 6))))
(generic-filter odd? '((1 2) 3 5 (8 7) (((4 5) 6))))
(generic-filter positive? '(1 5 6 -5 (-2 5)))
(generic-filter negative? '(1 5 6 -5 (-2 5)))

;; Ejercicio 7
;; filter-acum
;; Proposito: 
;; Procedimiento que recibe cinco parametros, dos numeros a y b una funcion binaria F, un valor inicial acum y una funcion unaria filter.
;; y retorna todos los elemento que estan en el intervalo [a,b] y que a su vez cumplen con el predicado de la funcion filter
;; el resultado debe ir consevando en acum y finalmente retornal el ultimo valor obtetenido en acum

(define filter-acum
  (lambda(a b F acum filter)
    (if (and (eqv? F *) (eqv? acum 0))
        (cond
          [(<= a b)(cond
                     [(filter a) (F 1 (filter-acum (+ a 1) b F a filter))]
                     [else (filter-acum (+ a 1) b F 1 filter)])]
          [else acum])
        
        (cond
          [(<= a b)(cond
                     [(filter a) (F acum (filter-acum (+ a 1) b F a filter))]
                     [else (filter-acum (+ a 1) b F acum filter)])]
          [else acum])
        )
    )
  )

;; Pruebas
(filter-acum 1 10 + 0 odd?)
(filter-acum 1 10 + 0 even?)
(filter-acum 1 5 * 0 odd?)
(filter-acum 1 5 + 0 odd?)
(filter-acum 1 5 * 0 even?)

;; Ejercicio 8
;; mapping
;; Propósito:
;; Entrada: 3 argumentos
;; 1. Una función F
;; 2. Una lista 1 L1
;; 3. Una lista 2 L2
;; Salida: Una lista de pares (a,b) donde se cumple que F(a) = b

(define mapping
  (lambda (funcion lista1 lista2)
    (cond [(or (null? lista1) (null? lista2)) empty]
          [(= (car lista2) (funcion (car lista1)))
           (cons (list (car lista1) (car lista2)) (mapping funcion (cdr lista1) (cdr lista2)))]
          [else (mapping funcion (cdr lista1) (cdr lista2))]
          )
    )
  )

;; Pruebas:
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))

;; Ejercicio 9
;; prod-scalar-matrix:
;; Proposito:
;; procedimiento que recibe una matriz (mat) y un vector (vec)
;; y retorna una lista con el resultado de realizar el producto escalar

(define aux
  (lambda (mat vec)
    (cond [(or (null? mat) (null? vec)) empty]
          [else (cons (*(car mat) (car vec)) 
          (aux (cdr mat) (cdr vec )))]
          )
    )
  )

(define prod-scalar-matriz
  (lambda (mat vec)
    (cond [(or (null? mat) (null? vec)) empty]
          [else(append (list(aux(car mat) vec))
          (prod-scalar-matriz (cdr mat) vec ))]
          )
    )
  )

;Pruebas:
(prod-scalar-matriz '((1 1 2 4) (2 2 5 4) (2 2 5 4)) '(2 3 1 2))
;;((2 3 2 8) (4 6 5 8) (4 6 5 8))
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
;;((2 3) (4 6))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))
;;((2 3) (4 6) (6 9))

;; Ejercicio 10
;; up
;; Proposito:
;; procedimiento que recibe como argumento una lista lst
;; retorna una lista similar a lst donde se hayan removido un par
;; de paréntesis de cada elemento del nivel más alto de lst. 

(define up
  (lambda (lst)
    (cond [(null? lst) empty]
          [else (if (list? (car lst))
                    (append (car lst) (up (cdr lst)))
                    (cons (car lst) (up (cdr lst)))
                    )])
    )
  )

;; Pruebas

(up '((1 2) (3 4)))
(up '((x (y)) z))
(up '((x (y)) (3 (u) 7)))

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
;; Entrada:
;; F: función
;; a: término para evaluación del inicio de la integral
;; b: término para evaluación del final de la integral
;; n: subintervalos
;; Salida: la aproximación de la integral utilizando el método simpson

;;  Función auxiliar de sumatoria
(define (sum term a next b)
  (cond [(> a b) 0]
        [else (+ (term a)(sum term (next a) next b))]
        )
  )

(define simpson-rule
  (lambda (F a b n)
    (define h (/ (- b a) n))
    (define (acc x) (+ x 1))
    (define (yk k) (F (+ a (* h k))))
    (define (simpson-term k)
      (* (cond [(or (= k 0) (= k n)) 1]
               [(odd? k) 4]
               [else 2])
         (yk k)))
    (* (/ h 3) (sum simpson-term 0 acc n))
    )
  )

;; Pruebas
(simpson-rule (lambda (x) (* x (* x x))) 1 5 8)
(simpson-rule (lambda (x) x) 1 5 12)

;; Ejercicio 13
;; compose
;; Proposito:
;; procedimiento que recibe dos argumentos proc1 y proc2 que son procedimientos
;; de un argumento, y un valor val. Retorna la composicion de ambos procedimientos
;; aplicados sobre val

(define compose
  (lambda (proc1 proc2)
    (lambda (val)
      (cond [(and (list? proc2) (eqv? (car proc2) compose)) ((proc2) val) ]
            [else (proc1 (proc2 val))])
      )
    )
  )

;; Prueba
((compose car cdr) '(a b c d))
((compose symbol? (compose car cdr)) '(a b c d))
((compose boolean? even?) 5)
((compose list? cdr) '(+ 2 6))
((compose number? (compose car cdr)) '(+ 2 6))

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

;; Ejercicio 15
;; Entrada: un número N
;; Salida: N-esima fila del triangulo de Pascal
;;(1)         Fila 1
;;(1 1)       Fila 2
;;(1 2 1)     Fila 3
;;(1 3 3 1)   Fila 4
;;(1 4 6 4 1) Fila 5

;; Función del pascal
(define pascal
  (lambda (n)
    (cond [(= n 1) '(1)]
          [else (pascal-sig-fila (pascal (- n 1)))]
          )
    )
  )

;; Función auxiliar para saber la lista del pascal N+1
(define (pascal-sig-fila lst)
   (append '(1)
           (suma-dos lst)
           '(1)))

;; Función auxiliar para sumar de a dos pares
(define (suma-dos lst)
   (if (null? (cdr lst))
      '()
      (cons (+ (car lst) (car (cdr lst)))
            (suma-dos (cdr lst)))))

;; Pruebas
(pascal 1)
(pascal 2)
(pascal 3)
(pascal 4)
(pascal 5)