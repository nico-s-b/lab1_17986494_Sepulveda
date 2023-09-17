#lang racket

(provide flow)
;(provide flow-add-option)
(require "option.rkt")

;TDA flow

;Constructor
;flow: crea un flujo para un chatbot
;;;Dominio:
;id (int) número identificador del flujo
;name (string) nombre dado al flujo
;options (cantidad variable) opciones asociadas al flujo
;;;Recorrido: flow
(define flow
  (lambda (id name . options)
    (if (and (integer? id)
           (string? name)
           (options-verify-id options))
      (list id name options)
      (begin (display "Error al crear flujo") #f)
   ))
)

;Selectores
;flow-id
;Dominio: flow / Recorrido: id (int)
(define flow-id car)

;flow-name
;Dominio: flow / Recorrido: name (string)
(define flow-name (lambda (flow) (car (cdr flow))))

;flow-options
;Dominio: flow / Recorrido: lista de opciones (list)
(define flow-options (lambda (flow) (cddr flow)))

;Modificador
;flow-add-option: añade una opción a un flujo ya existente.
;;;Dominio: flow X option
;;;Recorrido: flow
(define (flow-add-option flow . new-options)
  (if (and (integer? (flow-id flow))
           (string? (flow-name flow))
           (options-verify-id (append (flow-options flow) new-options)))
      (list (flow-id flow) (flow-name flow) (append (flow-options flow) new-options))
      (begin (display "Error al añadir opcion al flujo") #f)
   )
)

;Función que cuenta la cantidad de veces que está "elem" en "lista"
;;;Dominio: elem (any/c) X lista (list)
;;;Recorrido: int
;Utiliza currificación para que equal? tome como uno de sus argumentos el elemento
;que se desea contar, y count pueda usarlo como procedimiento unario para contar
;las apariciones de elem en la lista.
(define ocurrencia
  (lambda (elem lista)
    (count (curry equal? elem) lista)))

;Función que, a partir de una lista, genera otra lista que contiene la cantidad
;de veces que aparece cada elemento de la lista original. 
;;;Dominio: list
;;;Recorrido: list
;Utiliza la función "ocurrencia" definida anteriormente, aplicándola con map a
;cada elmento de la lista. OBSERVACIÓN: La lista generada tiene la misma cantidad
;de elementos que la lista original, contando de forma duplicada los elementos.
(define (contador lista)
  (map (lambda (elem) (ocurrencia elem lista)) lista))

;Función que verifica que no hayan opciones con un id u option-code repetido.
;;;Dominio: lista de datos tipo "opcion"
;;;Recorrido: boolean
;Utiliza la función "contador", definida anteriormente, sobre los id de una lista de
;opciones. Utiliza filter para generar una lista que no tenga ningún "1". Si tras
;el filtrado la lista no queda vacía, significa que hay alguna opción cuya id está
;repetida. En tal caso retorna #f. Si la lista queda vacía, retorna #t.
(define (options-verify-id options)
  (null? (filter
          (lambda (x) (not (= x 1)) )
          (contador (map option-code options)))
  )
)

(define op1 (option 1 "o" 2 3 "asdf" "qwerty"))
(define op2 (option 2 "o" 2 4))
(define op3 (option 3 "o" 1 3 "qwerty"))
(define op4 (option 1 "o" 5 6 "df" "rty"))
(define flow1 (flow 1 "f1" op1 op2))
(define flow3 (flow 3 "f3" op1))
(define flow4 (flow-add-option flow3 op2))