#lang racket

(provide flow)
(provide flow-add-option)
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
             (string? name))
      (list id name (options-rem-duplicates options))
      (begin (display "Error al crear flow") #f)
    )
  )
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
(define flow-options (lambda (flow) (caddr flow)))

;Modificador
;flow-add-option: añade una opción a un flujo ya existente.
;;;Dominio: flow X option
;;;Recorrido: flow
(define (flow-add-option flow . new-options)
  (let ([option-list (append (flow-options flow) new-options)]) 
        (list (flow-id flow) (flow-name flow) (options-rem-duplicates option-list))
  )
)

;Otras funciones
;options-rem-duplicates: remueve opciones duplicadas de una lista de opciones en base
;al id de la opción, dejando la primera aparición de la opción únicamente
;;;Dominio: options (list)
;;;Recorrido: options (list)
;Utiliza la función remove-duplicates, utilizando como criterio la igualdad de IDs de las opciones.
(define (options-rem-duplicates options)
  (remove-duplicates options
                     (lambda (x y) (equal? (option-code x) (option-code y) ))
  )
)

(define op1 (option 1 "op1" 2 3 "as" "qw"))
(define op2 (option 2 "op2" 2 4))
(define op3 (option 3 "op3" 1 3 "we"))
(define op4 (option 1 "op4" 5 6 "df" "ry"))
(define flow1 (flow 1 "f1" op1 op2))
(define flow2 (flow 2 "f2" op1 op2 op3 op4))
(define flow3 (flow 3 "f3" op1))
(define flow4 (flow-add-option flow3 op2))
(define flow5 (flow-add-option flow4 op4))
(define flow6 (flow 6 "f6" op1 op2 op2 op3 op3))
(define flow7 (flow-add-option flow6 op3))