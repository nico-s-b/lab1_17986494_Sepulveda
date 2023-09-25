#lang racket

(provide chatbot)
(provide chatbot-add-flow)
(provide chatbots-rem-duplicates)
(require "flow.rkt")

;Constructor
;chatbot: función que construye un chatbot
;;;Dominio: chatbotID (int) X name (string) X welcomeMessage (string) X initialFlowCode (int) X *flows
;;;Recorrido: chatbot
(define chatbot
  (lambda (chatbotID name welcomeMessage . flows)
    (if (and (integer? chatbotID)
             (string? name)
             (string? welcomeMessage)
             (or (null? flows) (andmap flow? flows)))
        (list chatbotID name welcomeMessage (flows-rem-duplicates flows))
        (raise "Error al crear chatbot")
    )
  )
)

;Selectores
;chatbot-id
;Dominio: chatbot / Recorrido: id (int)
(define chatbot-id car)

;chatbot-name
;Dominio: chatbot / Recorrido: name (string)
(define chatbot-name cadr)

;chatbot-welcome
;Dominio: chatbot / Recorrido: welcomeMessage (string)
(define chatbot-welcome caddr)

;chatbot-flows
;Dominio: chatbot / Recorrido: flows (lista)
(define chatbot-flows cadddr)

;Modificador
;chatbot-add-flow
;;;Dominio: chatbot X flow
;;;Recorrido: chatbot
;Recursión: de cola
;Función aplica recursión natural para verificar la no repitencia del id de flow en la lista de flows
;existentes y lo añade si no está previamente. En caso contrario, devuelve los flows originales.
;Recursión de cola se propone por una sintaxis "natural" para el procedimiento, a partir de la idea de
;verificar elemento a elemento de la lista si hay duplicación de id, continuando con el resto de la lista en cada llamado recursivo.
(define (chatbot-add-flow chatbot flow)
  (define (add-flows flows flow)
   (cond [(null? flows) flow] ;caso base 1: agregar si no hay opciones
        [(equal? (flow-id (car flows)) (flow-id flow)) flows] ;caso base 2: retornar chatbot sin cambios si flow tiene id repetido existente
        [else (cons (car flows) (add-flows (cdr flows) flow))]) ;llamada recursiva: se mantiene el primer elemento y se continúa la recursión con el resto
   )
  (list (chatbot-id chatbot) (chatbot-name chatbot) (chatbot-welcome chatbot)
        (add-flows (chatbot-flows chatbot) flow))
)

;Otras funciones
;chatbots-rem-duplicates: remueve chatbots duplicados de una lista de chatbot en base
;al id de este, dejando la primera aparición del chatbot únicamente
;;;Dominio: chatbots (list)
;;;Recorrido: chatbots (list)
;Utiliza la función remove-duplicates, utilizando como criterio la igualdad de IDs de los chatbots.
;*Misma lógica que función options-rem-duplicates*
(define (chatbots-rem-duplicates chatbots)
  (remove-duplicates chatbots
                     (lambda (x y) (equal? (chatbot-id x) (chatbot-id y) ))
  )
)

;(define op1 (option 1 "op1" 2 3 "as" "qw"))
;(define op2 (option 2 "op2" 2 4))
;(define op3 (option 3 "op3" 1 3 "we"))
;(define op4 (option 1 "op4" 5 6 "df" "ry"))
;(define flow1 (flow 1 "f1" op1 op2))
;(define flow2 (flow 2 "f2" op1 op2 op3 op4))
;(define flow3 (flow 3 "f3" op1))
;(define flow4 (flow-add-option flow3 op2))
;(define flow5 (flow-add-option flow4 op4))
;(define flow6 (flow 6 "f6" op1 op2 op2 op3 op3))
;(define flow7 (flow-add-option flow6 op3))
;(define cb2 (chatbot 2 "cb2" "Hola" flow1))
;(chatbot-add-flow cb2 flow3)