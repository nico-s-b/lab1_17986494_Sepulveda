#lang racket

(provide chatbot)
(provide chatbot-add-flow)
(provide chatbots-rem-duplicates)
(require "option.rkt")
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
;Función aplica recursión de cola para recompletar la lista de flows existentes y finalmente
;verifica que el flow que se desea añadir no esté previamente en la nueva lista de flows:
;en caso de no estar, lo añade. Si no, retorna los flujos originales. Recursión de cola
;permite ir reduciendo una lista (de flows por revisar) y de flows añadidos en cada llamado.
(define (chatbot-add-flow chatbot flow)
  (define (add-flows flows flow)
   (cond [(null? flows) flow] ;caso base 1: agregar si no hay opciones
        [(equal? (car flows) flow) flows] ;caso base 2: retornar chatbot sin cambios si flow tiene id repetido existente
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