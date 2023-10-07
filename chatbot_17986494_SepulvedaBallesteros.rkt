#lang racket

(provide chatbot)
(provide chatbot?)
(provide chatbot-name)
(provide chatbot-add-flow)
(provide chatbot-welcome)
(provide chatbot-flowid)
(provide chatbots-rem-duplicates)
(provide chatbot-flows)
(provide chatbot-id)
(provide chatbot-talk-flow)
(require "flow_17986494_SepulvedaBallesteros.rkt")

;TDA chatbot

;Representación: lista formada por
; chatbotID X name X welcomeMessage X initialFlowCode X *flows

;---------------------Constructor---------------------

;chatbot: función que construye un chatbot
;;;Dominio: chatbotID (int) X name (string) X welcomeMessage (string) X initialFlowCode (int) X *flows
;;;Recorrido: chatbot
(define chatbot
  (lambda (chatbotID name welcomeMessage startFlowId . flows)
    (if (and (integer? chatbotID)
             (string? name)
             (string? welcomeMessage)
             (integer? startFlowId)
             (or (null? flows) (andmap flow? flows)))
        (list chatbotID name welcomeMessage startFlowId (flows-rem-duplicates flows))
        (raise "Error al crear chatbot")
    )
  )
)

;---------------------Pertenencia---------------------

;chatbot?: comprueba si un listado de elementos corresponde a un chatbot
;Dominio: chatbot / Recorrido: boolean
(define (chatbot? args)
  (if (and (>= (length args) 3)
           (integer? (chatbot-id args))
           (string? (chatbot-name args))
           (integer? (chatbot-flowid args))
           (or (null? (chatbot-flows args)) (andmap flow? (chatbot-flows args))))
      #t
      #f)
)

;---------------------Selectores---------------------

;chatbot-id: selecciona el código id del chatbot
;Dominio: chatbot
;Recorrido: id (int)
(define chatbot-id car)

;chatbot-name: selecciona el nombre del chatbot
;Dominio: chatbot
;Recorrido: name (string)
(define chatbot-name cadr)

;chatbot-welcome: selecciona mensaje de bienvenida del chatbot
;Dominio: chatbot
;Recorrido: welcomeMessage (string)
(define chatbot-welcome caddr)

;chatbot-flowid: selecciona el identificador de flujo inicial startFlowId
;Dominio: chatbot
;Recorrido: startFlowId (int)
(define chatbot-flowid cadddr)

;chatbot-flows: selecciona el listado de flujos del chatbot
;Dominio: chatbot
;Recorrido: flows (lista)
(define chatbot-flows
  (lambda (cbot) (car (cdr (cdr (cdr (cdr cbot))))))
)

;chatbot-talk-flow: función que selecciona el flow activo del chatbot de acuerdo al flow-id actual
;Dominio: chatbot
;Recorrido: flow
(define (chatbot-talk-flow cbot)
  (car (filter (lambda (fl) (= (chatbot-flowid cbot) (flow-id fl))) (chatbot-flows cbot)))
)

;---------------------Modificadores---------------------

;chatbot-add-flow
;;;Dominio: chatbot X flow
;;;Recorrido: chatbot
;Recursión: natural
;Función aplica recursión natural para verificar la no repitencia del id de flow en la lista de
;flows existentes y lo añade si no está previamente. En caso contrario, devuelve los flows originales.
;Recursión natural se propone por una sintaxis "natural" para el procedimiento, a partir
;de la idea de verificar elemento a elemento de la lista si hay duplicación de id, continuando
;con el resto de la lista en cada llamado recursivo, además de construir la lista incluyendo
;el resultado recursivo con el llamado recursivo dentro de la operación cons / list
(define (chatbot-add-flow chatbot flow)
  (define (add-flows-aux flows flow)
   (cond
     ;caso base 1: agregar si no hay flujos
     [(null? flows) (list flow)]
     ;caso base 2: retornar chatbot sin cambios si flow tiene id repetido existente
     [(equal? (flow-id (car flows)) (flow-id flow)) flows]
     ;llamada recursiva: se mantiene el primer elemento y se continúa la recursión con el resto
     [else (cons (car flows) (add-flows-aux (cdr flows) flow))]) 
   )
  (list (chatbot-id chatbot) (chatbot-name chatbot) (chatbot-welcome chatbot)
        (chatbot-flowid chatbot) (add-flows-aux (chatbot-flows chatbot) flow))
)

;chatbot-change-flow: cambia el flow actual del chatbot dado un nuevo flow-id
;Dominio: chatbot X flow-id (int)
;Recorrido: chatbot
(define (chatbot-change-flow cbot new-fl-id)
  (list (chatbot-id cbot) (chatbot-name cbot) (chatbot-welcome cbot)
        new-fl-id (chatbot-flows cbot))
)

;---------------------Otras funciones---------------------

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