#lang racket

(provide system)

(require "flow.rkt")
(require "chatbot.rkt")

;TDA system

;Constructor
;system: función que crea un nuevo sistema de chatbots (system)
;;;Dominio: name (string) X initialChatbotCodeLink (int) X *chatbots
;;;Recorrido: system
(define (system name initialChatbotCodeLink . chatbots)
  (if (and (string? name)
           (integer? initialChatbotCodeLink)
           (or (null? chatbots) (andmap chatbot? chatbots)))
      (list name initialChatbotCodeLink (chatbots-rem-duplicates chatbots))
      (raise "Error al crear system")
   )
)

;Pertenencia
;system?
;Dominio: system / Recorrido: boolean
(define (system? system)
  (if (and (>= (lenght system) 2)
           (string? (system-name system))
           (intenger? (system-cblink system))
           (or (null? (system-chatbots system)) (andmap chatbot? (system-chatbots system))))
      #t
      #f)
)

;Selector
;system-name
;Dominio: system / Recorrido: name (string)
(define system-name car)

;system-cblink
;Dominio: system / Recorrido: initialChatbotCodeLink (int)
(define system-cblink cadr)

;system-chatbots
;Dominio: system / Recorrido: lista de chatbots (list)
(define system-name caddr)

;system-users
;Dominio: system / Recorrido: lista de users (list)
(define system-users cadddr)

;Modificador
;system-add-flow: Función que añade chatbots a un sistema existente
;;;Dominio: system X chatbot
;;;Recorrido: system
(define (system-add-chatbot system . chatbots)
  (if (and (system? system)
           (andmap chatbot? chatbots))
      (list (system-name system)
            (system-cblink system)
            (chatbots-rem-duplicates (list (system-chatbots system) chatbots)))
      (raise "No se pudo añadir chatbot")
  )
)

;system-add-user: Añade un usuario al sistema
;;;Dominio: system X user (string)
;;;Recorrido: system
(define (system-add-user system user)
  (if (or (null? (system-users system)) (not (member user (system-users system))))
      (list system (list (system-users system) user))
      (if (system? system) system (raise "No se pudo realizar la operación")))
)

;Otras funciones
;system-login: permite a un usuario iniciar sesión en el sistema
;;;Dominio: system X user (string)
;;;Recorrido: system
(define (system-login system user)
  (if (and (system? system) (string? user) (not (member 1 system)))
      (if (member user (system-users system))   ;si (member 1 system) = #t hay una sesión iniciada por user
          (list (system-name system) (system-cblink system) (system-chatbots system) user 1)
          (raise (string-append "Usuario " user " no está registrado en el sistema")))
      (raise "No se pudo realizar la operación"))
)