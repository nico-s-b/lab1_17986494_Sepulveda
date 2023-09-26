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

