#lang racket

(provide chatH)
(provide chatH-add)
(provide chat-format)
(provide chat-cb)
(provide chat-fl)
(provide chat-mess)
(provide chat-time)

;TDA chatHistory
;Representación como una lista cuyo primer elemento es el nombre de usuario y el segundo elemento
;es una lista de elementos de chat (chatElement)
;chatHistory = user X null|chatElement

;Representación de un chatElement: lista formada por:
;chatbotCodeLink X flowCodeLink X message X message-time

;---------------------Constructor---------------------
;chatH: crea un chatHistory para un usuario dado un nombre de usuario.
;Dominio: username (string)
;Recorrido: chatHistory
(define (chatH username)
  (list username))

;---------------------Selectores---------------------
;chat-cb: retorna el chatbotcodelink de un elemento del chatHistory
;Dominio: chatElement
;Recorrido: chatbotcodelink (int)
(define chat-cb car)

;chat-fl: retorna el flowcodelink de un elemento del chatHistory
;Dominio: chatElement
;Recorrido: flowcodelink (int)
(define chat-fl cadr)

;chat-mess: retorna el mensaje de un elemento del chatHistory
;Dominio: chatElement
;Recorrido: message (string)
(define chat-mess caddr)

;chat-time: retorna el time de un elemento del chatHistory
;Dominio: chatElement
;Recorrido: time (int)
(define chat-time cadddr)

;---------------------Modificador---------------------
;chatH-add: añade una entrada al chatHistory de un usuario
;Dominio: chatH X cblink (integer) X flowlink (string) X message (string)
;Recorrido: chatHistory
(define (chatH-add chatH cblink flowlink message)
  (cons (car chatH) (append (cdr chatH) (list (list cblink flowlink message (current-seconds) )))
  )
)
