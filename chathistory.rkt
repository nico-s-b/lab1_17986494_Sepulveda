#lang racket

(provide chatH)
(provide chatH-add)
(provide chat-format)

;TDA chatHistory

;---------------------Constructor---------------------
;chatH: crea un chatHistory para un usuario dado un nombre de usuario
;Dominio: username (string)
;Recorrido: chatHistory
(define (chatH username)
  (list username))

;---------------------Modificador---------------------
;chatH-add: añade una entrada al chatHistory de un usuario
;Dominio: chatH X cblink (integer) X flowlink (string) X message (string)
;Recorrido: chatHistory
(define (chatH-add chatH cblink flowlink message)
  (cons (car chatH) (append (cdr chatH) (list (list cblink flowlink message)))
  )
)

;---------------------Otras funciones---------------------
;chat-format: formatea un chatHistory para su visualización
;Dominio: chatH
;Recorrido: string
(define (chat-format chatH)
  (car chatH)
)

(define ch '("user2" '(0 1 "hola") '(1 1 "1") '(1 2 "1") '(1 1 "Museo") '(1 2 "1") '(1 1 "3") '(1 1 "5")))