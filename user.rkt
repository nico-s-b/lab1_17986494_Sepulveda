#lang racket

(provide user)
(provide user-insystem)

(require "option.rkt")
(require "flow.rkt")
(require "chatbot.rkt")

;TDA user

;---------------------Constructor---------------------

;user: función que crea un nuevo usuario  a partir de su nombre
;Dominio: username (string)
;Recorrido: user
(define (user name)
  (list name '())  ;el historial chathistory se irá guardando en '()
)

;---------------------Selector---------------------
;user-name: función que retorna el nombre de usuario de un usuario
;Dominio: user
;Recorrido: name (string)
(define user-name car)

;user-chat: función que retorna el historial de chat de un usuario
;Dominio: user
;Recorrido: chatHistory
(define user-chat cadr)

;user-insystem: función que identifica busca un usuario dentro de una lista de usuarios registrados a partir de su nombre de usuario
;retorna una lista vacía en caso de que no exista el usuario
;Dominio: username (string) X userlist (list)
;Recorrido: user
(define (user-insystem username userlist)
  (let ([maybeuser (filter (lambda (elem) (equal? username (user-name elem))) userlist)])
    (if (not (null? maybeuser))
        (car maybeuser)
        '())
  )
)
