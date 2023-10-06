#lang racket

(provide user)
(provide user-name)
(provide user-chat)
(provide user-insystem)
(provide user-add-talk)
(provide user-chat-format)
(require "chathistory.rkt")

;TDA user

;---------------------Constructor---------------------

;user: función que crea un nuevo usuario  a partir de su nombre
;Dominio: username (string)
;Recorrido: user
(define (user name)
  (list name (chatH name))  ;el historial chathistory se irá guardando en '()
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

;user-insystem: función que busca un usuario dentro de una lista de usuarios
;registrados a partir de su nombre de usuario
;Retorna una lista vacía en caso de que no exista el usuario
;Dominio: username (string) X userlist (list)
;Recorrido: user
(define (user-insystem username userlist)
  (let ([maybeuser (filter (lambda (elem) (equal? username (user-name elem))) userlist)])
    (if (not (null? maybeuser))
        (car maybeuser)
        '())
  )
)

;---------------------Modificadores---------------------
;user-add-talk: agrega una entrada al chatHistory del usuario
;Dominio: user X cblink (int) X flink (int) X mens (int OR string)
;Recorrido: user
(define user-add-talk
  (lambda (usuario cblink flink mens)
    (list (user-name usuario) (chatH-add (user-chat usuario) cblink flink mens))
  )
)

;---------------------Otras funciones---------------------
;user-chat-format
;Dominio: user
;Recorrido: string
(define (user-chat-format user)
  (chat-format (user-chat user))
)
