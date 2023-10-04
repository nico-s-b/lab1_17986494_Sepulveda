#lang racket

(provide chatH)
(provide addtalk)

;TDA chatHistory

(define (chatH username)
  (list username))

(define (addtalk chat cblink flowlink op message)
  (cons (car chat) (append (cdr chat) (list (list cblink flowlink op message)))
  )
)


(define c1 (chatH "user1"))
(define c2 (addtalk c1 0 1 2 "estudiar"))
(define c3 (addtalk c2 0 2 1 "tecnico"))
(define c4 (addtalk c3 1 1 3 "viajar"))