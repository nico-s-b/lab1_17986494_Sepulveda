#lang racket

(provide chatH)
(provide chatH-add)

;TDA chatHistory

(define (chatH username)
  (list username))

(define (chatH-add chatH cblink flowlink message)
  (cons (car chatH) (append (cdr chatH) (list (list cblink flowlink message)))
  )
)

(define (chat-format chatH)
  chatH
)

(define c1 (chatH "user1"))
(define c2 (chatH-add c1 0 1 5))
(define c3 (chatH-add c2 0 2 "tecnico"))
(define c4 (chatH-add c3 1 1 "viajar"))