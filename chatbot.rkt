#lang racket

(provide chatbot)
(provide chatbot-add-flow)
(require "option.rkt")
(require "flow.rkt")

(define chatbot
  (lambda (chatbotID name welcomeMessage . flows)
    (if (and (integer? chatbotID)
             (string? name)
             (string? welcomeMessage))
        (list chatbotID name welcomeMessage (flows-rem-duplicates flows))
        (begin (display "Error al crear chatbot") #f)
    )
  )
)