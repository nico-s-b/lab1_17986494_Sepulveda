#lang racket

(provide flow)
(provide flow-add-option)

(define
  (flow id name . options)
  (if (and (integer? id)
           (string? name))
      (list id name )
      ((display "Error al crear flujo") #f)
   )
)

(define flow-add-option
  (lambda (flow option)
    (proc))
)
