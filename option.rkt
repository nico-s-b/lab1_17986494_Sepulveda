#lang racket

(provide option)
(provide option-code)

;TDA option

;Constructor
;option: Crea una opción para flujo del chatbot
;;;Dominio:
;code (int) número de la opción dada
;message (string) texto de opción a mostrar
;cbotclink (int) indica chatbot al que se enlaza opción
;flowclink (int) indica flujo del chatbot al que se enlaza opción
;keys (cantidad variable) strings que se aceptan como palabras clave para opción
;;;Recorrido: option
(define
  (option code message cbotclink flowclink . keys)
  (if (and (integer? code)
           (string? message)
           (integer? cbotclink)
           (integer? flowclink)
           (or (null? keys) (list? keys))
           )
      (list code message cbotclink flowclink (map string-downcase keys) )
      (begin (display "Error al crear opción") #f)
   )
)

;Selector code
;Dominio: option
;Recorrido: int (code)
(define option-code car)