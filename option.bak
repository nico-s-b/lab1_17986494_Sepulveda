#lang racket

(provide option)
(provide option?)
(provide options-rem-duplicates)

;TDA option

;---------------------Constructor---------------------

;option: Crea una opción para flujo del chatbot
;;;Dominio: code (int) X message (string) X chatbotcodelink (int) X flowcodelink (int) X *keys
;;;Recorrido: option
(define
  (option code message chatbotcodelink flowcodelink . keys)
  (if (and (integer? code)
           (string? message)
           (integer? chatbotcodelink)
           (integer? flowcodelink)
           (or (null? keys) (andmap string? keys))
           )
      (list code message chatbotcodelink flowcodelink (map string-downcase keys) )
      (raise "Error al crear opción")
   )
)

;---------------------Pertenencia---------------------

;option?: Comprueba que los elementos de una lista dada cumplen con el formato del TDA option
;Dominio: list
;Recorrido: boolean
(define (option? args)
  (if (and (>= (length args) 4)
           (integer? (option-code args))
           (string? (option-message args))
           (integer? (option-cblink args))
           (integer? (option-flink args))
           (or (null? (option-keys args)) (andmap string? (option-keys args)))
           )
      #t
      #f
    )
)

;---------------------Selectores---------------------

;option-code: seleccionar codigo de opción
;Dominio: option
;Recorrido: int (code)
(define option-code car)

;option-message: seleccionar el mensaje de la opción
;Dominio: option
;Recorrido: string (message)
(define option-message cadr)

;option-cblink: seleccionar chatbotcodelink de la opción
;Dominio: option
;Recorrido: int (chatbotcodelink)
(define option-cblink caddr)

;option-flink: seleccionar flowcodelink de la opcion
;Dominio: option
;Recorrido: int (flowcodelink)
(define option-flink cadddr)

;option-keys: seleccionar el listado de palabras clave de la opción (keys)
;Dominio: option
;Recorrido: list (keys)
(define option-keys (compose cadddr cdr))

;---------------------Otras funciones---------------------

;options-rem-duplicates: remueve opciones duplicadas de una lista de opciones en base
;al id de la opción, dejando la primera aparición de la opción únicamente
;;;Dominio: options (list)
;;;Recorrido: options (list)
;Utiliza la función remove-duplicates, utilizando como criterio la igualdad de IDs de las opciones.
(define (options-rem-duplicates options)
  (remove-duplicates options
                     (lambda (x y) (equal? (option-code x) (option-code y) ))
  )
)