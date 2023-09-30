#lang racket

(provide system)
(provide system-add-chatbot)
(provide system-add-user)
(provide system-login)
(provide system-logout)
(provide system?)

(require "option.rkt")
(require "flow.rkt")
(require "chatbot.rkt")
(require "user.rkt")

;TDA system

;---------------------Constructor---------------------

;system: función que crea un nuevo sistema de chatbots (system)
;;;Dominio: name (string) X initialChatbotCodeLink (int) X *chatbots
;;;Recorrido: system
(define (system name initialChatbotCodeLink . chatbots)
  (if (and (string? name)
           (integer? initialChatbotCodeLink)
           (or (null? chatbots) (andmap chatbot? chatbots)))
      (list name initialChatbotCodeLink (chatbots-rem-duplicates chatbots))
      (raise "Error al crear system")
   )
)

;---------------------Pertenencia---------------------
;system?: comprueba que un listado de elementos cumpla con el formato de system
;Dominio: system
;Recorrido: boolean
(define (system? system)
  (if (and (>= (length system) 2)
           (string? (system-name system))
           (integer? (system-cblink system))
           (or (null? (system-chatbots system)) (andmap chatbot? (system-chatbots system))))
      #t
      #f)
)

;---------------------Selectores---------------------

;system-name: selecciona el nombre del sistema
;Dominio: system
;Recorrido: name (string)
(define system-name car)

;system-cblink: selecciona el código inicial initialChatbotCodeLink del sistema
;Dominio: system
;Recorrido: initialChatbotCodeLink (int)
(define system-cblink cadr)

;system-chatbots: selecciona el listado de chatbots del sistema
;Dominio: system
;Recorrido: lista de chatbots (list)
(define system-chatbots caddr)

;system-users: selecciona los usuarios registrados en el sistema
;Dominio: system
;Recorrido: lista de users (list)
(define system-users cadddr)

;---------------------Modificadores---------------------

;system-add-chatbot: Función que añade chatbots a un sistema existente
;;;Dominio: system X chatbot
;;;Recorrido: system
(define (system-add-chatbot system . chatbots)
  (if (and (system? system)
           (andmap chatbot? chatbots))
      (let ([cb-list (append (system-chatbots system) chatbots)])
        (list (system-name system)
              (system-cblink system)
              (chatbots-rem-duplicates cb-list))
       )
      (display "No se pudo añadir chatbot")
  )
)

;system-add-user: Añade un usuario al sistema
;;;Dominio: system X newUser (string)
;;;Recorrido: system
(define (system-add-user system newUser)
  (if (system? system)
      (if (= (length system) 3) ;si aún no se ha agregado ningún usuario, agregar uno
          (list (system-name system) (system-cblink system)
                (system-chatbots system) (list (user newUser)))
          (if (null? (user-insystem newUser (system-users system)))
              (list (system-name system) (system-cblink system)
                    (system-chatbots system) (append (system-users system) (list (user newUser))))
              system))
      (display "No se pudo añadir usuario")
  )
)

;---------------------Otras funciones---------------------

;system-login: permite a un usuario iniciar sesión en el sistema si este está
;previamente registrado y si no se encuentra una sesión iniciada
;;;Dominio: system X user (string)
;;;Recorrido: system
(define (system-login system user)
  (if (and (system? system) (string? user))
      (if (not (system-logged? system))  ;si system tiene menos de 5 elementos, no hay una sesión iniciada
          (if (not (null? (user-insystem user (system-users system)))) ;comprobar si el usuario está registrado en el sistema  
              (append system (list (user-insystem user (system-users system))))         ;si no hay una sesión, añade el usuario al final de system como señal de inicio de sesión
              system)
          system
       )
      (display "Error al intentar iniciar sesión"))
)

;system-logged?: función que indica si hay una sesión iniciada o no. Lo hace comprobando el tamaño de system
;Dominio: system
;Recorrido: boolean
(define (system-logged? system)
  (if (>= (length system) 5)
      #t
      #f)
)

;system-logout: cierra una sesión abierta por un usuario si hay una activa
;;;Dominio: system
;;;Recorrido: system
(define system-logout
  (lambda (system)
    (if (system? system)
        (if (system-logged? system)  ;comprueba si hay sesión iniciada (system tiene al menos 5 elementos)
            (list (system-name system) (system-cblink system)
                  (system-chatbots system) (system-users system))
            system)
        (display "No se realiza acción logout. Sistema no válido"))
))

;system-talk-rec
;Dominio: system X mensaje (string)
;Recorrido: system
(define system-talk-rec
  (lambda (system mens)
    (if (system-logged? system) ;Se comprueba si hay una sesión iniciada
        (1)
        system) ;solo es posible conversar si hay una sesión iniciada
  )
)


(define op1 (option  1 "1) Viajar" 2 1 "viajar" "turistear" "conocer"))
(define op2 (option  2 "2) Estudiar" 3 1 "estudiar" "aprender" "perfeccionarme"))
(define f10 (flow 1 "flujo1" op1 op2 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2
(define f11 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada
(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))  ;solo añade una ocurrencia de f10
(define cb1 (chatbot 1 "cb2" "Hola" 2 f11))
(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0 cb1))
(define cbl (append (system-chatbots s0) (list cb0 cb1)))
(define s1 (system-add-chatbot s0 cb0)) ;igual a s0
(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2 "user2"))
(define s4 (system-add-user s3 "user2")) ;solo añade un ocurrencia de user2