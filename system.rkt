#lang racket

(provide system)
(provide system-add-chatbot)
(provide system-add-user)
(provide system-login)
(provide system-logout)
(provide system?)
(provide system-talk-norec)

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

;system-users: selecciona los usuarios registrados en el sistema, solo si existen
;Dominio: system
;Recorrido: lista de users (list)
(define (system-users sys)
  (if (>= (length sys) 4) (cadddr sys) null)
)

;system-logged-user: selecciona el usuario actualmente registrado en el sistema, solo si existe
;Dominio: system
;Recorrido: user
(define (system-logged-user sys)
  (if (= (length sys) 5) ((compose cadddr cdr) sys) null)
)

;system-talk-cb: función que selecciona, dentro de los chatbots existentes, aquel activo
;en el sistema de acuerdo al chatbot-codelink actual
;Dominio: system
;Recorrido: chatbot
(define (system-talk-chatbot sys)
  (car (filter
        (lambda (cb) (equal? (chatbot-id cb) (system-cblink sys)))
        (system-chatbots sys)))
)

;system-talk-flow: selecciona al flow activo actual en el chatbot cargado en el sistema
;Dominio: system
;Recorrido: flow
(define (system-talk-flow system)
  (chatbot-talk-flow (system-talk-chatbot system))
)

;system-talk-op: selecciona la opción correspondiente al mensaje dado de acuerdo al
;chatbot actual y su flujo activo. Si el mensaje es string, flow-talk-op lo transforma a lowercase
;Dominio: system X mensaje (string)
;Recorrido: option
(define system-talk-op
  (lambda (system mens)
    (flow-talk-op (system-talk-flow system) mens)
  )
)

;---------------------Modificadores---------------------

;system-change-chatbot: función que modifica el chatbot activo actual del sistema
;Dominio: system X chatbot-id (int)
;Recorrido: system
(define (system-change-chatbot new-cb-id)
  (list (system-name system) new-cb-id (system-chatbots system)
        (system-users system) (system-logged-user system))
)

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

;system-update-user: Actualiza al usuario activo en el sistema
;Dominio: system X user
;Recorrido: system
(define (system-update-user system user)
  (list (system-name system) (system-cblink system) (system-chatbots system)
        (system-users system) user)
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


;system-talk-norec
;Dominio: system X mensaje (string)
;Recorrido: system
(define system-talk-norec
  (lambda (system mens)
    (if (system-logged? system) ;Se comprueba si hay una sesión iniciada
        (let ([opt (system-talk-op system mens)])
          (if (null? opt)
              (system-update-user system
                                  (user-add-talk
                                   (system-logged-user system) (system-cblink system)
                                   (flow-id (system-talk-flow system)) mens))
              (list (system-name system) (option-cblink opt) (system-chatbots system)
                      (system-users system) (user-add-talk
                                             (system-logged-user system) (option-cblink opt)
                                             (option-flink opt) mens)
              )
           )
        )
        system) ;solo es posible conversar si hay una sesión iniciada
  )
)

;Ejemplo de un sistema de chatbots basado en el esquema del enunciado general
;Chabot0
(define op1 (option  1 "1) Viajar" 1 1 "viajar" "turistear" "conocer"))
(define op2 (option  2 "2) Estudiar" 2 1 "estudiar" "aprender" "perfeccionarme"))
(define f10 (flow 1 "Flujo Principal Chatbot 1\nBienvenido\n¿Qué te gustaría hacer?" op1 op2 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2
(define f11 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada            
(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))  ;solo añade una ocurrencia de f10
;Chatbot1
(define op3 (option 1 "1) New York, USA" 1 2 "USA" "Estados Unidos" "New York"))
(define op4 (option 2 "2) París, Francia" 1 1 "Paris" "Eiffel"))
(define op5 (option 3 "3) Torres del Paine, Chile" 1 1 "Chile" "Torres" "Paine" "Torres Paine" "Torres del Paine"))
(define op6 (option 4 "4) Volver" 0 1 "Regresar" "Salir" "Volver"))
;Opciones segundo flujo Chatbot1
(define op7 (option 1 "1) Central Park" 1 2 "Central" "Park" "Central Park"))
(define op8 (option 2 "2) Museos" 1 2 "Museo"))
(define op9 (option 3 "3) Ningún otro atractivo" 1 3 "Museo"))
(define op10 (option 4 "4) Cambiar destino" 1 1 "Cambiar" "Volver" "Salir")) 
(define op11 (option 1 "1) Solo" 1 3 "Solo")) 
(define op12 (option 2 "2) En pareja" 1 3 "Pareja"))
(define op13 (option 3 "3) En familia" 1 3 "Familia"))
(define op14 (option 4 "4) Agregar más atractivos" 1 2 "Volver" "Atractivos"))
(define op15 (option 5 "5) En realidad quiero otro destino" 1 1 "Cambiar destino"))
(define f20 (flow 1 "Flujo 1 Chatbot1\n¿Dónde te Gustaría ir?" op3 op4 op5 op6))
(define f21 (flow 2 "Flujo 2 Chatbot1\n¿Qué atractivos te gustaría visitar?" op7 op8 op9 op10))
(define f22 (flow 3 "Flujo 3 Chatbot1\n¿Vas solo o acompañado?" op11 op12 op13 op14 op15))
(define cb1 (chatbot 1 "Agencia Viajes"  "Bienvenido\n¿Dónde quieres viajar?" 1 f20 f21 f22))
;Chatbot2
(define op16 (option 1 "1) Carrera Técnica" 2 1 "Técnica"))
(define op17 (option 2 "2) Postgrado" 2 1 "Doctorado" "Magister" "Postgrado"))
(define op18 (option 3 "3) Volver" 0 1 "Volver" "Salir" "Regresar"))

(define f30 (flow 1 "Flujo 1 Chatbot2\n¿Qué te gustaría estudiar?" op16 op17 op18))
(define cb2 (chatbot 2 "Orientador Académico"  "Bienvenido\n¿Qué te gustaría estudiar?" 1 f30))
;Sistema
(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0 cb1 cb2))
(define s1 (system-add-chatbot s0 cb0)) ;igual a s0
(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2 "user2"))
(define s4 (system-add-user s3 "user2"))
(define s5 (system-add-user s4 "user3"))
(define s6 (system-login s5 "user8"))
(define s7 (system-login s6 "user1"))
(define s8 (system-login s7 "user2"))
(define s9 (system-logout s8))
(define s10 (system-login s9 "user2"))

(define s11 (system-talk-norec s10 "hola"))
(define s12 (system-talk-norec s11 "1"))
(define s13 (system-talk-norec s12 "1"))
(define s14 (system-talk-norec s13 "Museo"))
(define s15 (system-talk-norec s14 "1"))
(define s16 (system-talk-norec s15 "3"))
(define s17 (system-talk-norec s16 "5"))