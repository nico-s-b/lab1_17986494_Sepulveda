#lang racket

(provide option)
(provide flow)
(provide flow-add-option)
(provide chatbot)
(provide chatbot-add-flow)
(provide system)
(provide system-add-chatbot)
(provide system-add-user)
(provide system-login)
(provide system-logout)
(provide system-talk-rec)
(provide system-talk-norec)
(provide system-synthesis)
 
(require "option_17986494_SepulvedaBallesteros.rkt")
(require "flow_17986494_SepulvedaBallesteros.rkt")
(require "chatbot_17986494_SepulvedaBallesteros.rkt")
(require "system_17986494_SepulvedaBallesteros.rkt")
(require "user_17986494_SepulvedaBallesteros.rkt")

;TDA option
;Representación compuesta por una lista formada por:
;code X message X chatbotcodelink X flowcodelink X *keys

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
      (null)
   )
)

;TDA flow
;Representación mediante una lista formada por:
;id X name X *options

;---------------------Constructor---------------------

;flow: crea un flujo para un chatbot
;;;Dominio: id (int) X name (string) X *options
;;;Recorrido: flow
(define flow
  (lambda (id name . options)
    (if (and (integer? id)
             (string? name)
             (or (null? options) (andmap option? options)))
      (list id name (options-rem-duplicates options))
      (raise "Error al crear flow")
    )
  )
)

;---------------------Modificadores---------------------

;flow-add-option: añade una opción a un flujo ya existente. Implementación no recursiva
;;;Dominio: flow X option
;;;Recorrido: flow
(define (flow-add-option flow . new-options)
  (let ([option-list (append (flow-options flow) new-options)]) 
        (list (flow-id flow) (flow-name flow) (options-rem-duplicates option-list))
  )
)


;TDA chatbot

;Representación: lista formada por
; chatbotID X name X welcomeMessage X initialFlowCode X *flows

;---------------------Constructor---------------------

;chatbot: función que construye un chatbot
;;;Dominio: chatbotID (int) X name (string) X welcomeMessage (string) X initialFlowCode (int) X *flows
;;;Recorrido: chatbot
(define chatbot
  (lambda (chatbotID name welcomeMessage startFlowId . flows)
    (if (and (integer? chatbotID)
             (string? name)
             (string? welcomeMessage)
             (integer? startFlowId)
             (or (null? flows) (andmap flow? flows)))
        (list chatbotID name welcomeMessage startFlowId (flows-rem-duplicates flows))
        (raise "Error al crear chatbot")
    )
  )
)

;---------------------Modificadores---------------------

;chatbot-add-flow
;;;Dominio: chatbot X flow
;;;Recorrido: chatbot
;Recursión: natural
;Función aplica recursión natural para verificar la no repitencia del id de flow en la lista de
;flows existentes y lo añade si no está previamente. En caso contrario, devuelve los flows originales.
;Recursión natural se propone por una sintaxis "natural" para el procedimiento, a partir
;de la idea de verificar elemento a elemento de la lista si hay duplicación de id, continuando
;con el resto de la lista en cada llamado recursivo, además de construir la lista incluyendo
;el resultado recursivo con el llamado recursivo dentro de la operación cons / list
(define (chatbot-add-flow chatbot flow)
  (define (add-flows-aux flows flow)
   (cond
     ;caso base 1: agregar si no hay flujos
     [(null? flows) (list flow)]
     ;caso base 2: retornar chatbot sin cambios si flow tiene id repetido existente
     [(equal? (flow-id (car flows)) (flow-id flow)) flows]
     ;llamada recursiva: se mantiene el primer elemento y se continúa la recursión con el resto
     [else (cons (car flows) (add-flows-aux (cdr flows) flow))]) 
   )
  (list (chatbot-id chatbot) (chatbot-name chatbot) (chatbot-welcome chatbot)
        (chatbot-flowid chatbot) (add-flows-aux (chatbot-flows chatbot) flow))
)

;TDA system
;Representación de acuerdo a las operaciones que se hayan llevado a cabo:

;Estado mínimo de system, sin usuarios aún registrados, recién creado:
;name X initialChatbotCodeLink X *chatbots X init-time

;Estado de system con usuarios registrados, sin sesión activa
;name X initialChatbotCodeLink X *chatbots X init-time X *userlist

;Estado de system con una sesión activa (y, por tanto, usuarios registrados)
;name X initialChatbotCodeLink X *chatbots X init-time X *userlist X logged-user

;---------------------Constructor---------------------

;system: función que crea un nuevo sistema de chatbots (system)
;;;Dominio: name (string) X initialChatbotCodeLink (int) X *chatbots
;;;Recorrido: system
(define (system name initialChatbotCodeLink . chatbots)
  (if (and (string? name)
           (integer? initialChatbotCodeLink)
           (or (null? chatbots) (andmap chatbot? chatbots)))
      (list name initialChatbotCodeLink (chatbots-rem-duplicates chatbots) (current-seconds))
      (raise "Error al crear system")
   )
)

;system-add-chatbot: Función que añade chatbots a un sistema existente. No utiliza recursión. Puede
;añadir uno o más chatbots correctamente
;;;Dominio: system X chatbot
;;;Recorrido: system
(define (system-add-chatbot system . chatbots)
  (if (and (system? system)                      ;verificación de la validez de los argumentos
           (andmap chatbot? chatbots))    
      (let ([cb-list (append (system-chatbots system) chatbots)])
        (list (system-name system)
              (system-cblink system)
              (chatbots-rem-duplicates cb-list)     ;evita duplicación de chatbots
              (system-creationtime system))
       )
      (display "No se pudo añadir chatbot")
  )
)

;system-add-user: Añade un usuario al sistema, siempre que éste no esté previamente registrado
;;;Dominio: system X newUser (string)
;;;Recorrido: system
(define (system-add-user system newUser)
  (if (system? system)
      (if (= (length system) 4) ;si aún no se ha agregado ningún usuario, agregar uno
          ;recostruir lista "system" agregando nuevo listado para users
          (list (system-name system) (system-cblink system)
                (system-chatbots system) (system-creationtime system) (list (user newUser)))
          ;Si ya hay una lista de usuarios, primero se busca si ya existe usuario registrado
          (if (null? (user-insystem newUser (system-users system)))
              ;Si no está registrado, se reconstruye sistema agregando el nuevo usuario a userlist
              (list (system-name system) (system-cblink system)
                    (system-chatbots system) (system-creationtime system)
                    (append (system-users system) (list (user newUser))))
              system))    ;Si ya está registrado, se retorna sistema sin cambios
      (display "No se pudo añadir usuario")
  )
)

;---------------------Otras funciones---------------------

;system-login: permite a un usuario iniciar sesión en el sistema si este está
;previamente registrado y si no se encuentra una sesión iniciada ya en el sistema
;;;Dominio: system X user (string)
;;;Recorrido: system
(define (system-login system user)
  (if (and (system? system) (string? user))
      (if (not (system-logged? system))  ;system-logged utiliza lentgh de sistema para verificar sesión inicada
          ;comprobar si el usuario está registrado en el sistema  
          (if (not (null? (user-insystem user (system-users system))))
              ;si no hay una sesión, añade el usuario al final de system como señal sesión iniciada
              (append system (list (user-insystem user (system-users system))))         
              system)
          system
       )
      (display "Error al intentar iniciar sesión"))
)

;system-logout: cierra una sesión abierta por un usuario si hay una activa. Al cerrar la sesión
;se guarda el contenido del chathistory generado en la lista de usuarios del sistema
;;;Dominio: system
;;;Recorrido: system
(define system-logout
  (lambda (system)
    (if (system? system)
        (if (system-logged? system)  ;comprueba si hay sesión iniciada
            ;retorna system sin usuario loggeado
            (list (system-name system)
                  0                      ;Al cerrar sesión, se vuelve al chatbot inicial
                  (system-chatbots system)
                  (system-creationtime system)
                  ;actualizar estado de usuario activo en el sistema antes de cerrar sesión. De esta
                  (system-update-userlist system)) ;forma, se guarda su actividad durante la sesión
            system
         )
    (display "No se realiza acción logout. Sistema no válido"))
  )
)

;system-talk-rec
;Dominio: system X mensajes* (string)
;Recorrido: system
;Recursión: Natural
;Función que permite interactuar con un chatbot. Utiliza recursión natural, teniendo como caso base
;cuando no hay mensaje con el que interactuar.
;Los casos recursivos se dividen en si el mensaje dado está asociado a una opción vigente o no.
;Se utiliza recursión natural puesto que presenta una forma clara de realizar los retornos de la
;función en el caso base, ya que dada la naturaleza de system no se requiere un acumulador, pues
;es una lista de tamaño fijo que puede actualizar sus elementos y basta con devolver el sistema
;original en el caso base. 
;La implementación recursiva es tal que puede manejar más de un mensaje a la vez, pero funciona
;exactamente igual que system-talk-norec cuando es llamada con un solo mensaje.
(define system-talk-rec
  (lambda (system . mens)
    (if (not (system-logged? system))            ;Comprobación de inicio de sesión
        system
        (cond
          ;Caso base: llamado a la función sin mensaje
          [(or (null? mens) (null? (car mens))) system]
          ;Caso recursivo 1: mensaje no asociado a opción
          [(or (null? (system-talk-op system (car mens))))     
           (let ([updated-system (system-update-user system
                               (user-add-talk (system-logged-user system) (system-cblink system)
                                              (flow-id (system-talk-flow system)) (car mens)))])
             ;Llamado recursivo a system-talk-rec con el sistema actualizado y resto de mensajes
           (apply system-talk-rec updated-system (cdr mens)))]
      
          [else
            ;Caso recursivo 2: mensaje asociado a opción
           (let ([opt (system-talk-op system (car mens))])
             (let ([updated-system (list (system-name system) (option-cblink opt)
                                         (system-chatbots system) (system-creationtime system)
                                         (system-update-userlist system)
                                         (user-add-talk (system-logged-user system) (option-cblink opt)
                                                        (option-flink opt) (car mens)))])
               ;Llamado recursivo a system-talk-rec con el sistema actualizado y resto de mensajes
               (apply system-talk-rec updated-system (cdr mens))))]   
        )
    )
  )
)

;system-talk-norec
;Dominio: system X mensaje (string)
;Recorrido: system
;Función que permite interactuar con un chatbot. Verifica si hay una sesión activa, y de acuerdo
;al mensaje dado, verifica si pertenece a alguna opción del flujo actual del chatbot activo.
;En base a esto, devuelve el systema con las actualizaciones necesarias, ya sea de cambio en el
;chatbot activo y con la actualización en el historial de chat del usuario acorde.
(define system-talk-norec
  (lambda (system mens)
    (if (not (system-logged? system)) ;Se comprueba si hay una sesión iniciada
        system                        ;solo es posible conversar si hay una sesión iniciada
        (let ([opt (system-talk-op system mens)]) 
          (if (null? opt)             ;se comprueba si hay una opción asociada al mensaje
              (system-update-user system (user-add-talk
                                                      (system-logged-user system)
                                                      (system-cblink system)
                                                      (flow-id (system-talk-flow system))
                                                      mens)
              )
              (list (system-name system)
                    (option-cblink opt)    ;Actualización de chatbotCodeLink en caso derivación a otro
                    (system-chatbots system)
                    (system-creationtime system)
                    (system-update-userlist system)
                    (user-add-talk        ;Actualizar usuario
                                 (system-logged-user system)
                                 (option-cblink opt)
                                 (option-flink opt)
                                 mens)
              )
           )
        )
     )
  )
)

;system-synthesis: ofrece una síntesis del chatHistory de un usuario particular registrado en el
;sistema, entregando un string formateado que puede visualizarse mediante display.
;La función format-chat realiza el trabajo de formateo de su contenido
;Dominio: system X user (string)
;Recorrido: string
(define system-synthesis 
  (lambda (system user)
    (if (system-logged? system)
        ;Antes de proceder al formateo, actualiza el estado del usuario con su última interacción
        ;si el usuario aún no cierra sesión
        (let ([updated-sys (system-update-user system (user-name (system-logged-user system)))])
          (format-chat user updated-sys (user-chat (system-get-user updated-sys user)))
          )
        ;En caso de no haber sesión iniciada, el chatHistory ya fue guardado completamente
        (format-chat user system (user-chat (system-get-user system user)))
    )
  )
)