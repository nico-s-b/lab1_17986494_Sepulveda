#lang racket

(provide system)
(provide system-name)
(provide system-cblink)
(provide system-chatbots)
(provide system-creationtime)
(provide system-users)
(provide system-get-user)
(provide system-logged-user)
(provide system-add-user)
(provide system-add-chatbot)
(provide system-logged?)
(provide system-login)
(provide system-logout)
(provide system-update-userlist)
(provide system-update-user)
(provide system?)
(provide system-talk-op)
(provide system-talk-flow)
(provide system-talk-norec)
(provide system-talk-rec)
(provide format-chat)

(require "option_17986494_SepulvedaBallesteros.rkt")
(require "flow_17986494_SepulvedaBallesteros.rkt")
(require "chatbot_17986494_SepulvedaBallesteros.rkt")
(require "user_17986494_SepulvedaBallesteros.rkt")
(require "chathistory_17986494_SepulvedaBallesteros.rkt")

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

;---------------------Pertenencia---------------------
;system?: comprueba que un listado de elementos cumpla con el formato de system
;Dominio: system
;Recorrido: boolean
(define (system? system)
  (if (and (>= (length system) 4)
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

;system-creationtime: selecciona el tiempo en el que fue creado el chatbot
(define system-creationtime cadddr)

;system-users: selecciona los usuarios registrados en el sistema, solo si existen
;Dominio: system
;Recorrido: lista de users (list)
(define (system-users sys)
  (if (>= (length sys) 5) ((compose cadddr cdr) sys) null)
)

;system-get-user: devuelve un usuario a partir de su nombre, en caso de estar registrado
;Dominio: system X username (string)
;Recorrido: user
(define (system-get-user system username)
  (let ([maybeuser (filter (lambda (user) (equal? username (user-name user))) (system-users system))])
    (if (null? maybeuser) null (car maybeuser)))
)

;system-logged-user: selecciona el usuario actualmente registrado en el sistema, solo si existe
;Dominio: system
;Recorrido: user
(define (system-logged-user sys)
  (if (= (length sys) 6) ((compose cadddr cddr) sys) null)
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
  (list (system-name system) new-cb-id (system-chatbots system) (system-creationtime system)
        (system-users system) (system-logged-user system))
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

;system-update-user: Actualiza al usuario activo en el sistema
;Dominio: system X user
;Recorrido: system
(define (system-update-user system user)
  (list (system-name system) (system-cblink system) (system-chatbots system)
        (system-creationtime system) (system-update-userlist system) user)
)

;system-save-user: Actualiza usuario loggeado, guardando su nuevo estado dentro de la lista
;de usuarios registrados. Usado para guardar el chatHistory de un usuario en el sistema
;cuando este se desloggea del systema.
;Dominio: system
;Recorrido: users (lista de users del sistema)
;Recursión: natural
(define (system-update-userlist system)
  (let ([userlist (system-users system)])
      (define updating 
        (lambda (lista)
          (if (null? lista)       ;Caso base 1: lista de usuarios vacía
              '()
              (let ([user (system-logged-user system)])
                (if (equal? (user-name user) (user-name (car lista)))
                    ;Caso base 2: encontramos al usuario a reemplazar
                    (cons user (cdr lista))
                    ;Caso recursivo: seguir recorriendo la lista
                    (cons (car lista) (updating (cdr lista)))
                )
              )
            )
          )
        )
      (updating (system-users system))
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

;system-logged?: función que indica si hay una sesión iniciada o no, comprobando el tamaño de system
;Dominio: system
;Recorrido: boolean
(define (system-logged? system)
  (if (>= (length system) 6)
      #t
      #f)
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

;format-chat: función que formatea un chatHistory para formar un string visualizable con display.
;Utiliza recursión natural, construyendo el string recursivamente y entregando un string vacío ""
;en su caso base. Se prefiere por sobre la recursión de cola dado que, de esta forma, resulta
;más clara la concatenación de los distintos llamados de la función, que retorna en cada caso un
;string de un flujo que van sucediéndose uno bajo otro.
;Dominio: chatH
;Recorrido: string
;Recursión: natural
(define format-chat
  (lambda (user system chatH)
    (display chatH)
    (display "\n")
    (if (= (length chatH) 1)  ;Caso base: se agotaron los elementos del chatHistory (solo queda el nombre de ususario
        ""
        ;Definiciones: elemento del chathistory, chatbot y flujo asociados al elemento
        (let* ([chatElem (cadr chatH)]
               [actual-cbot (car (filter (lambda (cb) (equal? (chat-cb chatElem) (chatbot-id cb)))
                                         (system-chatbots system)))]
               [actual-flow (car (filter (lambda (fl) (equal? (chat-fl chatElem) (flow-id fl)))
                                         (chatbot-flows actual-cbot)))])
          (string-append        ;Contatenación de la recursión
           (string-join         ;Concatenación que construye un flujo en particular
            (list (string-append (number->string (chat-time chatElem)) " - "
                                (string-append user ": " (chat-mess chatElem)))
                  (string-append (number->string (system-creationtime system)) " - "
                                (chatbot-name actual-cbot) ": "
                                (flow-name actual-flow))
                  (string-join (map option-message (flow-options actual-flow)) "\n")
                  )
           "\n"
           )
          "\n\n"
          (format-chat user system (append (list user)(cddr chatH)))    ;Llamado recursivo
          )
       )
    )
  )
)