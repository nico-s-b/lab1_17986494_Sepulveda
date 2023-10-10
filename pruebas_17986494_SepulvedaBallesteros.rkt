#lang racket

(require "main_17986494_SepulvedaBallesteros.rkt")

;*************************************************
;**************SCRIPT DE PRUEBAS N°1**************
;*************************************************

(define opx1 (option  1 "1) Viajar" 2 1 "viajar" "turistear" "conocer"))
(define opx2 (option  2 "2) Estudiar" 3 1 "estudiar" "aprender" "perfeccionarme"))
(define fx10 (flow 1 "flujo1" opx1 opx2 opx2 opx2 opx2 opx1)) ;solo añade una ocurrencia de op2
(define fx11 (flow-add-option fx10 opx1)) ;se intenta añadir opción duplicada
(define cbx0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 fx10 fx10 fx10 fx10))  ;solo añade una ocurrencia de f10
(define sx0 (system "Chatbots Paradigmas" 0 cbx0 cbx0 cbx0))
(define sx1 (system-add-chatbot sx0 cbx0)) ;igual a s0
(define sx2 (system-add-user sx1 "user1"))
(define sx3 (system-add-user sx2 "user2"))
(define sx4 (system-add-user sx3 "user2")) ;solo añade un ocurrencia de user2
(define sx5 (system-add-user sx4 "user3"))
(define sx6 (system-login sx5 "user8")) ;user8 no existe. No inicia sesión
(define sx7 (system-login sx6 "user1"))
(define sx8 (system-login sx7 "user2"))  ;no permite iniciar sesión a user2, pues user1 ya inició sesión
(define sx9 (system-logout sx8))
(define sx10 (system-login sx9 "user2"))

;*************************************************
;**************SCRIPT DE PRUEBAS N°2**************
;*************************************************

;Ejemplo de un sistema de chatbots basado en el esquema del enunciado general
;Chabot0
(define op1x (option  1 "1) Viajar" 1 1 "viajar" "turistear" "conocer"))
(define op2x (option  2 "2) Estudiar" 2 1 "estudiar" "aprender" "perfeccionarme"))
(define f10x (flow 1 "Flujo Principal Chatbot 1\nBienvenido\n¿Qué te gustaría hacer?" op1x op2x op2x op2x op2x op1x)) ;solo añade una ocurrencia de op2
(define f11x (flow-add-option f10x op1x)) ;se intenta añadir opción duplicada            
(define cb0x (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10x f10x f10x f10x))  ;solo añade una ocurrencia de f10
;Chatbot1
(define op3x (option 1 "1) New York, USA" 1 2 "USA" "Estados Unidos" "New York"))
(define op4x (option 2 "2) París, Francia" 1 1 "Paris" "Eiffel"))
(define op5x (option 3 "3) Torres del Paine, Chile" 1 1 "Chile" "Torres" "Paine" "Torres Paine" "Torres del Paine"))
(define op6x (option 4 "4) Volver" 0 1 "Regresar" "Salir" "Volver"))
;Opciones segundo flujo Chatbot1
(define op7x (option 1 "1) Central Park" 1 2 "Central" "Park" "Central Park"))
(define op8x (option 2 "2) Museos" 1 2 "Museo"))
(define op9x (option 3 "3) Ningún otro atractivo" 1 3 "Museo"))
(define op10x (option 4 "4) Cambiar destino" 1 1 "Cambiar" "Volver" "Salir")) 
(define op11x (option 1 "1) Solo" 1 3 "Solo")) 
(define op12x (option 2 "2) En pareja" 1 3 "Pareja"))
(define op13x (option 3 "3) En familia" 1 3 "Familia"))
(define op14x (option 4 "4) Agregar más atractivos" 1 2 "Volver" "Atractivos"))
(define op15x (option 5 "5) En realidad quiero otro destino" 1 1 "Cambiar destino"))
(define f20x (flow 1 "Flujo 1 Chatbot1\n¿Dónde te Gustaría ir?" op3x op4x op5x op6x))
(define f21x (flow 2 "Flujo 2 Chatbot1\n¿Qué atractivos te gustaría visitar?" op7x op8x op9x op10x))
(define f22x (flow 3 "Flujo 3 Chatbot1\n¿Vas solo o acompañado?" op11x op12x op13x op14x op15x))
(define cb1x (chatbot 1 "Agencia Viajes"  "Bienvenido\n¿Dónde quieres viajar?" 1 f20x f21x f22x))
;Chatbot2
(define op16x (option 1 "1) Carrera Técnica" 2 1 "Técnica"))
(define op17x (option 2 "2) Postgrado" 2 1 "Doctorado" "Magister" "Postgrado"))
(define op18x (option 3 "3) Volver" 0 1 "Volver" "Salir" "Regresar"))

(define f30x (flow 1 "Flujo 1 Chatbot2\n¿Qué te gustaría estudiar?" op16x op17x op18x))
(define cb2x (chatbot 2 "Orientador Académico"  "Bienvenido\n¿Qué te gustaría estudiar?" 1 f30x))
;Sistema
(define s0x (system "Chatbots Paradigmas" 0 cb0x cb0x cb0x cb1x cb2x))
(define s1x (system-add-chatbot s0x cb0x)) ;igual a s0
(define s2x (system-add-user s1x "user1"))
(define s3x (system-add-user s2x "user2"))
(define s4x (system-add-user s3x "user2"))
(define s5x (system-add-user s4x "user3"))
(define s6x (system-login s5x "user8"))
(define s7x (system-login s6x "user1"))
(define s8x (system-login s7x "user2"))
(define s9x (system-logout s8x))
(define s10x (system-login s9x "user2"))

(define s11x (system-talk-norec s10x "hola"))
(define s12x (system-talk-rec s11x "1"))
(define s13x (system-talk-rec s12x "1"))
(define s14x (system-talk-rec s13x "Museo"))
(define s15x (system-talk-rec s14x "1"))
(define s16x (system-talk-rec s15x "3"))
(define s17x (system-talk-rec s16x "5"))
(display (system-synthesis s17x "user2"))
(system-simulate s0x 5 32131)
(display (system-synthesis (system-simulate s0x 5 312321) "user312321"))


;*************************************************
;**************SCRIPT DE PRUEBAS N°3**************
;*************************************************


;Ejemplo propio de un sistema de chatbots basado en el esquema del enunciado general
;Chabot0
;Opciones unico flujo Chatbot0
(define op1 (option 1 "1) Calendario" 1 1 "fechas" "calendario" "dias"))
(define op2 (option 2 "2) Tareas" 2 1 "tareas" "pendientes" "tasks"))
(define op3 (option 3 "3) Metas" 3 1 "meta" "metas" "Objetivos"))
(define f11 (flow 1 "Flujo Principal Organizador\nBienvenido\n¿Qué te gustaría revisar" op1 op2 op2 op2 op2)) ;solo añade una ocurrencia de op2
;flow-add-option
(define f12 (flow-add-option f11 op1)) ;se intenta añadir opción duplicada
(define f13 (flow-add-option f12 op3)) ;se intenta añadir opción duplicada
(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría revisar" 1 f13 f13))  ;solo añade una ocurrencia de f13

;Chatbot1
;Opciones primer flujo Chatbot1
(define op4 (option 1 "1) 1 día" 1 2 "dia" "diario"))
(define op5 (option 2 "2) 1 semana" 1 2 "semana" "semanal"))
(define op6 (option 3 "3) 1 mes" 1 2 "mes" "mensual"))
(define op7 (option 4 "4) Volver" 0 1 "Regresar" "Salir" "Volver"))
;Opciones segundo flujo Chatbot1
(define op8 (option 1 "1) Resaltar tareas" 1 2 "tareas"))
(define op9 (option 2 "2) Resaltar eventos" 1 2 "eventos"))
(define op10 (option 3 "3) Resaltar cumpleaños" 1 2 "cumpleaños" "cumples")) 
(define op11 (option 4 "4) Volver" 1 1 "Regresar" "Salir" "Volver"))
;Flujos Chatbot 1
(define f20 (flow 1 "Flujo 1 Chatbot1\nElige ventana de tiempo del calendario" op4 op5))
(define f21 (flow-add-option f20 op6 op7)) 
(define f22 (flow 2 "Flujo 2 Chatbot1\n¿Deseas resaltar algo del calendario?" op8 op9 op10 op11))
(define cb1 (chatbot 1 "Calendario"  "Elige ventana de tiempo del calendario" 1 f21 f22))

;Chatbot2
;Flujo 1
(define op12 (option 1 "1) Ver listado de tareas" 2 2 "ver" "Listado" "Tareas"))
(define op13 (option 2 "2) Añadir tarea" 2 1 "agregar" "añadir" "add"))
(define op14 (option 3 "3) Eliminar tarea" 2 1 "eliminar" "borrar" "remove"))
(define op15 (option 4 "4) Volver" 0 1 "Regresar" "Salir" "Volver"))
;Flujo 2
(define op16 (option 1 "1) Por prioridad" 2 1 "prioridad" "importancia"))
(define op17 (option 2 "1) Por fecha" 2 1 "tiempo" "dia" "fecha"))
(define op18 (option 3 "1) Por estado" 2 1 "estado"))
(define op19 (option 4 "4) Volver" 2 1 "Regresar" "Salir" "Volver"))
;Flujos Chatbot 2
(define f30 (flow 1 "Flujo 1 Chatbot2\n¿Qué te gustaría hacer con las tareas?" op12 op13 op14 op15))
(define f31 (flow 2 "Flujo 2 Chatbot2\n¿De qué forma quieres filtrar las tareas?" op16 op17 op18 op19))
(define cb2 (chatbot 2 "Tareas"  "Bienvenido\n¿Qué te gustaría hacer con las tareas?" 1 f30))
;chatbot-add-flow
(define cb3 (chatbot-add-flow cb2 f31))   ;agregar flow de forma recursiva, añadiendo una sola ocurrencia
(define cb4 (chatbot-add-flow cb3 f31))   ;agregar flow de forma recursiva, añadiendo una sola ocurrencia
;Chatbot3
;Flujo1
(define op20 (option 1 "1) Ver metas" 3 1 "ver" "Listado" "metas"))
(define op21 (option 2 "2) Cambiar meta" 3 1 "modificar" "cambiar"))
(define op22 (option 3 "3) Asignar meta" 3 1 "asignar" "nueva" "añadir"))
(define op23 (option 4 "4) Volver" 0 1 "Regresar" "Salir" "Volver"))
(define f40 (flow 1 "Flujo 1 Chatbot3\n¿Qué te gustaría hacer con las metas?")) ;Crear flow sin opciones
(define f41 (flow-add-option f40 op20 op21 op22 op23))    ;Rellenar flow tras crearlo
(define cb5 (chatbot 3 "Metas"  "Bienvenido\n¿Qué te gustaría hacer con las metas?" 1)) ;Crear chatbot sin flows
(define cb6 (chatbot-add-flow cb5 f41))     ;Rellenar chatbot tras crearlo

;Sistema
(define s0 (system "Chatbot Organizador" 0 cb0 cb0 cb1 cb1))
;system-add-chatbot
(define s1 (system-add-chatbot s0 cb4))
(define s2 (system-add-chatbot s1 cb6)) 
(define s3 (system-add-chatbot s2 cb6))  ;igual a s1

;system-add-user
(define s4 (system-add-user s3 "user1"))
(define s5 (system-add-user s4 "user1"))
(define s6 (system-add-user s5 "user2"))
(define s7 (system-add-user s6 "user3"))

;system-login & log-out
(define s8 (system-login s7 "user2"))
(define s9 (system-logout s8))
(define s10 (system-login s9 "user1"))

;user1 interactuando con sistema
;system-talk-rec & no-rec (uso indistinto alternado
(define s11 (system-talk-norec s10 "hola"))
(define s12 (system-talk-rec s11 "1"))
(define s13 (system-talk-rec s12 "semana" "eventos"))  ;talk-rec procesando más de un mensaje
(define s14 (system-talk-norec s13 "4"))
(define s15 (system-logout s14))

;user2 interactuando con sistema
(define s16 (system-talk-norec s15 "hola"))   ;no hay interacción pues no hay sesión iniciada
(define s17 (system-login s16 "user22"))       ;no hay login pues user22 no existe en sistema
(define s18 (system-login s17 "user2"))
(define s19 (system-talk-norec s18 "que tal?"))
(define s20 (system-talk-rec s19 "3"))
(define s21 (system-talk-norec s20 "ver"))
(define s22 (system-talk-rec s21 "4" "2"))      ;talk-rec procesando más de un mensaje
(define s23 (system-talk-norec s22 "prioridad"))
(define s24 (system-talk-rec s23 "1"))
(define s25 (system-talk-rec s24 "volver" ))
(define s26 (system-talk-norec s25 "no se"))    ;interaccion con mensaje sin opcion, se mantiene flujo
(define s27 (system-talk-norec s26 "calendario"))
(define s28 (system-logout s27))
;system-synthesis
(display (system-synthesis s28 "user2"))    ;síntesis de dos usuarios distintos con chathistory almacenado
(define s29 (system-login s28 "user3"))       
(display (system-synthesis s29 "user1"))    ;la segunda síntesis se puede hacer aunque haya otro usuario loggeado
(define s30 (system-login s29 "user2"))    ;user2 no puede iniciar sesión ya que user3 está loggeado
;system-simulate
(system-simulate s3 4 312321)
;La siguiente simulación generada no permite hacer síntesis, pero esto ocurre porque no están añadidos todos
;los chatbots y por tanto alguna de las opciones generadas conduce a un flujo existente que deriva
;a un chatbotcode no cargado en el sistema
(system-simulate s0 5 312321)  ;<-- fallará hacer system-synthesis de esta simulación**
(system-simulate s3 5 312321)    ;problema se resuelve cuando el system tiene todos los chatbots
(system-simulate s27 6 1234)     ;Simulación puede hacerse con o sin sesión iniciada por otro usuario
(display (system-synthesis (system-simulate s28 6 1234) "user1234")) ;display de simulación