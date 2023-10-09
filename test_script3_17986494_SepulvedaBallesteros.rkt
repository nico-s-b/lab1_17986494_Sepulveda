#lang racket

(require "main_17986494_SepulvedaBallesteros.rkt")

;Ejemplo propio de un sistema de chatbots basado en el esquema del enunciado general
;Chabot0
;Opciones unico flujo Chatbot0
(define op1 (option 1 "1) Calendario" 1 1 "fechas" "calendario" "dias"))
(define op2 (option 2 "2) Tareas" 2 1 "tareas" "pendientes" "tasks"))
(define op3 (option 3 "3) Metas" 3 1 "meta" "metas" "Objetivos"))
(define f11 (flow 1 "Flujo Principal Organizador\nBienvenido\n¿Qué te gustaría revisar" op1 op2 op2 op2 op2)) ;solo añade una ocurrencia de op2
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
(define s0 (system "Chatbot Organizador" 0 cb0 cb0 cb1 cb1 cb4))
(define s1 (system-add-chatbot s0 cb6)) 
(define s2 (system-add-chatbot s0 cb6))  ;igual a s1

;system-add-user
(define s3 (system-add-user s2 "user1"))
(define s4 (system-add-user s3 "user1"))
(define s5 (system-add-user s4 "user2"))
(define s6 (system-add-user s5 "user3"))

;system-login & log-out
(define s7 (system-login s6 "user2"))
(define s8 (system-logout s7))
(define s9 (system-login s8 "user1"))

;user1 interactuando con sistema
;system-talk-rec & no-rec (uso indistinto alternado
(define s10 (system-talk-norec s9 "hola"))
(define s11 (system-talk-rec s10 "1"))
(define s12 (system-talk-rec s11 "semana" "eventos"))  ;talk-rec procesando más de un mensaje
(define s13 (system-talk-norec s12 "4"))
(define s14 (system-logout s13))

;user2 interactuando con sistema
(define s15 (system-talk-norec s14 "hola"))   ;no hay interacción pues no hay sesión iniciada
(define s16 (system-login s15 "user22"))       ;no hay login pues user22 no existe en sistema
(define s17 (system-login s16 "user2"))
(define s18 (system-talk-norec s17 "que tal?"))
(define s19 (system-talk-rec s18 "3"))
(define s20 (system-talk-norec s19 "ver"))
(define s21 (system-talk-rec s20 "4" "2"))      ;talk-rec procesando más de un mensaje
(define s22 (system-talk-norec s21 "prioridad"))
(define s23 (system-talk-rec s22 "1"))
(define s24 (system-talk-rec s23 "volver" ))
(define s25 (system-talk-norec s24 "no se"))    ;interaccion con mensaje sin opcion, se mantiene flujo
(define s26 (system-talk-norec s25 "calendario"))
(define s27 (system-logout s26))
;system-synthesis
(display (system-synthesis s27 "user2"))    ;síntesis de dos usuarios distintos con chathistory almacenado
(define s28 (system-login s27 "user3"))       
(display (system-synthesis s28 "user1"))    ;la segunda síntesis se puede hacer aunque haya otro usuario loggeado
(define s29 (system-login s28 "user2"))    ;user2 no puede iniciar sesión ya que user3 está loggeado