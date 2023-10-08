#lang racket

(require "main_17986494_SepulvedaBallesteros.rkt")
;Ejemplo de un sistema de chatbots basado en el esquema del enunciado general
;Chabot0
;Opciones unico flujo Chatbot0
(define op1 (option 1 "1) Calendario" 1 1 "fechas" "calendario" "dias"))
(define op2 (option 2 "2) Tareas" 2 1 "tareas" "pendientes" "tasks"))
(define op3 (option 3 "3) Metas" 3 1 "meta" "metas" "Objetivos"))
(define f11 (flow 1 "Flujo Principal Organizador\nBienvenido\n¿Qué te gustaría revisar" op1 op2 op2 op2 op2)) ;solo añade una ocurrencia de op2
(define f12 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada
(define f13 (flow-add-option f10 op3)) ;se intenta añadir opción duplicada
(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría revisar" 1 f13 f13))  ;solo añade una ocurrencia de f13

;Chatbot1
;Opciones primer flujo Chatbot1
(define op4 (option 1 "1) 1 día" 1 2 "dia" "diario"))
(define op5 (option 2 "2) 1 semana" 1 2 "semana" "semanal"))
(define op6 (option 3 "3) 1 mes" 1 2 "mes" "mensual"))
(define op7 (option 4 "4) Volver" 0 1 "Regresar" "Salir" "Volver"))
;Opciones segundo flujo Chatbot1
(define op8 (option 1 "1) Resaltar tareas" 1 2 "Museo"))
(define op9 (option 2 "2) Resaltar eventos" 1 3 "Museo"))
(define op10 (option 3 "3) Resaltar cumpleaños" 1 1 "Cambiar" "Volver" "Salir")) 
(define op11 (option 4 "4) Volver" 1 1 "Solo"))
;Flujos Chatbot 1
(define f20 (flow 1 "Flujo 1 Chatbot1\nElige ventana de tiempo del calendario" op4 op5))
(define f21 (flow-add-option f10 op6 op7)) 
(define f22 (flow 2 "Flujo 2 Chatbot1\n¿Deseas resaltar algo del calendario?" op8 op9 op10 op11))
(define cb1 (chatbot 1 "Calendario"  "Elige ventana de tiempo del calendario" 1 f20 f22)))
;Chatbot2
(define op16 (option 1 "1) Carrera Técnica" 2 1 "Técnica"))
(define op17 (option 2 "2) Postgrado" 2 1 "Doctorado" "Magister" "Postgrado"))
(define op18 (option 3 "3) Volver" 0 1 "Volver" "Salir" "Regresar"))

(define f30 (flow 1 "Flujo 1 Chatbot2\n¿Qué te gustaría estudiar?" op16 op17 op18))
(define cb2 (chatbot 2 "Orientador Académico"  "Bienvenido\n¿Qué te gustaría estudiar?" 1 f30))
;Sistema
(define s0 (system "Chatbot Organizador" 0 cb0 cb0 cb0 cb1 cb2))
(define s1 (system-add-chatbot s0 cb0)) ;igual a s0

;Ejemplos de implementación de las funciones del requerimiento funcional:

;option
(define op1 (option  1 "1) Viajar" 1 1 "viajar" "turistear" "conocer"))
(define op2 (option  2 "2) Estudiar" 2 1 "estudiar" "aprender" "perfeccionarme"))
;flow

;flow-add-option

;chatbot

;chatbot-add-flow

;system

;system-add-chatbot

;system-add-user

;system-login

;system-logout

;system-talk-rec

;system-talk-norec

;system-synthesis


(define s18 (system-talk-rec s10 "hola" "1" "1" "Museo" "1" "3" "5"))