#lang racket

(require "option.rkt")
(require "flow.rkt")
(require "chatbot.rkt")
(require "system.rkt")

(define op1 (option  1 "1) Viajar" 2 1 "viajar" "turistear" "conocer"))
(define op2 (option  2 "2) Estudiar" 3 1 "estudiar" "aprender" "perfeccionarme"))
(define f10 (flow 1 "flujo1" op1 op2 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2
(define f11 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada
(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))  ;solo añade una ocurrencia de f10
(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0))
(define s1 (system-add-chatbot s0 cb0)) ;igual a s0
(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2 "user2"))
(define s4 (system-add-user s3 "user2")) ;solo añade un ocurrencia de user2
(define s5 (system-add-user s4 "user3"))
(define s6 (system-login s5 "user8")) ;user8 no existe. No inicia sesión
(define s7 (system-login s6 "user1"))
(define s8 (system-login s7 "user2"))  ;no permite iniciar sesión a user2, pues user1 ya inició sesión
(define s9 (system-logout s8))
(define s10 (system-login s9 "user2"))