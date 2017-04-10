#lang racket

;Space invader
;https://github.com/heticor915/VideoJuego
;Hector Favio Jimenes
;Brian Ruiz Idarraga


;librerias
(require 2htdp/image
 (only-in racket/gui/base play-sound))
(require 2htdp/universe)


;Constantes  Detalles Tecnicos
(define ancho 780)
(define alto 500)
(define veloz-nave 25) ;velocidad nave propia
(define veloz-bala 15)
(define veloz-aliens 5)
(define intensidad 3);intensidad de ataque
(define proyectil (rectangle 5 10 "solid" "red"))
(define rango-acierto 20)
(define fondo .) ;fondo     
(define game-over-im .) ;imagen game-over 
(define nave1 .)   ;nave1
(define nave2 .)   ;nave2
(define aliens1 .)   ;aliens1
(define aliens2 .)   ;aliens2

;posibilidad de ataque enemigo
(define int (truncate (/ ancho  intensidad))) 

;Funciones de Sonido.
(define (disparo) (play-sound "Sounds/shoot.wav" #t))  ;Esto va a reproducir  el nombre de archivo completo, el sonido es corto.
(define (explosion) (play-sound "Sounds/explosion.wav" #t))  ;
(define (menu) (play-sound "Sounds/menu.wav" #t))              ;Se debe recortar a 10 segundos héctor lo hace.

;nivel del juego
(define nivel 1)
(define puntuacion 0)

;cambia los parametros del juego, aumenta la dificultad
(define (dificultad a)
 (begin 
  (set! veloz-nave (+ veloz-nave 4))
  (set! nivel (+ nivel 1))
  (set! intensidad (+ intensidad 1))
  (set! veloz-aliens (+ veloz-aliens 1))
  (set! veloz-bala (+ veloz-bala 1))
  (if 
   (= nivel 5) 
   (set! aliens1 aliens2)
   ""
  )     
  (* 20 (+ 1 (random nivel)))  
 )
) 


;lista donde se guarda a los enemigos

(define enemigos 
 (list 
  (list (random ancho) 20 #t) 
  (list (random ancho) 40 #f) 
  (list (random ancho) 60 #t)
  (list (random ancho) 80 #f)
 )
)

;envía la lista de enemigos y agrega un enemigo
(define (dame-enemigos a) 
 (begin
  (set! enemigos 
   (cons 
    (list (random ancho) (dificultad ";)") #t)
    enemigos
   )               
  )
  enemigos  
 )
)



;mundo
(define-struct mundo [jugador disparo enemigos])


;muestra en pantalla la nave y envía datos a "mostrar-balas"
(define (comunicador m)
 (place-image 
  nave1 
  (mundo-jugador m) 
  (- alto 20) 
  (ver-bala (mundo-disparo m) (mundo-enemigos m))
 )
)

;muestra las balas en pantalla y envía datos a "ver-aliens"
(define (ver-bala b puente)
 (cond
  [(empty? b) (ver-aliens puente)]      
  [(cons? b)                          
    (place-image 
     proyectil 
     (caar b) 
     (cadar b)
     (ver-bala (cdr b) puente) 
    )        
  ]
  [else "error"]
 )
)

;muestra los aliens en pantalla
(define (ver-aliens b)
 (cond
  [(empty? b) (texto)]      
  [(cons? b)                          
   (place-image 
    aliens1 
    (caar b) 
    (cadar b)
    (ver-aliens (cdr b)) 
   )        
  ]
  [else "error"]
 )
)

(define (texto)
 (place-image 
  (text 
   (string-append "puntos: "(number->string puntuacion)) 24 "blue") 
   700 
   40
   fondo 
 )
)

;función que cambia los valores, se usa para actualizar posiciones de
;objetos, también para crear otros objetos de manera temporizada,
;la función principal reciva todos los datos del juego y los envía a otras funciones
;que modifican los valores para generar efectos en el juego.
(define (monitoreo m)   
 (cond
  ;cuando pasa de nivel se retiran las valas aca 
  [(not (cons? (mundo-enemigos m))) 
   (make-mundo
    (mundo-jugador m)
    empty
    (dame-enemigos (mundo-enemigos m))
   )]
  ;ejecura el juego noral
  [else
   (make-mundo
    (mundo-jugador m)
    (mover-balas  (mundo-disparo m))
    (mover-aliens (remover-aliens (mundo-enemigos m) (mundo-disparo m)))
   )
  ]
 )
)

;cambia la altura de las balas (resta posiciones para avansar)
(define (mover-balas b)
 (cond
  [(empty? b) empty]
  [(< (cadar b) 20) empty];elimina las balas lejanas   
  [(cons? b) 
   (cons  
    (list 
     (caar b)   
     (- (cadar b) veloz-bala)
    )                  
    (mover-balas (cdr b))
   )
  ]
 )  
)

;cambia la posición de los aliens:
;la función busca que los aliens bajen en un
;momento aleatorio
;(mover-aliens mundo-enemigo)

(define (mover-aliens a)
 (cond
  [(empty? a) empty]
  ;hace que los alien bajen
  [(number? (car (cddar a))) 
   (cons
    (list 
     (caar a)
     (+ (cadar a) veloz-aliens)
     (car (cddar a))
    )
    (mover-aliens (cdr a))
   )
  ]
  ;mueve a la derecha
  [(car (cddar a)) 
   (cons  
    (list     
     (+ (caar a) veloz-aliens )
     (+ (cadar a) 0)
     (if (> ancho (caar a)) 
      (if (= 1 (random int))
       (+ 20 (random (- ancho 20)))
       #t
      )       
       #f
      )      
     )                      
    (mover-aliens (cdr a))
   )
  ]
  ;mueve a la izquierda
  [(not (car (cddar a))) 
   (cons  
    (list 
     (- (caar a) veloz-aliens)
     (+ (cadar a) 0)
     (if (> (caar a) 20)
      (if (= 1 (random int))
       (+ 20 (random (- ancho 20)))
       #f
      )
      #t
     )
    )                  
    (mover-aliens (cdr a))
   )
  ]
 )  
)

;remover los aliens muertos
(define (remover-aliens enemigo bala)
 (cond
  [(empty? enemigo) empty]
  [(cons? enemigo) 
   (cond
    [(serca-aliens? (car enemigo) bala) (remover-aliens (cdr enemigo) bala)]
    [else (cons (car enemigo) (remover-aliens (cdr enemigo) bala))]
   )
  ]
 )
)

;descarta los aliens impactados por alguna bala
(define (serca-aliens? enemigo bala)
 (cond
  [(empty? bala) #f]
  [else (or (serca? enemigo (car bala)) (serca-aliens? enemigo (cdr bala)))]
 )   
)

;dice si la bala impactó o no contra el aliens
(define (serca? e d)
 (cond
  [(<= (distancia e d) rango-acierto) 
   (begin 
    (set! puntuacion (+ 10 puntuacion)) 
    (explosion) #t
   )]
  [else #f]
 )  
)

;calcula la distancia de las naves y las balas
(define (distancia p1 p2)
 (sqrt (+ (sqr (- (car p2) (car p1))) 
          (sqr (- (cadr p2) (cadr p1)))
       )
 )
)

;manejo con mouse
;datos-mundo   pos-x   pos-y tipo-de-evento -> (en la ventana)
(define (mouse m x y estado) 
 (cond
  ;lee posision horizontal del mouse
  [(string=? estado "move") 
   (make-mundo
    x 
    (mundo-disparo m) 
    (mundo-enemigos m)
   )
  ]
  ;lee clic
  [(string=? estado "button-down") 
   (make-mundo 
    (mundo-jugador m)                                             
    (agregar-bala x (mundo-disparo m))                                              
    (mundo-enemigos m))]
  ;devuelve datos (necesario)
  [else m]
 )
)
 
;manejo con el teclado
(define (teclado m tecla) 
 (cond
   ;lee flecha izquierda  
   [(and (equal? tecla "left") (> (mundo-jugador m) 20) )
   (make-mundo    
    (- (mundo-jugador m) veloz-nave) 
    (mundo-disparo m) 
    (mundo-enemigos m)
   )
  ]
  ;lee flecha derecha  
  [(and (equal? tecla "right")(< (mundo-jugador m) (- ancho 20)))
   (make-mundo    
    (+ (mundo-jugador m) veloz-nave) 
    (mundo-disparo m) 
    (mundo-enemigos m)
   )
  ]
  ;leer tecla espacio (para disparar)
  [(equal? tecla " ") 
    (make-mundo 
     (mundo-jugador m)                                             
     (agregar-bala (mundo-jugador m) (mundo-disparo m))                                              
     (mundo-enemigos m)
    )
  ]
  ;devuelve datos (necesario)
  [else m]
 )    
)

;agrega una bala en la lista de balas. 
(define (agregar-bala x viejas-balas)
 (begin
   (disparo)
   (cons 
  (list x (- alto 40)) 
  viejas-balas
 ))  
)  

;evaluo la posision vertical de los alien
;verificando que se encuentre en el rango de bajada,

(define (perdi? p)
 (perdi (mundo-enemigos p))
)

;compara las posiciones de los aliens
(define (perdi p)
(cond
  [(empty? p) #f      ]
  [(> (cadar p) (- alto 40)) #t]
  [#t (perdi (cdr p))]
 )  
)

;muestra imagen final
(define (dar-final f)
 (begin 
  (display "su nivel fue: ")
  (display nivel)
  (newline)
  (display "su puntaje fue: ")
  (display puntuacion)
  (newline)
  (set! fondo game-over-im)
  (comunicador f)
 )
)

;controla el mundo, el la encargada de enviar y recibir actualizaciones de 
;todas las funciones del juego
(define (iniciar-juego) 
 (big-bang 
  (make-mundo (/ ancho 2) empty enemigos)
  [on-tick monitoreo]
  [to-draw comunicador]
  [on-mouse mouse]
  [on-key teclado]
  [stop-when perdi? dar-final]
 )
)

;inici el juego
(iniciar-juego)
