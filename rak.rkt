(require 2htdp/image)

;;;;;;;;;;;;;;;;;; SI proj ;;;;;;;;;;;;;;;;;;
;;;              Yohei Yasukawa           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Global Defenitions
(define MAX_ALIENS 7)
(define MAX_HEALTH 2)
(define INIT_SPEED 5)
(define JOG_SPEED (+ INIT_SPEED 2))
(define SPRINT_SPEED (+ JOG_SPEED 2))

; Definitions for testing
(define mis-0-0 (make-posn 0 0))
(define mis-10-10 (make-posn 10 10))
(define mis-20-20 (make-posn 20 20))
(define mis-30-30 (make-posn 30 30))
(define mis-40-40 (make-posn 40 40))
(define mis-50-50 (make-posn 50 50))
(define mis-60-60 (make-posn 60 60))
(define mis-70-70 (make-posn 70 70))
(define mis-80-80 (make-posn 80 80))
(define mis-90-90 (make-posn 90 90))
(define mis-100-100 (make-posn 100 100))

; Definitions for Init
(define BG_WIDTH 500)
(define BG_HEIGHT 500)
;(define EMPTY_SCENE (empty-scene BG_WIDTH BG_HEIGHT))
(define BG_BLANK (rectangle BG_WIDTH BG_HEIGHT "outline" "blue"))

;;;;;;;;;;;;;;;;
;;; Defender ;;;
;;;;;;;;;;;;;;;;

; Definitions for Defender
(define DEF_IMG .) ; 50x40 pixels
(define DEF_SPEED 10) ; how fast a defendder moves to right and left.
(define DEF_POSX 250)
(define DEF_POSY 450)
(define L_EDGE (/ (image-width DEF_IMG) 2))
(define R_EDGE (- BG_WIDTH (/ (image-width DEF_IMG) 2)))
(define defender-init DEF_POSX)


;; defender-key: Defender Key -> Defender
; calculates the state following the given state if given key is pressed
(define (defender-key current key)
  (cond
    [(string=? key "left") (move-left current)]
    [(string=? key "right") (move-right current)]
    [else current]
    ))
(check-expect (defender-key 100 "left") (- 100 DEF_SPEED))
(check-expect (defender-key 100 "right") (+ 100 DEF_SPEED))

;; touch-left-wall? : Posn -> boolean
; determine if a given position is touching a wall on the left
(define (touch-left-wall? current) 
  (<= current L_EDGE)
  )
(check-expect (touch-left-wall? L_EDGE) true)
(check-expect (touch-left-wall? 200) false)


;; touch-right-wall? : Posn -> boolean
; determine if a given position is touching a wall on the right
(define (touch-right-wall? current) 
  (cond
    [(>= current R_EDGE) true]
    [else false]
    ))
(check-expect (touch-right-wall? 10) false)
(check-expect (touch-right-wall? R_EDGE) true)


;; move-left : Defender -> Defender
; move a given defender to the left in 1 px
(define (move-left current)
  (if (touch-left-wall? current)
      current
      (- current DEF_SPEED) 
      ))
(check-expect (move-left 200) (- 200 DEF_SPEED))
(check-expect (move-left L_EDGE) L_EDGE)


;; move-right : Defender -> Defender
; move a given object to the right in 1 px
(define (move-right current)
  (if (touch-right-wall? current)
      current
      (+ current DEF_SPEED)
      ))
(check-expect (move-right 10) (+ 10 DEF_SPEED))
(check-expect (move-right R_EDGE) R_EDGE)

; defender-render : Defender -> image
; constructs an image representing the given state
(define (defender-render current)
  (place-image DEF_IMG
               current
               DEF_POSY
               BG_BLANK))


;;;;;;;;;;;;;;;;
;;; Barrage  ;;;
;;;;;;;;;;;;;;;;

; Definitions for Barrage
(define DEFAULT_X 0)
(define DEFAULT_Y 0)
(define MISSILE_IMG .)
(define MISSILE_SPEED 5)
(define T_EDGE 0)
(define EMPTY_MISSILE .) ; 10x20 pixels
(define barrage-init empty)
(define missile-init (make-posn 100 100))

;; missile-tick: Missile -> Missile
; calculates the state following the given state if only time passes
(define (missile-tick current)
  (move-up current))

;; barrage-tick: Barrage -> Barrage
; calculates the state following the given state if only time passes
(define (barrage-tick barrage)
  (cond
    [(empty? barrage) empty]
    [(cons? barrage) (barrage-filter-offscreen barrage)]
    ))
;; barrage-filter-offscreen : Barrage -> Barrage
; filters out a offscreen missile from a given barrage if it exists
(define (barrage-filter-offscreen barrage)
  (if (touch-top-wall? (first barrage))
      (barrage-tick (rest barrage)) ; remove a needless missile.
      (cons-barrage-tick barrage))
  )
(define (cons-barrage-tick barrage)
  (cons
   (missile-tick (first barrage))
   (barrage-tick (rest barrage))
   ))
(check-expect (barrage-tick (cons mis-10-10 empty))
              (cons (make-posn 10 (- 10 MISSILE_SPEED)) empty))
(check-expect (barrage-tick (cons mis-0-0 empty)) empty)

;; touch-top-wall? : Missile -> boolean
; determine if a given missile is touching a wall on the top.
(define (touch-top-wall? posn) 
  (<= (posn-y posn) T_EDGE))
(check-expect (touch-top-wall? (make-posn 0 T_EDGE)) true)
(check-expect (touch-top-wall? (make-posn 10 10)) false)

;; move-up : Missile -> Missile
; move a given object to the top in 1 px
(define (move-up current)
  (if (touch-top-wall? current)
      current
      (make-posn (posn-x current)
                 (- (posn-y current) MISSILE_SPEED))
      ))
(check-expect (move-up (make-posn 0 10)) (make-posn 0 (- 10 MISSILE_SPEED)))
(check-expect (move-up (make-posn 0 T_EDGE)) (make-posn 0 T_EDGE))


; barrage-render : List of Missile -> image
; constructs an image representing the given state
(define (barrage-render barrage)
  (cond
    [(empty? barrage) BG_BLANK]
    [(cons? barrage) (cons-barrage-render barrage)]
    ))
(define (cons-barrage-render barrage)
  (overlay (place-image MISSILE_IMG
                        (posn-x (first barrage))
                        (posn-y (first barrage))
                        BG_BLANK)
           (barrage-render (rest barrage))
           ))


;;;;;;;;;;;;;
;;; Alien ;;;
;;;;;;;;;;;;;

; AlienLoc is a posn
; the direction the Alien will move next
; Ex:
(make-posn 100 200)

; AlienDir is a srting, one of
; - "left" 
; - "right"
; - "down"
; Ex:
"left"

; Speed is a number that shows how fast a alien moves in a tick.
; Ex:
1
5
10

; regular-alien is a strucure containing
; - location (AlienLoc)
; - direction (AlienDir)
; - speed
(define-struct regular-alien
  (loc dir speed))
(define ra-100-100-l (make-regular-alien (make-posn 100 100) "left" INIT_SPEED))
ra-100-100-l

; diver-alien is a structure containing
; - location (AlienLoc)
; - direction (AlienDir)
; - diving? (boolean)
(define-struct diver-alien
  (loc dir diving? speed))
(define da-200-100-r (make-diver-alien (make-posn 200 100) "right" true INIT_SPEED))
da-200-100-r

; shielded-alien is a structure containing
; - location (AlienLoc)
; - direction (AlienDir)
; - health (integer 0-2; amount of shield remaining)
(define-struct shielded-alien
  (loc dir health speed))
(define sa-300-100-d (make-shielded-alien (make-posn 300 100) "down" 2 1))
sa-300-100-d


; Aliens is a List (of Alien)
; the Aliens in a particular game state
(define one-line-aliens 
  (list ra-100-100-l da-200-100-r sa-300-100-d))
one-line-aliens

;; Definitions for Aliens
(define RA_IMG .) ; 20x20 pixels
(define DA_IMG .) ; 20x20 pixels
(define SA_IMG .) ; 20x20 pixels
(define X_INTERVAL 45) ; the space between aliens or between alien and wall in x axis
(define Y_INTERVAL 10) ; the space between aliens or between alien and wall in y axis
(define ALIEN_WIDTH 20)
(define ALIEN_HEIGHT 20)
(define DEF_DIR "right")
(define DIVE_LOC (make-posn 250 200))
(define da-1st (make-diver-alien (make-posn (+ (* ALIEN_WIDTH 1) (* X_INTERVAL 1))
                                            Y_INTERVAL)
                                 DEF_DIR false INIT_SPEED))
(define ra-2nd (make-regular-alien (make-posn (+ (* ALIEN_WIDTH 2) (* X_INTERVAL 2)) 
                                              Y_INTERVAL)
                                   DEF_DIR INIT_SPEED))
(define sa-3rd (make-shielded-alien (make-posn (+ (* ALIEN_WIDTH 3) (* X_INTERVAL 3))
                                               Y_INTERVAL)
                                    DEF_DIR MAX_HEALTH INIT_SPEED))
(define da-4th (make-diver-alien (make-posn (+ (* ALIEN_WIDTH 4) (* X_INTERVAL 4))
                                            Y_INTERVAL)
                                 DEF_DIR false INIT_SPEED))
(define ra-5th (make-regular-alien (make-posn (+ (* ALIEN_WIDTH 5) (* X_INTERVAL 5))
                                              Y_INTERVAL)
                                   DEF_DIR INIT_SPEED))
(define sa-6th (make-shielded-alien (make-posn (+ (* ALIEN_WIDTH 6) (* X_INTERVAL 6))
                                               Y_INTERVAL)
                                    DEF_DIR MAX_HEALTH INIT_SPEED))
(define ra-7th (make-regular-alien (make-posn (+ (* ALIEN_WIDTH 7) (* X_INTERVAL 7))
                                              Y_INTERVAL)
                                   DEF_DIR INIT_SPEED))
(define 1st-aliens-line
  (list da-1st ra-2nd sa-3rd da-4th ra-5th sa-6th ra-7th))
(define aliens-init 1st-aliens-line)


;; get-alien-loc : Alien -> AlienLoc
; get a location info from a given alien.
(define (get-alien-loc alien)
  (cond
    [(regular-alien? alien) (regular-alien-loc alien)]
    [(diver-alien? alien) (diver-alien-loc alien)]
    [(shielded-alien? alien) (shielded-alien-loc alien)]
    ))
(check-expect (get-alien-loc ra-100-100-l) (make-posn 100 100))
(check-expect (get-alien-loc da-200-100-r) (make-posn 200 100))

;; get-alien-dir : Alien -> AlienDir
; get a direction info from a given alien.
(define (get-alien-dir alien)
  (cond
    [(regular-alien? alien) (regular-alien-dir alien)]
    [(diver-alien? alien) (diver-alien-dir alien)]
    [(shielded-alien? alien) (shielded-alien-dir alien)]
    ))
(check-expect (get-alien-dir ra-100-100-l) "left")
(check-expect (get-alien-dir da-200-100-r) "right")

;; get-alien-speed : Alien -> Speed
; get a speed info from a given alien.
(define (get-alien-speed alien)
  (cond
    [(regular-alien? alien) (regular-alien-speed alien)]
    [(diver-alien? alien) (diver-alien-speed alien)]
    [(shielded-alien? alien) (shielded-alien-speed alien)]
    ))
(check-expect (get-alien-speed ra-100-100-l) INIT_SPEED)
(check-expect (get-alien-speed da-200-100-r) INIT_SPEED)

;; alien-next-loc : AlienLoc AlienDir -> AlienLoc
; calculate the next alien location by a given location and direction.
(define (alien-next-loc loc dir speed)
  (cond
    [(string=? dir "left") (make-posn (- (posn-x loc) speed)
                                      (posn-y loc))]
    [(string=? dir "right") (make-posn (+ (posn-x loc) speed)
                                       (posn-y loc))]
    [(string=? dir "down") (make-posn (posn-x loc)
                                      (+ (posn-y loc) ALIEN_HEIGHT Y_INTERVAL))]
    ))
(check-expect (alien-next-loc (make-posn 10 10) "right" 1)
              (make-posn (+ 10 1) 10))
(check-expect (alien-next-loc (make-posn 100 100) "left" 1)
              (make-posn (- 100 1) 100))
(check-expect (alien-next-loc (make-posn 10 10) "down" 1)
              (make-posn 10 (+ 10 ALIEN_HEIGHT Y_INTERVAL)))

;; alien-next-dir : AlienLoc AlienDir -> AlienDir
; calculates the next direction of alien by a given location and direction.
(define (alien-next-dir loc dir)
    (cond
    [(string=? dir "left") (if (adjacent-left-wall? loc)
                               "down"
                               "left")]
    [(string=? dir "right") (if (adjacent-right-wall? loc)
                                "down"
                                "right")]
    [(string=? dir "down")
     (cond
       [(adjacent-left-wall? loc) "right"]
       [(adjacent-right-wall? loc) "left"]
       [else "down"] ; may be executed by diver aliens
       )]
    ))
(check-expect (alien-next-dir (make-posn 0 0) "left") "down")
(check-expect (alien-next-dir (make-posn 100 0) "left") "left")
(check-expect (alien-next-dir (make-posn 0 0) "down") "right")
(check-expect (alien-next-dir (make-posn R_EDGE 0) "right") "down")
(check-expect (alien-next-dir (make-posn (- BG_WIDTH 100) 0) "right") "right")
(check-expect (alien-next-dir (make-posn R_EDGE 0) "down") "left")
(check-expect (alien-next-dir (make-posn 250 250) "down") "down")

;; adjacent-left-wall? : AlienLoc -> boolean
; determine if an alien on a given location is adjacent to the left wall.
(define (adjacent-left-wall? loc)
  (touch-left-wall? (posn-x (alien-next-loc loc "left" 1))))
(check-expect (adjacent-left-wall? (make-posn 0 0)) true)
(check-expect (adjacent-left-wall? (make-posn R_EDGE 0)) false)

;; adjacent-right-wall? : AlienLoc -> boolean
; determine if an alien on a given location is adjacent to the right wall.
(define (adjacent-right-wall? loc)
  (touch-right-wall? (posn-x (alien-next-loc loc "right" 1))))
(check-expect (adjacent-right-wall? (make-posn 0 0)) false)
(check-expect (adjacent-right-wall? (make-posn R_EDGE 0)) true)


;; aliens-tick : Aliens -> Aliens
; calculates the state following the given state if only time passes
(define (aliens-tick aliens)
  (cond
    [(empty? aliens) empty]
    [(cons? aliens) (cons-aliens-tick aliens)]
    ))
(define (cons-aliens-tick aliens)
  (cond
    [(regular-alien? (first aliens)) (cons (move-regular-alien (first aliens))
                                          (aliens-tick (rest aliens)))]
    [(diver-alien? (first aliens)) (cons (move-diver-alien (first aliens))
                                        (aliens-tick (rest aliens)))]
    [(shielded-alien? (first aliens)) (cons (move-shielded-alien (first aliens))
                                           (aliens-tick (rest aliens)))]
    ))

;; move-regular-alien : RegularAlien -> RegularAlien
; calculate the next regular alien state by a given regular alien state
(define (move-regular-alien ra)
  (make-regular-alien (alien-next-loc (regular-alien-loc ra)
                                      (regular-alien-dir ra)
                                      (regular-alien-speed ra))
                      (alien-next-dir (regular-alien-loc ra)
                                      (regular-alien-dir ra))
                      (regular-alien-speed ra)))
(check-expect (move-regular-alien (make-regular-alien (make-posn 0 0)
                                                         "right" 1))
              (make-regular-alien (make-posn 1 0)
                                  "right" 1))


;; move-diver-alien : DiverAlien -> DiverAlien
; move a given diver alien to the next location
(define (move-diver-alien da)
  (if (diver-alien-diving? da)
      (dived-diver-alien da)
      (moved-diver-alien da)
      ))
(check-expect (move-diver-alien (make-diver-alien (make-posn 0 0) "right" false 1))
              (make-diver-alien (make-posn 1 0) "right" false 1))
(check-expect (move-diver-alien (make-diver-alien (make-posn 0 0) "right" true 1))
              (make-diver-alien (make-posn 0 1) "down" true 1))

;; dived-diver-alien : DiverAlien -> DiverAlien
; calculate a dived location of a given diver alien.
(define (dived-diver-alien da)
  (make-diver-alien (make-posn (posn-x (diver-alien-loc da))
                               (+ (posn-y (diver-alien-loc da))
                                  (diver-alien-speed da)))
                    "down" true (diver-alien-speed da)))
(check-expect (dived-diver-alien (make-diver-alien (make-posn 0 0)
                                                   "right" true 1))
              (make-diver-alien (make-posn 0 1)
                                "down" true 1))

;; moved-diver-alien : DiverAlien -> DiverAlien
; calculate a moved location of a given diver alien.
(define (moved-diver-alien da)
  (cond
    [(over-line? (diver-alien-loc da) DIVE_LOC)
     (make-diver-alien (alien-next-loc (diver-alien-loc da)
                                       (diver-alien-dir da)
                                       (diver-alien-speed da))
                       "down"
                       true
                       (diver-alien-speed da))]
    [else (random-diving da)]
    ))
(check-expect (moved-diver-alien (make-diver-alien (make-posn 0 0)
                                                  "right" false INIT_SPEED))
              (make-diver-alien (make-posn INIT_SPEED 0)
                                "right" false INIT_SPEED))
(check-expect (moved-diver-alien (make-diver-alien (make-posn 300 300)
                                                  "right" false INIT_SPEED))
              (make-diver-alien (make-posn (+ 300 INIT_SPEED) 300)
                                "down" true INIT_SPEED))

(define (random-diving da)
  (if (= (random 100) 1) ; NOTE: might cause check-expect error due to random output
      (make-diver-alien (alien-next-loc (diver-alien-loc da)
                                        (diver-alien-dir da)
                                        (diver-alien-speed da))
                        (alien-next-dir (diver-alien-loc da)
                                        (diver-alien-dir da))
                        true ; random diving
                        (diver-alien-speed da)
                        )
      (make-diver-alien (alien-next-loc (diver-alien-loc da)
                                        (diver-alien-dir da)
                                        (diver-alien-speed da))
                        (alien-next-dir (diver-alien-loc da)
                                        (diver-alien-dir da))
                        (diver-alien-diving? da)
                        (diver-alien-speed da)
                        )
      ))

;; over-line? : Posn Posn -> boolean
; determines if a first given position is over the other position line.
(define (over-line? a b)
  (and (>= (posn-x a) (posn-x b))
       (>= (posn-y a) (posn-y b))
       ))
(check-expect (over-line? (make-posn 0 0) (make-posn 1 1)) false)
(check-expect (over-line? (make-posn 0 0) (make-posn 0 0)) true)

;; move-shielded-alien : shieldedAlien -> shieldedAlien
; calculate the next regular alien state by a given regular alien state
(define (move-shielded-alien sa)
  (make-shielded-alien (alien-next-loc (shielded-alien-loc sa)
                                       (shielded-alien-dir sa)
                                       (shielded-alien-speed sa))
                       (alien-next-dir (shielded-alien-loc sa)
                                       (shielded-alien-dir sa))
                       (shielded-alien-health sa)
                       (shielded-alien-speed sa)
                       ))
(check-expect (move-shielded-alien (make-shielded-alien (make-posn 0 0)
                                                        "right" MAX_HEALTH
                                                        INIT_SPEED))
              (make-shielded-alien (make-posn INIT_SPEED 0)
                                   "right" MAX_HEALTH INIT_SPEED))


;; aliens-render : Aliens -> image
; constructs an image representing the given state
(define (aliens-render aliens)
  (cond
    [(empty? aliens) BG_BLANK]
    [(cons? aliens) (cons-aliens-render aliens)]
    ))

(define (cons-aliens-render aliens)
  (cond
    [(regular-alien? (first aliens))
     (overlay (put-image (regular-alien-loc (first aliens))
                         RA_IMG)
              (aliens-render (rest aliens)))]
    [(diver-alien? (first aliens))
     (overlay (put-image (diver-alien-loc (first aliens))
                         DA_IMG)
              (aliens-render (rest aliens)))]
    [(shielded-alien? (first aliens))
     (overlay (put-image (shielded-alien-loc (first aliens))
                         SA_IMG)
              (aliens-render (rest aliens)))]
    ))

;; put-image : location image -> image
; put a given image on a given location
(define (put-image loc image)
  (place-image image
               (posn-x loc) (posn-y loc)
               BG_BLANK))
(check-expect (put-image (make-posn 10 10) RA_IMG)
              .)





;;;;;;;;;;;;
;;; Game ;;;
;;;;;;;;;;;;

; Score is a number that shows how many points you get so far.
; Ex:
100
10000

; Definitions for Game
(define-struct game (defender aliens barrage round score prev-score state))
(define game-init (make-game defender-init aliens-init barrage-init 1 0 0 "start"))
(define FTSZ 16)
(define FTCL "black")

; game-render : Game -> image
; constructs an image representing the given state
(define (game-render game)
  (overlay (barrage-render (game-barrage game))
           (aliens-render (game-aliens game))
           (defender-render (game-defender game))
           (score-render (game-score game))
           ))

;; score-render : Score -> image
; constructs an image representing the given state
(define (score-render score)
  (place-image (text (string-append "Score: " (number->string score))
                     FTSZ FTCL)
               (- BG_WIDTH 50) 30
               BG_BLANK))
  
;; game-tick : Game -> Game
; calculates the state following the given state if only time passes
(define (game-tick game)
  (make-game
   (game-defender game)
   (aliens-tick (speedup-aliens (remove-alien-hit-by-missile (game-aliens game)
                                             (game-barrage game))))
   (barrage-tick (consume-missile-hit-to-alien (game-barrage game) 
                                              (game-aliens game)))
   (game-round game)
   (score-update game)
   (game-prev-score game)
   (game-state game)
   ))

;; score-update : Game-> Score
; claculate a score by a given game state
(define (score-update game)
  (+ (game-prev-score game)
     (* (- MAX_ALIENS (alien-remains (game-aliens game))) 
        100)))

;; speedup-aliens : Main -> Main
; increase a speed of each alien by a given state
(define (speedup-aliens aliens)
  (cond
    [(= (alien-remains aliens) 4) (set-aliens-speed aliens JOG_SPEED)]
    [(= (alien-remains aliens) 1) (set-aliens-speed aliens SPRINT_SPEED)]
    [else aliens]
    ))
(check-expect (speedup-aliens aliens-init) aliens-init)
(check-expect (speedup-aliens (list ra-100-100-l))
              (list (make-regular-alien (make-posn 100 100)
                                  "left"
                                  SPRINT_SPEED)))

;; alien-remains : Aliens -> Number
; count a living alien in given aliens
(define (alien-remains aliens)
  (cond
    [(empty? aliens) 0]
    [(cons? aliens) (cons-alien-remains aliens)]
    ))
(define (cons-alien-remains aliens)
  (+ 1 (alien-remains (rest aliens))))
(check-expect (alien-remains empty) 0)
(check-expect (alien-remains aliens-init) 7)

;; set-aliens-speed : Aliens Speed -> Main
; set given aliens' speed to the given speed
(define (set-aliens-speed aliens speed)
  (cond
    [(empty? aliens) empty]
    [(cons? aliens) (cons-set-aliens-speed aliens speed)]
    ))
(define (cons-set-aliens-speed aliens speed)
  (cond
    [(regular-alien? (first aliens))
     (cons (set-ra-speed (first aliens) speed)
           (set-aliens-speed (rest aliens) speed))]
    [(diver-alien? (first aliens))
     (cons (set-da-speed (first aliens) speed)
           (set-aliens-speed (rest aliens) speed))]
    [(shielded-alien? (first aliens))
     (cons (set-sa-speed (first aliens) speed)
           (set-aliens-speed (rest aliens) speed))]
    ))
(check-expect (set-aliens-speed empty JOG_SPEED) empty)
(check-expect (set-aliens-speed (list ra-100-100-l) JOG_SPEED)
              (list (make-regular-alien (make-posn 100 100)
                                  "left"
                                  JOG_SPEED)))

;; set-ra-speed : RegularAlien Speed -> RegularAlien
; set a given regular alien's speed to the given speed
(define (set-ra-speed ra speed)
  (make-regular-alien (regular-alien-loc ra)
                      (regular-alien-dir ra)
                      speed))
(check-expect (set-ra-speed ra-100-100-l JOG_SPEED)
              (make-regular-alien (make-posn 100 100)
                                  "left"
                                  JOG_SPEED))
;; set-da-speed : DiverAlien Speed -> DiverAlien
; set a given diver alien's speed to the given speed
(define (set-da-speed da speed)
  (make-diver-alien (diver-alien-loc da)
                    (diver-alien-dir da)
                    (diver-alien-diving? da)
                    speed))
(check-expect (set-da-speed da-200-100-r JOG_SPEED)
              (make-diver-alien (make-posn 200 100)
                                "right"
                                true
                                JOG_SPEED))

;set-sa-speed : ShieldedAlien Speed -> ShieldedAlien
; set a given shielded alien's speed to the given speed
(define (set-sa-speed sa speed)
  (make-shielded-alien (shielded-alien-loc sa)
                       (shielded-alien-dir sa)
                       (shielded-alien-health sa)
                       speed))
(check-expect (set-sa-speed sa-300-100-d INIT_SPEED)
              (make-shielded-alien (make-posn 300 100)
                                   "down"
                                   MAX_HEALTH
                                   INIT_SPEED))

;; remove-alien-hit-by-missile : Aliens Barrage -> Aliens
; remove an alien hit by a misile in a given barrage from a given aliens.
(define (remove-alien-hit-by-missile aliens barrage)
  (cond
    [(empty? aliens) empty]
    [(cons? aliens) (cons-remove-alien-hit-by-missile aliens barrage)]
    ))
(define (cons-remove-alien-hit-by-missile aliens barrage)
  (if (alien-hit-by-barrage? (first aliens) barrage)
      (damage-aliens aliens barrage)
      (cons (first aliens) (remove-alien-hit-by-missile (rest aliens) barrage))
      ))
(check-expect (remove-alien-hit-by-missile empty empty) empty)
(check-expect (remove-alien-hit-by-missile (cons ra-100-100-l (cons da-200-100-r empty))
                                           (cons (make-posn 0 0)
                                                 (cons (make-posn 100 100) empty)))
              (cons da-200-100-r empty))

;; damage-aliens : Aliens Barrage -> Aliens
; damage a first alien in given aliens (decrement health once)
(define (damage-aliens aliens barrage)
  (if (and (shielded-alien? (first aliens))
           (> (shielded-alien-health (first aliens)) 0)) ; never evaluated if other type of aliens.
      (cons (decrement-health (first aliens)) 
            (remove-alien-hit-by-missile (rest aliens) barrage))
      (remove-alien-hit-by-missile (rest aliens) barrage)))
(check-expect (damage-aliens (cons ra-100-100-l empty) empty) empty)
(check-expect (damage-aliens (cons (make-shielded-alien (make-posn 0 0)
                                                        "down" 2 INIT_SPEED) empty) empty)
              (cons (make-shielded-alien (make-posn 0 0) "down" 1 INIT_SPEED) empty))
                                     
                                     

;; decrement-health : Alien -> Alien
; decrement health of a first alien of given aliens.
; If the alien has no health or 0 health remained, the alien died (removed).
(define (decrement-health alien)
  (make-shielded-alien (shielded-alien-loc alien)
                       (shielded-alien-dir alien)
                       (- (shielded-alien-health alien) 1)
                       (shielded-alien-speed alien)
                       ))
(check-expect (decrement-health (make-shielded-alien (make-posn 0 0) 
                                                     "down" 2 INIT_SPEED))
              (make-shielded-alien (make-posn 0 0) "down" 1 INIT_SPEED))

;; consume-missile-hit-to-alien : Barrage Aliens -> Barrage
; consume a missile hit to an alien from a given barrage.
(define (consume-missile-hit-to-alien barrage aliens)
  (cond
    [(empty? barrage) empty]
    [(cons? barrage) (cons-consume-missile-hit-to-alien barrage aliens)]
    ))
(define (cons-consume-missile-hit-to-alien barrage aliens)
  (if (missile-hits-aliens? (first barrage) aliens)
      (consume-missile-hit-to-alien (rest barrage) aliens)
      (cons (first barrage) (consume-missile-hit-to-alien (rest barrage) aliens))
      ))
(check-expect (consume-missile-hit-to-alien empty empty) empty)
(check-expect (consume-missile-hit-to-alien (cons (make-posn 100 100) empty)
                                           (cons ra-100-100-l empty))
              empty)

;; missile-hits-aliens? : Missile Aliens -> boolean
; determine if a given missile hits one of given aliens.
(define (missile-hits-aliens? missile aliens)
  (cond
    [(empty? aliens) false]
    [(cons? aliens) (cons-missile-hits-aliens? missile aliens)]
    ))
(define (cons-missile-hits-aliens? missile aliens)
  (or (missile-hits-alien? missile (first aliens))
      (missile-hits-aliens? missile (rest aliens))
      ))
(check-expect (missile-hits-aliens? (make-posn 100 100) empty) false)
(check-expect (missile-hits-aliens? (make-posn 100 100) 
                                    (cons ra-100-100-l (cons ra-100-100-l empty)))
              true)



;; alien-hit-by-barrage? : Alien Barrage -> boolean
; determine if missiles in a given barrage hit a given alien.
(define (alien-hit-by-barrage? alien barrage)
  (cond
    [(empty? barrage) false]
    [(cons? barrage) (cons-alien-hit-by-barrage? alien barrage)]
    ))
(define (cons-alien-hit-by-barrage? alien barrage)
  (or (missile-hits-alien? (first barrage) alien)
      (alien-hit-by-barrage? alien (rest barrage))
      ))
(check-expect (alien-hit-by-barrage? ra-100-100-l empty) false)
(check-expect (alien-hit-by-barrage? ra-100-100-l
                                     (cons (make-posn 100 100) empty))
              true)



;; missile-hits-alien? : Missile Alien -> boolean
; determine if a given missile hits a given alien.
(define (missile-hits-alien? missile alien)
  (cond
    [(regular-alien? alien) (missile-hits-around-aloc? missile
                                                      (regular-alien-loc alien))]
    [(diver-alien? alien) (missile-hits-around-aloc? missile
                                                    (diver-alien-loc alien))]
    [(shielded-alien? alien) (missile-hits-around-aloc? missile
                                                       (shielded-alien-loc alien))]
    ))
(check-expect (missile-hits-alien? (make-posn 100 100) ra-100-100-l) true)
(check-expect (missile-hits-alien? (make-posn 0 0) ra-100-100-l) false)
                                   
              

;; missile-hits-around-aloc? : Missile AlienLoc -> Boolean
; determine if a given missile hits an area (20x20 square) from a given location.
(define (missile-hits-around-aloc? missile loc)
  (and (<= (posn-x missile) (+ (posn-x loc) 10))
       (>= (posn-x missile) (- (posn-x loc) 10))
       (<= (posn-y missile) (+ (posn-y loc) 10))
       (>= (posn-y missile) (- (posn-y loc) 10))
       ))
(check-expect (missile-hits-around-aloc? (make-posn 0 0) (make-posn 0 0)) true)
(check-expect (missile-hits-around-aloc? (make-posn 0 0) (make-posn 50 50)) false)


;; game-key: Game Key -> Game
; calculates the state following the given state if given key is pressed
(define (game-key game key)
  (cond
    [(string=? key " ") (make-game
                             (defender-key (game-defender game) key)
                             (game-aliens game)
                             (new-barrage (game-barrage game)
                                          (game-defender game))
                             (game-round game)
                             (game-score game)
                             (game-prev-score game)
                             (game-state game))]
    [else (make-game
           (defender-key (game-defender game) key)
           (game-aliens game)
           (game-barrage game)
           (game-round game)
           (game-score game)
           (game-prev-score game)
           (game-state game))]
    ))

;; new-barrage: Barrage Defender -> Barrage (List of Missile)
; create a new missile by given defender status,
; and add it to a given barrage.
(define (new-barrage barrage defender)
  (cons (new-missile defender) barrage))
(check-expect (new-barrage barrage-init defender-init)
              (cons (make-posn DEF_POSX DEF_POSY) barrage-init))
                           
;; new-missile: Defender -> Missile
; create a new missile 
(define (new-missile defender)
  (make-posn defender DEF_POSY))
(check-expect (new-missile 50)
              (make-posn 50 DEF_POSY))

; Game functions
(define (game-start current)
  (big-bang current
            (on-tick game-tick)
            (on-key game-key)
            (to-draw game-render)
            ))
;(game-start game-init)



;;;;;;;;;;;;
;;; Main ;;;
;;;;;;;;;;;;

;; Definitions for Main
(define main-init game-init)
(define START_IMG .)
(define WIN_IMG .) ; 500x500 pixels
(define LOSE_IMG .) ; 500x500 pixels
(define DEAD_LINE (make-posn 0 450))

;; main-tick : Main -> Main (Game)
; calculates the state following the given state if only time passes
(define (main-tick main)
  (cond ; game state should be evaluated earlier than other conditions
     [(string=? (game-state main) "win") (create-new-game-state main)]
     [(string=? (game-state main) "lose") (set-game-state main-init "lose")]
    [(wiped-out-aliens? (game-aliens main)) (set-game-state main "win")]
    [(invaded-by-aliens? (game-aliens main)) (set-game-state main "lose")]
    [else (game-tick main)]
    ))

;; wiped-out-aliens? : Aliens -> boolean
; determine if a given state wiped out aliens.
(define (wiped-out-aliens? aliens)
  (empty? aliens))
(check-expect (wiped-out-aliens? empty) true)
(check-expect (wiped-out-aliens? (cons ra-100-100-l empty)) false)

;; invaded-by-aliens? : Aliens -> boolean
; determine if any of aliens invaded a dead line.
(define (invaded-by-aliens? aliens)
  (cond
    [(empty? aliens) false]
    [(cons? aliens) (cons-invaded-by-aliens? aliens)]
    ))
(define (cons-invaded-by-aliens? aliens)
  (or (over-line? (get-alien-loc (first aliens)) DEAD_LINE)
      (invaded-by-aliens? (rest aliens))))
(check-expect (invaded-by-aliens? empty) false)
(check-expect (invaded-by-aliens? (cons (make-regular-alien
                                         DEAD_LINE "down" INIT_SPEED) empty))
              true)
(check-expect (invaded-by-aliens? (list ra-100-100-l da-200-100-r sa-300-100-d))
              false)
(check-expect (invaded-by-aliens? aliens-init) false)

;; create-new-game-state : Game -> Game
; create a new game state with taking some of given states
(define (create-new-game-state game)
  (make-game
   defender-init aliens-init barrage-init
   (game-round game)
   (game-score game)
   (game-score game) ; save a score so far.
   (game-state game)))
   
;; main-key : Main Key -> Main (Game)
; calculates the state following the given state if given key is pressed
(define (main-key main key)
  (cond
    [(or (string=? (game-state main) "start")
         (string=? (game-state main) "win")
         (string=? (game-state main) "lose"))
     (start-key main key)]
    [else (game-key main key)]
    ))
(check-expect (main-key main-init "a") main-init)

;; start-key : Main Key -> Main (Game)
; calculates the state following the given state if given key is pressed
(define (start-key main key)
  (cond
    [(string=? key "n") (set-game-state main "playing")]
    [else main]
    ))
(check-expect (start-key main-init "m") main-init)

;; set-game-state : Game State -> Main (Game)
; set a given state to the given game state.
(define (set-game-state game state)
  (make-game
   (game-defender game) 
   (game-aliens game)
   (game-barrage game)
   (game-round game)
   (game-score game)
   (game-prev-score game)
   state))
(check-expect (set-game-state main-init "start") main-init)

;; main-render : Main -> image
; constructs an image representing the given state
(define (main-render main)
  (cond
    [(string=? (game-state main) "start") START_IMG]
    [(string=? (game-state main) "win") WIN_IMG]
    [(string=? (game-state main) "lose") LOSE_IMG]
    [else (game-render main)]
    ))

;; put-result-image : Main -> image
; put a result image by a given main state.
(define (put-result-image main)
  (cond
    [(string=? main "win") WIN_IMG] 
    [(string=? main "lose") LOSE_IMG]
    [else (error "put-result-image")]
    ))
(check-expect (put-result-image "win") WIN_IMG)
(check-expect (put-result-image "lose") LOSE_IMG)
(check-error (put-result-image "foobar") "put-result-image")

; Main functions
(define (main-start current)
  (big-bang current
            (on-tick main-tick)
            (on-key main-key)
            (to-draw main-render)
            ))
(main-start main-init)



