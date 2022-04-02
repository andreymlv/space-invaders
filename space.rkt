;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Space Invaders Clone

;; =================
;; Constants:
;; =================

(define ENEMY-IMAGE (bitmap "images/enemy.png"))
(define HERO-IMAGE (bitmap "images/enemy.png"))
(define PIXEL-IMAGE (square 1 "solid" "green"))
(define ENEMY-PROJECTILE-IMAGE (bitmap "images/enemy-projectile.png"))
(define HERO-PROJECTILE-IMAGE (bitmap "images/hero-projectile.png"))
(define WIDTH 256)
(define HEIGHT 256)
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "black"))
(define HERO-SPEED 4)
(define PROJECTILE-SPEED 2)
(define SHIELD-SIZE 32)

;; Data definitions:

;; EnemyDirection is one of:
;;  - "right"
;;  - "left"
;; interp. the direction of the enemy
#;
(define (fn-for-enemy-direction ed)
  (cond [(string=? "right" ed) (...)]
        [(string=? "left" ed) (...)]))
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: "right"
;;  - atomic distinct: "left"

;; GameState is one of:
;;  - "running"
;;  - "defeat"
;;  - "victory"
;; interp. current state of the game
#;
(define (fn-for-game-state gs)
  (cond [(string=? "running" gs) (...)]
        [(string=? "defeat" gs) (...)]
        [(string=? "victory" gs) (...)]))
;; Template rules used:
;;  - one of: 3 cases
;;  - atomic distinct: "running"
;;  - atomic distinct: "defeat"
;;  - atomic distinct: "victory"

(define-struct enemy (position speed direction))
;; Enemy is (make-enemy posn Number EnemyDirection)
;; interp. a enemy at position x, y with speed and direction

(define ENEMY-1 (make-enemy (make-posn 10 10) 2 "right"))

#;
(define (fn-for-enemy e)
  (... (fn-for-posn (enemy-position e))                 ; posn
       (enemy-speed e)                                  ; Number
       (fn-for-enemy-direction (enemy-direction e))))   ; EnemyDirection
;; Template rules used:
;;  - compound: 4 fields
;;  - reference: position field is posn
;;  - reference: direction field is EnemyDirection

(define-struct stat (points state))
;; Statistics is (make-stat Number GameState)
;; interp. a stats for current game

(define STAT-1 (make-stat 6 "running"))

#;
(define (fn-for-stat s)
  (... (stat-points s)                        ; Number
       (fn-for-game-state (stat-state s))))   ; GameState
;; Template rules used:
;;  - compound: 2 fields
;;  - reference: state field is GameState

(define-struct pixel (position))
;; Pixel is (make-pixel posn)
;; interp. a pixel at position x, y

(define PIXEL-1 (make-pixel (make-posn 12 142)))

#;
(define (fn-for-pixel p)
  (fn-for-posn p))  ; Number
;; Template rules used:
;;  - compound: 2 fields
;;  - reference: position field is posn

;; ListOfPixel is one of:
;;  - empty
;;  - (cons Pixel ListOfPixel)
;; interp. the list of pixels that corresponds to the shield in the game

(define LOP-1 empty)
(define LOP-2 (list (make-pixel (make-posn 1 1))))
(define LOP-3 (list (make-pixel (make-posn 1 1)) (make-pixel (make-posn 1 2))))

#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]                    ; BASE CASE
        [else (... (fn-for-pixel (first lop))   ; Pixel
                   (fn-for-lop (rest lop)))]))  ; NATURAL RECURSION
;;                 /
;;                /
;;          COMBINATION
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Pixel ListOfPixel)
;;  - self-reference: (rest lop) is ListOfPixel

;; ListOfShields is one of:
;;  - empty
;;  - (cons ListOfPixel ListOfShields)
;; interp. the list of shields

(define LOS-1 empty)
(define LOS-2 (list LOP-2))
(define LOS-3 (list LOP-2 LOP-3))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]                    ; BASE CASE
        [else (... (fn-for-lop (first los))     ; ListOfPixel
                   (fn-for-los (rest los)))]))  ; NATURAL RECURSION
;;                 /
;;                /
;;          COMBINATION
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons ListOfPixel ListOfShields)
;;  - self-reference: (rest los) is ListOfShields

;; ListOfEnemy is one of:
;;  - empty
;;  - (cons Enemy ListOfEnemy)
;; interp. a list of enemies

(define LOE-1 empty)
(define LOE-2 (list (make-enemy (make-posn 10 20) 2 "right")))
(define LOE-3 (list (make-enemy (make-posn 10 20) 2 "right") (make-enemy (make-posn 20 20) 2 "right")))

#;
(define (fn-for-loe loe)
  (cond [(empty? loe) (...)]                   ;BASE CASE
        [else (... (fn-for-enemy (first loe))  ;Enemy
                   (fn-for-loe (rest loe)))])) ;NATURAL RECURSION
;;             /
;;            /
;;       COMBINATION
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Enemy ListOfEnemy)
;;  - self-reference: (rest loe) is ListOfEnemy

(define-struct hero (position lives))
;; Hero is (make-hero posn Number)
;; interp. a hero at position x, y with lives count

(define HERO-1 (make-hero (make-posn 10 2) 3))

#;
(define (fn-for-hero h)
  (... (fn-for-posn (hero-posn h))       ;posn
       (hero-lives h)))   ;Number
;; Template rules used:
;;  - compound: 3 fields

;; ProjectileDirection is one of:
;;  - "up"
;;  - "down"
;; interp. the direction of the projectile

#;
(define (fn-for-projectile-direction pd)
  (cond [(string=? "up" pd) (...)]
        [(string=? "down" pd) (...)]))
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: "up"
;;  - atomic distinct: "down"

(define-struct projectile (positon direction))
;; Projectile is (make-projectile posn ProjectileDirection)
;; interp. a projectile at position x, y  with some direction

(define PROJECTILE-1 (make-projectile (make-posn 11 13) "up"))

#;
(define (fn-for-projectile p)
  (... (fn-for-posn (projectile-x p))                            ;posn
       (fn-for-projectile-direction (projectile-direction p))))  ;ProjectileDirection

;; Template rules used:
;;  - compound: 3 fields

;; ListOfProjectile is one of:
;;  - empty
;;  - (cons Projectile ListOfProjectile)
;; interp. a list of Dot
(define LOP1 empty)
(define LOP2 (list (make-projectile (make-posn 6 10) "up") (make-projectile (make-posn 20 30) "down")))
#;
(define (fn-for-loproj lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-projectile (first lop))
              (fn-for-loproj (rest lop)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Projectile ListOfProjectile)
;;  - reference: (first lop) is Projectile
;;  - self-reference: (rest lop) is ListOfProjectile

(define-struct game (level hero enemies shields projectiles stat))
;; Game is (make-game Number ListOfEnemy ListOfShields Statistics)
;; interp. a game with level, enemies, shields and stats

(define GAME-1 (make-game 1
                          (make-hero (make-posn 6 3) 3)
                          (list (make-enemy (make-posn 10 20) 2 "right")
                                (make-enemy (make-posn 20 20) 2 "right"))
                          (list (make-pixel (make-posn 1 1))
                                (make-pixel (make-posn 1 2)))
                          (list (make-projectile (make-posn 6 10) "up")
                                (make-projectile (make-posn 12 42) "down"))
                          (make-stat 6 "running")))

#;
(define (fn-for-game g)
  (... (game-level g)                       ;Number
       (fn-for-hero (game-hero g))          ;Hero
       (fn-for-loe (game-enemies g))        ;ListOfEnemy
       (fn-for-los (game-shields g))        ;ListOfShields
       (fn-for-loproj (game-projectiles g)) ;ListOfProjectile
       (fn-for-stat (game-stat g))))        ;Statistics
;; Template rules used:
;;  - compound: 6 fields

;; =================
;; Functions:

;; Game -> Game
;; start the world with (main GAME-INIT)
;;
(define (main g)
  (big-bang g         ; Game
    (on-tick tock)    ; Game -> Game
    (to-draw render)  ; Game -> Image
    (on-key ...))) ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next state of the game
;; !!!
(define (tock g) g)

;; Game -> Image
;; render current state of game
;; !!!
(define (render g) empty-image)

;; Number Number Number -> ListOfPixel
;; Generate line of pixels where x and y with some length
(check-expect (generate-line (make-posn 0 0) 0) empty)
(check-expect (generate-line (make-posn 0 0) 4) (list (make-pixel (make-posn 0 0)) (make-pixel (make-posn 1 0)) (make-pixel (make-posn 2 0)) (make-pixel (make-posn 3 0))))

;(define (generate-line x y length) empty) ; stub

(define (generate-line position lenght)
  (cond [(= lenght 0) empty]
        [else (cons (make-pixel (make-posn (posn-x position) (posn-y position)))
                    (generate-line (make-posn (+ (posn-x position) 1) (posn-y position)) (- lenght 1)))]))

;; Pixel Image -> Image
;; Render pixel to image on background
(check-expect (draw-pixel (make-pixel (make-posn 0 0)) BACKGROUND) (place-image PIXEL-IMAGE 0 0 BACKGROUND))
(check-expect (draw-pixel (make-pixel (make-posn 10 10)) BACKGROUND) (place-image PIXEL-IMAGE 10 10 BACKGROUND))

;(define (draw-pixel p scene) scene) ; stub

(define (draw-pixel p scene)
  (place-image PIXEL-IMAGE
               (posn-x (pixel-position p)) (posn-y (pixel-position p))
               scene))

;; ListOfPixel Image -> Image
;; Render list of pixels to image on background
(check-expect (draw-pixels empty empty-image) empty-image)
(check-expect (draw-pixels (generate-line (make-posn 10 10) 1) BACKGROUND) (draw-pixel (make-pixel (make-posn 10 10)) BACKGROUND))
(check-expect (draw-pixels (generate-line (make-posn 10 10) 4) BACKGROUND) (draw-pixel (make-pixel (make-posn 10 10)) (draw-pixel (make-pixel (make-posn 11 10)) (draw-pixel (make-pixel (make-posn 12 10)) (draw-pixel (make-pixel (make-posn 13 10)) BACKGROUND)))))

;(define (draw-pixels lop scene) BACKGROUND) ; stub

(define (draw-pixels lop scene)
  (cond [(empty? lop) scene]                               
        [else (draw-pixel (first lop) (draw-pixels (rest lop) scene))]))

;; posn Number -> ListOfPixel
;; prod. n-times with some position line of pixels (helper function for generate shields)
(check-expect (generate-lines (make-posn 0 0) 0) empty)
(check-expect (generate-lines (make-posn 0 0) 1) (list
                                                  (make-pixel (make-posn 0 0))
                                                  (make-pixel (make-posn 1 0))
                                                  (make-pixel (make-posn 2 0))
                                                  (make-pixel (make-posn 3 0))
                                                  (make-pixel (make-posn 4 0))
                                                  (make-pixel (make-posn 5 0))
                                                  (make-pixel (make-posn 6 0))
                                                  (make-pixel (make-posn 7 0))
                                                  (make-pixel (make-posn 8 0))
                                                  (make-pixel (make-posn 9 0))
                                                  (make-pixel (make-posn 10 0))
                                                  (make-pixel (make-posn 11 0))
                                                  (make-pixel (make-posn 12 0))
                                                  (make-pixel (make-posn 13 0))
                                                  (make-pixel (make-posn 14 0))
                                                  (make-pixel (make-posn 15 0))
                                                  (make-pixel (make-posn 16 0))
                                                  (make-pixel (make-posn 17 0))
                                                  (make-pixel (make-posn 18 0))
                                                  (make-pixel (make-posn 19 0))
                                                  (make-pixel (make-posn 20 0))
                                                  (make-pixel (make-posn 21 0))
                                                  (make-pixel (make-posn 22 0))
                                                  (make-pixel (make-posn 23 0))
                                                  (make-pixel (make-posn 24 0))
                                                  (make-pixel (make-posn 25 0))
                                                  (make-pixel (make-posn 26 0))
                                                  (make-pixel (make-posn 27 0))
                                                  (make-pixel (make-posn 28 0))
                                                  (make-pixel (make-posn 29 0))
                                                  (make-pixel (make-posn 30 0))
                                                  (make-pixel (make-posn 31 0))))

(define (generate-lines position times)
  (cond [(< times 1) empty]
        [else (append (generate-line position SHIELD-SIZE)
                      (generate-lines
                       (make-posn (posn-x position) (+ (posn-y position) 1))
                       (- times 1)))]))

;; posn -> ListOfPixel
;; prod. shield with some position
(check-expect (generate-shield (make-posn 0 0)) (append
                                                 (generate-line (make-posn 0 0) 32)
                                                 (generate-line (make-posn 0 1) 32)
                                                 (generate-line (make-posn 0 2) 32)
                                                 (generate-line (make-posn 0 3) 32)
                                                 (generate-line (make-posn 0 4) 32)
                                                 (generate-line (make-posn 0 5) 32)
                                                 (generate-line (make-posn 0 6) 32)
                                                 (generate-line (make-posn 0 7) 32)
                                                 (generate-line (make-posn 0 8) 32)
                                                 (generate-line (make-posn 0 9) 32)
                                                 (generate-line (make-posn 0 10) 32)
                                                 (generate-line (make-posn 0 11) 32)
                                                 (generate-line (make-posn 0 12) 32)
                                                 (generate-line (make-posn 0 13) 32)
                                                 (generate-line (make-posn 0 14) 32)
                                                 (generate-line (make-posn 0 15) 32)
                                                 (generate-line (make-posn 0 16) 32)
                                                 (generate-line (make-posn 0 17) 32)
                                                 (generate-line (make-posn 0 18) 32)
                                                 (generate-line (make-posn 0 19) 32)
                                                 (generate-line (make-posn 0 20) 32)
                                                 (generate-line (make-posn 0 21) 32)
                                                 (generate-line (make-posn 0 22) 32)
                                                 (generate-line (make-posn 0 23) 32)
                                                 (generate-line (make-posn 0 24) 32)
                                                 (generate-line (make-posn 0 25) 32)
                                                 (generate-line (make-posn 0 26) 32)
                                                 (generate-line (make-posn 0 27) 32)
                                                 (generate-line (make-posn 0 28) 32)
                                                 (generate-line (make-posn 0 29) 32)
                                                 (generate-line (make-posn 0 30) 32)
                                                 (generate-line (make-posn 0 31) 32)
                                                 (generate-line (make-posn 0 32) 32)))


(define (generate-shield position)
  (generate-lines position (+ SHIELD-SIZE 1)))

(define SHIELDS-INIT (append (generate-shield (make-posn 0 0)) (generate-shield (make-posn 64 0))))

(define GAME-INIT (make-game 1
                             (make-hero (make-posn 0 0) 3)
                             empty
                             SHIELDS-INIT
                             empty
                             (make-stat 0 "running")))
