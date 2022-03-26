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
(define WIDTH 512)
(define HEIGHT 1024)
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "black"))
(define HERO-SPEED 4)
(define PROJECTILE-SPEED 2)

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

(define-struct enemy (x y speed direction))
;; Enemy is (make-enemy Number Number Number EnemyDirection)
;; interp. a enemy at position x, y with speed and direction

(define ENEMY-1 (make-enemy 10 20 2 "right"))

#;
(define (fn-for-enemy e)
  (... (enemy-x e)                                      ; Number
       (enemy-y e)                                      ; Number
       (enemy-speed e)                                  ; Number
       (fn-for-enemy-direction (enemy-direction e))))   ; EnemyDirection
;; Template rules used:
;;  - compound: 4 fields
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

(define-struct pixel (x y))
;; Pixel is (make-pixel Number Number)
;; interp. a pixel at position x, y

(define PIXEL-1 (make-pixel 6 10))

#;
(define (fn-for-pixel p)
  (... (pixel-x p)    ; Number
       (pixel-y p)))  ; Number
;; Template rules used:
;;  - compound: 2 fields

;; ListOfPixel is one of:
;;  - empty
;;  - (cons Pixel ListOfPixel)
;; interp. the list of pixels that corresponds to the shield in the game

(define LOP-1 empty)
(define LOP-2 (list (make-pixel 1 1)))
(define LOP-3 (list (make-pixel 1 1) (make-pixel 1 2)))

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
(define SHIELDS-INIT empty)

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
(define LOE-2 (list (make-enemy 10 20 2 "right")))
(define LOE-3 (list (make-enemy 10 20 2 "right") (make-enemy 20 20 2 "right")))

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

(define-struct hero (x y lives))
;; Hero is (make-hero Number Number Number)
;; interp. a hero at position x, y with lives count

(define HERO-1 (make-hero 6 10 3))

#;
(define (fn-for-hero h)
  (... (hero-x h)       ;Number
       (hero-y h))      ;Number
  (hero-lives h))  ;Number
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

(define-struct projectile (x y direction))
;; Projectile is (make-projectile Number Number ProjectileDirection)
;; interp. a projectile at position x, y  with some direction

(define PROJECTILE-1 (make-projectile 6 10 "up"))

#;
(define (fn-for-projectile p)
  (... (projectile-x p)                                         ;Number
       (projectile-y p))                                        ;Number
  (fn-for-projectile-direction (projectile-direction p)))  ;ProjectileDirection
;; Template rules used:
;;  - compound: 3 fields

;; ListOfProjectile is one of:
;;  - empty
;;  - (cons Projectile ListOfProjectile)
;; interp. a list of Dot
(define LOP1 empty)
(define LOP2 (list (make-projectile 6 10 "up") (make-projectile 20 10 "down")))
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
                          (make-hero 6 10 3)
                          (list (make-enemy 10 20 2 "right")
                                (make-enemy 20 20 2 "right"))
                          (list (make-pixel 1 1)
                                (make-pixel 1 2))
                          (list (make-projectile 6 10 "up")
                                (make-projectile 20 10 "down"))
                          (make-stat 6 "running")))

(define GAME-INIT (make-game 1
                             (make-hero 0 0 3)
                             empty
                             SHIELDS-INIT
                             empty
                             (make-stat 0 "running")))

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
;; start the world with (main ...)
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
(check-expect (generate-line 0 0 0) empty)
(check-expect (generate-line 0 0 4) (list (make-pixel 0 0) (make-pixel 1 0) (make-pixel 2 0) (make-pixel 3 0)))

;(define (generate-line x y length) empty) ; stub

(define (generate-line x y lenght)
  (cond [(= lenght 0) empty]
        [else (cons (make-pixel x y)
                    (generate-line (+ x 1) y (- lenght 1)))]))

;; Pixel Image -> Image
;; Render pixel to image on background
(check-expect (draw-pixel (make-pixel 0 0) BACKGROUND) (place-image PIXEL-IMAGE 0 0 BACKGROUND))
(check-expect (draw-pixel (make-pixel 10 10) BACKGROUND) (place-image PIXEL-IMAGE 10 10 BACKGROUND))

;(define (draw-pixel p scene) scene) ; stub

(define (draw-pixel p scene)
  (place-image PIXEL-IMAGE
               (pixel-x p) (pixel-y p)
               scene))

;; ListOfPixel Image -> Image
;; Render list of pixels to image on background
(check-expect (draw-pixels empty BACKGROUND) BACKGROUND)
(check-expect (draw-pixels (generate-line 10 10 4) BACKGROUND) (overlay
                                                                (draw-pixel (make-pixel 10 10)
                                                                            (draw-pixel (make-pixel 11 10)
                                                                                        (draw-pixel (make-pixel 12 10)
                                                                                                    (draw-pixel (make-pixel 13 10)
                                                                                                                BACKGROUND))))
                                                                BACKGROUND))

;(define (draw-pixels lop scene) BACKGROUND) ; stub

(define (draw-pixels lop scene)
  (cond [(empty? lop) scene]                     ; BASE CASE
        [else (overlay (draw-pixel (first lop)   ; Pixel
                                   (draw-pixels (rest lop) scene)) ; NATURAL RECURSION 
                       scene)]))