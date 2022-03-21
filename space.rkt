#lang racket

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
;; interp. a list of pixels which corresponds to shield in game

(define LOP-1 empty)
(define LOP-2 (cons (make-pixel 1 1) empty))
(define LOP-3 (cons (make-pixel 1 1) (cons (make-pixel 1 2) empty)))

#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]                   ; BASE CASE
        [else (... (first lop)                 ; String
                   (fn-for-lop (rest lop)))])) ; NATURAL RECURSION
;;             /
;;            /
;;       COMBINATION
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Pixel ListOfPixel)
;;  - self-reference: (rest lop) is ListOfPixel



;; WS is ... (give WS a better name)

;; =================
;; Functions:

;; WS -> WS
;; start the world with (main ...)
;;
(define (main ws)
  (big-bang ws ; WS
    (on-tick tock) ; WS -> WS
    (to-draw render) ; WS -> Image
    (on-key (void)))) ; WS KeyEvent -> WS

;; WS -> WS
;; produce the next ...
;; !!!
(define (tock ws) ws)

;; WS -> Image
;; render ...
;; !!!
(define (render ws) empty-image)
