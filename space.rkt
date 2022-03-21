#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; Space Invaders Clone

;; =================
;; Constants:
;; =================

(define ENEMY-IMAGE (bitmap "images/enemy.png"))

;; Data definitions:

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
    ;(on-key ...)  ; WS KeyEvent -> WS
    ))


;; WS -> WS
;; produce the next ...
;; !!!
(define (tock ws) ws)

;; WS -> Image
;; render ...
;; !!!
(define (render ws) empty-image)
