(require 2htdp/image)
(require 2htdp/universe)

(require 2htdp/image)
(require 2htdp/universe)

;; Traffic Lights, changing colors

;; =================
;; Constants:
(define WIDTH 600)
(define HEIGHT 400)

(define X (/ WIDTH 2))
(define Y (/ HEIGHT 2))

(define MTS (overlay (rectangle WIDTH HEIGHT "solid" "gray") (empty-scene WIDTH HEIGHT)))

(define CIRCLE-D 30)
(define RECTANGLE-W 80)
(define RECTANGLE-H 200)

(define RED 5)
(define YELLOW 2)
(define GREEN 5)

(define RED-LIGHT
  (overlay
   (above
    (circle CIRCLE-D "solid" "red")
    (circle CIRCLE-D "outline" "orange")
    (circle CIRCLE-D "outline" "green"))
   (rectangle RECTANGLE-W RECTANGLE-H "solid" "black")))

(define GREEN-LIGHT
  (overlay
   (above
    (circle CIRCLE-D "outline" "red")
    (circle CIRCLE-D "outline" "orange")
    (circle CIRCLE-D "solid" "green"))
   (rectangle RECTANGLE-W RECTANGLE-H "solid" "black")))

(define YELLOW-LIGHT
  (overlay
   (above
    (circle CIRCLE-D "outline" "red")
    (circle CIRCLE-D "solid" "orange")
    (circle CIRCLE-D "outline" "green"))
   (rectangle RECTANGLE-W RECTANGLE-H "solid" "black")))
;; =================
;; Data definitions:

;; Traffic is one of:
;;  - "Red"
;;  - "Yellow"
;;  - "Green"
;; interp. The colors of the traffic lights

#;
(define (fn-for-traffic t)
  (cond [{string=? "Red" t} (...)]
        [{string=? "Yellow" t} (...)]
        [{string=? "Green" t} (...)]))

;; Template rules used:
;;  - one of the 3 cases
;;     - atomic distinct: "Red"
;;     - atomic distinct: "Yellow"
;;     - atomic distinct: "Green"

;; =================
;; Functions:

;; Traffic -> Traffic
;; start the world with ...
;; 
(define (main Traffic)
  (big-bang Traffic              ; Traffic
            (on-tick start 1)    ; Traffic -> Traffic
            (to-draw render)))   ; Traffic -> Image

;; Traffic -> Traffic
;; produce the next traffic light
(check-expect (start "Red") "Green")

(define (start t)
  (cond [{string=? "Red" t} "Green"]
        [{string=? "Yellow" t} "Red"]
        [{string=? "Green" t} "Yellow"]))

;; Traffic -> Image
;; render the traffic color
(check-expect (render "Yellow") YELLOW-LIGHT)
  
(define (render t)
  (cond [{string=? "Red" t} RED-LIGHT]
        [{string=? "Yellow" t} YELLOW-LIGHT]
        [{string=? "Green" t} GREEN-LIGHT]))


(main "Red")
