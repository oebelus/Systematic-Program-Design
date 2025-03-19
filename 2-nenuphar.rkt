;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-nenuphar) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; NENUPHAR walking from the left to the right across the screen.

;; =================
;; Constants:
(define WIDTH 600)
(define HEIGHT 400)

(define Y-N (/ HEIGHT 3))
(define Y-S (- HEIGHT (/ HEIGHT 3)))

(define MTS (overlay
             (rectangle WIDTH HEIGHT "solid" "blue")
             (empty-scene WIDTH HEIGHT)))

(define NENUPHAR-IMG (overlay
                    (radial-star 8 8 50 "solid" "violet")
                    (radial-star 10 10 80 "solid" "white")
                    (rotate 30 (wedge 60 300 "solid" "green"))))

(define SPIRAL (overlay
                (radial-star 8 8 50 "solid" "purple")
                (radial-star 10 10 80 "solid" "yellow")
                (star-polygon 20 20 3 "solid" "lime")
                ))

;; =================
;; Data definitions:

(define-struct position (x dx))
;; Position is (make-position Natural[0, WIDTH], Integer)
;; interp. make-position(x, dy) is the NENUPHAR x coordinates and dx speed.

(define p1 (make-position 10 3));
(define P2 (make-position -10 -3));

(define (fn-for-position p)
  (... (p-x p)    ; Natural[0, WIDTH]
       (p-dx p)))  ; Natural[0, HEIGHT]

;; Template rules used:
;;  - atomic non-distince: Number

;; =================
;; Functions:

;; NENUPHAR -> NENUPHAR
;; start the world with (main 0)
;; 
(define (main p)
  (big-bang p                           ; NENUPHAR
            (on-tick   advance)         ; NENUPHAR -> NENUPHAR
            (to-draw   render)          ; NENUPHAR -> Image
            (on-key    handle-key)))    ; NENUPHAR KeyEvent -> NENUPHAR

(define (handle-key p key)
        (cond [(key=? key " ") (make-position 0 (position-dx p))]
              [else p]))

;; NENUPHAR -> NENUPHAR
;; produce the next NENUPHAR by advancing it 1 pixel to the rigth

; (define (advance p) 0)

(define (advance p)
  (cond [(> (+ (position-x p) (position-dx p)) WIDTH) (make-position WIDTH (- (position-dx p)))]
        [(< (+ (position-x p) (position-dx p)) 0) (make-position 0 (- (position-dx p)))]
        [else
         (make-position (+ (position-x p) (position-dx p)) (position-dx p))]))

;; NENUPHAR -> Image
;; render NENUPHAR image at appropriate place on MTS

; (define (render p) MTS)

(define (render n)
  (place-image (rotate (modulo (position-x n) 360) NENUPHAR-IMG)
               (position-x n)
               Y-N
               (place-image (rotate (modulo (position-x n) 360) SPIRAL)
               (position-x n)
               (+ Y-S 30)
               MTS)))

(main (make-position 0 5))

;; NENUPHAR KeyEvent -> NENUPHAR
;; Start the animation over

; (define (handle-key p key) 0)

