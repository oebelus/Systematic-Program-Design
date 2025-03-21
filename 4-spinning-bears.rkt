(require 2htdp/image)
(require 2htdp/universe)

;; An Arbitrary number of Spinning Bears

;; Constants:
;; =================

(define BEAR .)
(define ROTATION-SPEED 10)

(define WIDTH 600)
(define HEIGHT 400)
(define MTS (empty-scene WIDTH HEIGHT))

(define CENTER-X (/ WIDTH 2))
(define CENTER-Y (/ HEIGHT 2))

;; Data definitions:
;; =================

(define-struct bear (x y r))

;; Position is (make-bear Number Number)
;; interp. the bear at position x, y and rotation angle r

(define B1 (make-bear 5 4 20))
(define B2 (make-bear 200 100 15))

#;
(define (fn-for-bear b)
  (... (bear-x b)
       (bear-y b)
       (bear-r b)))

;; Template rules used:
;;  - compound: 3 fields


;; ListOfBear is one of:
;;  - empty
;;  - (cons Bear ListOfBear)
;; interp. a list of Bears

(define LOB1 empty)
(define LOB2 (cons (make-bear 10 5 10) empty))
(define LOB3 (cons (make-bear 10 5 15) (cons (make-bear 200 400 25) empty)))

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-bear (first lob))
              (fn-for-lob (rest lob)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Bear ListOfBear)
;;  - reference: (first lob) is Bear
;;  - self-reference: (rest lob) is ListOfBear

;; =================
;; Functions:

;; Bear -> Bear
;; start the world with ...
;; 
(define (main Bear)
  (big-bang Bear                   ; Bear
            (on-tick   spin-all)     ; ListOfBear -> ListOfBear
            (to-draw   render-all)   ; Bear -> Image
            ;; (stop-when ...)      ; Bear -> Boolean
            (on-mouse  handle-mouse)))      ; Bear Integer Integer MouseEvent -> Bear
            ;; (on-key    ...)))    ; Bear KeyEvent -> Bear

;; ListOfBear Number Number MouseEvent -> Bear
;; Replace the old Bear with a new Bear
 
(define (handle-mouse lob x y m)
  (cond
    [(string=? m "button-down")
     (cons (make-bear x y 0) lob)]
    [else lob]))

;; Bear -> Bear
;; Increment the bear's rotation angle by ROTATION-SPEED
;; !!!

(define (spin-all lob)
  (cond [(empty? lob) empty]
        [else
         (cons (spin (first lob))
              (spin-all (rest lob)))]))

(define (spin b)
  (make-bear
   (bear-x b)
   (bear-y b)
   (modulo (+ (bear-r b) ROTATION-SPEED) 360)))
  

;; Bear -> Image
;; Render the Bear Image

(define (render-bear b scene)
  (place-image (rotate (bear-r b) BEAR)
               (bear-x b)
               (bear-y b)
               scene))

(define (render-all lob)
  (cond [(empty? lob) MTS]
        [else
         (render-bear (first lob)
                      (render-all (rest lob)))]))

(define INITIAL-LOB (cons (make-bear CENTER-X CENTER-Y ROTATION-SPEED) empty))

(main INITIAL-LOB)










