(require 2htdp/image)
(require 2htdp/universe)

;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
            (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
            (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position
(check-expect (handle-mouse (cons (make-drop 100 200) (cons (make-drop 150 150) empty)) 10 50 "button-down")
              (cons (make-drop 10 50) (cons (make-drop 100 200) (cons (make-drop 150 150) empty))))

;; (define (handle-mouse lod x y mevt) empty) ; stub

(define (handle-mouse lod x y mevt)
  (cond [(mouse=? mevt "button-down") (cons (make-drop x y) lod)]
        [else lod]))

;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops
(check-expect (next-drops empty) empty)
(check-expect (next-drops (cons (make-drop 100 200) (cons (make-drop 150 150) empty)))
              (cons (make-drop 100 (+ SPEED 200)) (cons (make-drop 150 (+ SPEED 150)) empty)))
(check-expect (next-drops (cons (make-drop 100 200) (cons (make-drop 500 150) empty)))
              (cons (make-drop 100 (+ SPEED 200)) empty))

; (define (next-drops lod) empty) ; stub

(define (next-drops lod)
  (cond [(empty? lod) empty]
        [else
         (if (beyond (first lod))
             (next-drops (rest lod))
             (cons (move-drop (first lod)) (next-drops (rest lod))))]))

;; Drop -> Drop
;; Move the drop accross the Y axis by factor of SPEED
(check-expect (move-drop (make-drop 10 20)) (make-drop 10 (+ 20 SPEED)))

(define (move-drop d)
  (make-drop (drop-x d) (+ (drop-y d) SPEED)))

;; Drop -> Boolean
;; Tells if a drop's dimensions exceeds the layout
(check-expect (beyond (make-drop 10 15)) false)
(check-expect (beyond (make-drop 350 350)) true)

(define (beyond d)
  (or (> (drop-x d) WIDTH) (> (drop-y d) HEIGHT)))

;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (render-drops empty) MTS)
(check-expect (render-drops (cons (make-drop 100 200) (cons (make-drop 150 150) empty)))
              (place-image DROP 100 200 (place-image DROP 150 150 MTS)))

; (define (render-drops lod) MTS) ; stub

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (place-image DROP (drop-x (first lod)) (drop-y (first lod))
         (render-drops (rest lod)))]))

(main (cons (make-drop 10 15) empty))