;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname render-roster) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants
;; ---------

(define CELL-WIDTH 200)
(define CELL-HEIGHT 30)

(define TEXT-SIZE 20)
(define TEXT-COLOR "black")

;; Data Definitions
;; ----------------

(define-struct player (name team))
;; Player is (make-player String Natural[1,2])
;; interp. a dodgeball player. 
;;   (make-player s t) represents the player named s 
;;   who plays on team t
;; 
(define P0 (make-player "Samael" 1))
(define P1 (make-player "Georgia" 2))

#;
(define (fn-for-player p)
  (... (player-name p)
       (player-team p)))


;; ListOfPlayer is one of:
;; - empty
;; - (cons Player ListOfPlayer)
;; interp.  A list of players.
(define LOP0 empty)                     ;; no players
(define LOP2 (cons P0 (cons P1 empty))) ;; two players

#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-player (first lop))
              (fn-for-lop (rest lop)))]))


;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LOS0 empty)
(define LOS2 (cons "Samael" (cons "Georgia" empty)))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; Functions
;; ---------

;; PROBLEM 1: 
;;
;; Design a function called select-players that consumes a list 
;; of players and a team t (Natural[1,2]) and produces a list of 
;; players that are on team t.

;; ListOfPlayer Natural[1, 2] -> ListOfPlayer
;; Given a list of players and a team, produces the list of players that are on team t
(check-expect (select-players empty 1) empty)
(check-expect (select-players empty 2) empty)

(check-expect (select-players (cons (make-player "Samael" 1) (cons (make-player "John" 1) (cons (make-player "Bob" 2) empty))) 1)
              (cons (make-player "Samael" 1) (cons (make-player "John" 1) empty)))

(check-expect (select-players (cons (make-player "Sally" 2) (cons (make-player "John" 1) (cons (make-player "Bob" 2) empty))) 2)
              (cons (make-player "Sally" 2) (cons (make-player "Bob" 2) empty)))

(define (select-players lop t)
  (cond [(empty? lop) empty]
        [else
         (if (= (player-team (first lop)) t)
             (cons (first lop) (select-players (rest lop) t))
             (select-players (rest lop) t))]))
  

;; PROBLEM 2:  
;; 
;; Complete the design of render-roster. We've started you off with 
;; the signature, purpose, stub and examples. You'll need to use
;; the function that you designed in Problem 1.
;; 
;; Note that we've also given you a full function design for render-los
;; and its helper, render-cell. You will need to use these functions
;; when solving this problem.

;; ListOfPlayer -> Image
;; Render a game roster from the given list of players

(check-expect (render-roster empty)
              (beside/align 
               "top"
               (overlay
                (text "Team 1" TEXT-SIZE TEXT-COLOR)
                (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))
               (overlay
                (text "Team 2" TEXT-SIZE TEXT-COLOR)
                (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))))
                
(check-expect (render-roster LOP2)
              (beside/align 
               "top"
               (above
                (overlay
                 (text "Team 1" TEXT-SIZE TEXT-COLOR)
                 (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))
                (overlay
                 (text "Samael" TEXT-SIZE TEXT-COLOR)
                 (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR)))
               (above
                (overlay
                 (text "Team 2" TEXT-SIZE TEXT-COLOR)
                 (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))
                (overlay
                 (text "Georgia" TEXT-SIZE TEXT-COLOR)
                 (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR)))))


; (define (render-roster lop) empty-image) ; stub

(define (render-roster lop)
  (beside/align "top"
                (render-team lop 1)
                (render-team lop 2)))

;; ListOfString, Natural[1, 2] -> Image
;; Renders a team

(define (render-team lop t)
  (above (render-cell (string-append "Team " (number->string t)))
         (render-los (team-names (select-players lop t)))))

;; ListOfPlayer -> ListOfString
;; Produces a list of player names (String) from ListOfPlayer
;; !!!

(define (team-names lop)
  (cond [(empty? lop) empty]
        [else
         (cons (player-name (first lop))
               (team-names (rest lop)))]))

;; ListOfString -> Image
;; Render a list of strings as a column of cells.
(check-expect (render-los empty) empty-image)
(check-expect (render-los (cons "Samael" empty))
              (above 
               (overlay
                (text "Samael" TEXT-SIZE TEXT-COLOR)
                (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))
                     empty-image))
(check-expect (render-los (cons "Samael" (cons "Brigid" empty)))
              (above
                (overlay
                (text "Samael" TEXT-SIZE TEXT-COLOR)
                (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))
               (overlay
                (text "Brigid" TEXT-SIZE TEXT-COLOR)
                (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR))))

;(define (render-los lon) empty-image) ; stub

;; Took Template from ListOfString
(define (render-los los)
  (cond [(empty? los) empty-image]
        [else
         (above (render-cell (first los))
                (render-los (rest los)))]))



;; String -> Image
;; Render a cell of the game table
(check-expect (render-cell "Team 1") 
              (overlay
                 (text "Team 1" TEXT-SIZE TEXT-COLOR)
                 (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR)))

;(define (render-cell s) empty-image) ; stub

;; Template for String
(define (render-cell s)
  (overlay
   (text s TEXT-SIZE TEXT-COLOR)
   (rectangle CELL-WIDTH CELL-HEIGHT "outline" TEXT-COLOR)))
  


