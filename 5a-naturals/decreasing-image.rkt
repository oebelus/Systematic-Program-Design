;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname decreasing-image) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Design a function called decreasing-image that consumes a Natural n and produces an image of all the numbers 
;; from n to 0 side by side.
 
;; Constants
;; ===========
(define SIZE 24)
(define COLOR "black")

;; Natural -> Image
;; Produces an image of Natural[n, 0], side by side.
(check-expect (decreasing-image 0) (text "0" SIZE COLOR))
(check-expect (decreasing-image 3)
              (beside (text "3" SIZE COLOR)
                      (rectangle 5 10 "outline" "white")
                      (text "2" SIZE COLOR)
                      (rectangle 5 10 "outline" "white")
                      (text "1" SIZE COLOR)
                      (rectangle 5 10 "outline" "white")
                      (text "0" SIZE COLOR)))

(define (decreasing-image n)
  (cond [(zero? n) (text (format "~v" n) SIZE COLOR)]
        [else
         (beside (text (format "~v" n) SIZE COLOR)
                 (rectangle 5 10 "outline" "white")
                 (decreasing-image (sub1 n)))]))