;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname my-fractals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define BASE 2)

(define (carpet s)
  (if (>= BASE s)
      (square BASE "outline" "red")
      (overlay (square s "outline" "red")
               (local [(define sub (carpet (/ s 2)))]
                 (above (beside sub sub sub)
                        (beside sub (square (/ s 2) "outline" "red") sub)
                        (beside sub sub sub))))))

(define (vine s)
  (if (>= BASE s)
      (square BASE "outline" "green")
      (overlay (square s "outline" "green")
               (local [(define sub (vine (/ s 2)))]
                 (above (beside sub sub sub)
                        (beside sub sub)
                        (beside sub sub))))))

;(vine 50)

(define (pixel s)
  (if (>= BASE s)
      (square BASE "outline" "violet")
      (overlay (square s "outline" "violet")
               (local [(define sub (pixel (/ s 4)))]
                 (above (beside sub sub sub)
                        (beside sub (square (/ s 2) "outline" "violet") sub)
                        (beside sub sub sub))))))

(define (chip s)
  (if (>= BASE s)
      (square BASE "outline" "darkgreen")
      (overlay (square s "outline" "darkgreen")
               (local [(define sub (chip (/ s 2)))]
                 (above (beside sub sub)
                        (beside sub sub))))))

(chip 400)