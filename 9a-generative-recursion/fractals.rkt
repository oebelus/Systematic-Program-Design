;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fractals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
;; Design a function that consumes a number and produces a Sierpinski
;; triangle of that size. Your function should use generative recursion.

;; Number -> Image
;; Produces Serpienski Triangle of a given size
(check-expect (sierpinski BASE) (triangle BASE "outline" "red"))
(check-expect (sierpinski (* BASE 2))
              (overlay (triangle (* 2 BASE) "outline" "red")
                       (local [(define sub (triangle BASE "outline" "red"))]
                         (above sub
                                (beside sub sub)))))

(define BASE 10)

(define (sierpinski s)
  (if (>= BASE s)
      (triangle BASE "outline" "red")
      (overlay (triangle s "outline" "red")
               (local [(define sub (sierpinski (/ s 2)))]
                 (above sub
                        (beside sub sub))))))

;; Design a function to produce a Sierpinski carpet of size s.

(define (sierpinski-carpet s)
  (if (>= BASE s)
      (square s "solid" "red")
      (overlay (square s "outline" "red")
               (local [(define sub (sierpinski-carpet (/ s 3)))
                       (define blk (square (/ s 3) "solid" "white"))]
                 (above (beside sub sub sub)
                        (beside sub blk sub)
                        (beside sub sub sub))))))

(sierpinski 300)
(sierpinski-carpet 300)