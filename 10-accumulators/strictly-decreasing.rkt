;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname strictly-decreasing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (listof Natural) -> boolean
;; checks if the number in the list are in a strictly decreasing order
(check-expect (decreasing? (list 1 2 3 4 5)) false)
(check-expect (decreasing? (list 5 4 3 2 1)) true)
(check-expect (decreasing? (list 22 15 11 0)) true)

(define (decreasing? lon0)
  (local [(define (decreasing? lon prev)
            (cond [(empty? lon) true]
                  [else
                   (if (> prev (first lon))
                       (decreasing? (rest lon) (first lon))
                       false)]))]
    (decreasing? lon0 (add1 (first lon0)))))