;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sum-odds-tr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (listof Number) -> Number
;; produce sum of all odd numbers of lon
(check-expect (sum-odds empty) 0) 
(check-expect (sum-odds (list 1 2 5 6 11)) 17) 

(define (sum-odds lon0)
  (local [(define (sum-odds lon sum)
            (cond [(empty? lon) sum]
                  [else
                   (if (odd? (first lon))
                       (sum-odds (rest lon) (+ (first lon) sum))
                       (sum-odds (rest lon) sum))]))]
    (sum-odds lon0 0)))

