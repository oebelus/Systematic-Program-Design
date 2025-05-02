;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname to-list-tr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Natural -> (listof Natural) 
;; produce (cons n (cons n-1 ... empty)), not including 0
(check-expect (to-list 0) empty)
(check-expect (to-list 1) (list 1))
(check-expect (to-list 3) (list 1 2 3))

;(define (to-list n) empty) ;stub
(define (to-list n)
  (local [(define (to-list n acc)
            (cond [(zero? n) acc]
                  [else
                   (to-list (sub1 n) (append (list n) acc))]))]
    (to-list n empty)))