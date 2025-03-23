;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname odd-from-n) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design a function called odd-from-n that consumes a natural number n, and produces a list of all 
;; the odd numbers from n down to 1. 
;;
;; Note that there is a primitive function, odd?, that produces true if a natural number is odd.

;; Natural -> ListOfNatural
;; Produces a list of odd numbers from n to 1
(check-expect (list-odd 0) empty)
(check-expect (list-odd 4) (cons 3 (cons 1 empty)))
(check-expect (list-odd 7) (cons 7 (cons 5 (cons 3 (cons 1 empty)))))

(define (list-odd n)
  (cond [(zero? n) empty]
        [else
         (if (odd? n)
             (cons n (list-odd (sub1 n)))
             (list-odd (sub1 n)))]))