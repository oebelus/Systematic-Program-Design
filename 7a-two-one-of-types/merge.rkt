;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname merge) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ListOfNumber ListOfNumber -> ListOfNumber
;; Concat sorted lists of numbers and produce one sorted list of all numbers
(check-expect (merge empty empty) empty)
(check-expect (merge empty (list 4 5 6)) (list 4 5 6))
(check-expect (merge (list 1 2 3) empty) (list 1 2 3))
(check-expect (merge (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
(check-expect (merge (list 4 5 6) (list 1 2 3)) (list 1 2 3 4 5 6))
(check-expect (merge (list 0 2 4) (list 1 3)) (list 0 1 2 3 4))

(define (merge lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(empty? lon2) lon1]
        [else
         (if (<= (first lon1) (first lon2))
             (cons (first lon1)
                   (merge (rest lon1) lon2 ))
             (cons (first lon2)
                   (merge (rest lon2) lon1)))]))