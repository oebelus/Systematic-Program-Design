;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname count-odd-even-tr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct counts (odds evens))
;; Counts is (make-counts Natural Natural)
;; interp. describes the number of even and odd numbers in a list

(define C1 (make-counts 0 0)) ;describes an empty list
(define C2 (make-counts 3 2)) ;describes (list 1 2 3 4 5))

(check-expect (count (list 1 2 3 4 5)) (make-counts 3 2))

;; (listof Number) -> Counts
(define (count lon0)
  (local [(define (count lon acc)
            (cond [(empty? lon) acc]
                  [else
                   (if (zero? (modulo (first lon) 2))
                       (count (rest lon) (make-counts (counts-odds acc) (add1 (counts-evens acc))))
                       (count (rest lon) (make-counts (add1 (counts-odds acc)) (counts-evens acc))))]))]
    (count lon0 (make-counts 0 0))))