;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname replicate-elm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (listof X) Natural -> (listof X)
;; replicate each element in the list n times
(check-expect (replicate-elm (list "a" "b" "c") 2) (list "a" "a" "b" "b" "c" "c"))
(check-expect (replicate-elm (list 1 2 3) 3) (list 1 1 1 2 2 2 3 3 3))

(define (replicate-elm lox0 n)
  (local [(define (replicate-elm lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (zero? acc)
                       (replicate-elm (rest lox) n)
                       (cons
                        (first lox)
                        (replicate-elm lox (sub1 acc))))]))]
    (replicate-elm lox0 n)))
                       