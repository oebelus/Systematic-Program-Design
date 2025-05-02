;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname dropn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (listof X) Natural -> (listof X)
;; drops every nth element from the list
(check-expect (dropn (list 1 2 3 4 5 6 7) 2) (list 1 2 4 5 7))
(check-expect (dropn (list 1 2 3 4 5 6 7) 1) (list 1 3 5 7))

(define (dropn lox0 n)
  (local [(define (dropn lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (not (= acc n))
                       (cons (first lox)
                             (dropn (rest lox) (add1 acc)))
                       (dropn (rest lox) 0))]))]
    (dropn lox0 0)))