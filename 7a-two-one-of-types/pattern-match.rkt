;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pattern-match) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================
;; Data Definitions:

;; 1String is String
;; interp. these are strings only 1 character long
(define 1SA "x")
(define 1SB "2")

;; Pattern is one of:
;;  - empty
;;  - (cons "A" Pattern)
;;  - (cons "N" Pattern)
;; interp.
;;   A pattern describing certain ListOf1String. 
;;  "A" means the corresponding letter must be alphabetic.
;;  "N" means it must be numeric.  For example:
;;      (list "A" "N" "A" "N" "A" "N")
;;   describes Canadian postal codes like:
;;      (list "V" "6" "T" "1" "Z" "4")
(define PATTERN1 (list "A" "N" "A" "N" "A" "N"))

;; ListOf1String is one of:
;;  - empty
;;  - (cons 1String ListOf1String)
;; interp. a list of strings each 1 long
(define LOS1 (list "V" "6" "T" "1" "Z" "4"))

;; ==========
;; Functions:

;; 1String -> Boolean
;; produce true if 1s is alphabetic/numeric
(check-expect (alphabetic? " ") false)
(check-expect (alphabetic? "1") false)
(check-expect (alphabetic? "a") true)
(check-expect (numeric? " ") false)
(check-expect (numeric? "1") true)
(check-expect (numeric? "a") false)

(define (alphabetic? 1s) (char-alphabetic? (string-ref 1s 0)))
(define (numeric?    1s) (char-numeric?    (string-ref 1s 0)))

;; Pattern ListOf1String -> Boolean
;; Return true if ListOf1String matches the Pattern
(check-expect (pattern-match? empty empty) true)
(check-expect (pattern-match? (list "A" "N" "A" "N")
                              (list "V" "6" "T" "1" "Z" "4")) true)
(check-expect (pattern-match? (list "A" "N" "A" "N" "A" "N")
                              (list "V" "6" "T" "1")) false)
(check-expect (pattern-match? (list "A" "N" "A" "N" "A" "N")
                              (list "V" "6" "T" "1" "Z" "4")) true)
(check-expect (pattern-match? (list "A" "N" "A" "N" "A" "A")
                              (list "V" "6" "T" "1" "Z" "4")) false)
(check-expect (pattern-match? empty (list "a")) true)
(check-expect (pattern-match? (list "A") empty) false)
(check-expect (pattern-match? (list "N") empty) false)
(check-expect (pattern-match? (list "A" "N" "A") (list "x" "3" "y")) true)
(check-expect (pattern-match? (list "A" "N" "A") (list "1" "3" "y")) false)
(check-expect (pattern-match? (list "N" "A" "N") (list "1" "a" "4")) true)
(check-expect (pattern-match? (list "N" "A" "N") (list "1" "b" "c")) false)
(check-expect (pattern-match? (list "A" "N" "A" "N" "A" "N")
                              (list "V" "6" "T" "1" "Z" "4")) true)

(define (pattern-match? p lo1s)
  (cond [(and (empty? p) (empty? lo1s)) true]
        [(empty? p) true]
        [(> (length p) (length lo1s)) false]
        [else
         (if (or
              (and (alphabetic? (first lo1s)) (string=? (first p) "A"))
              (and (numeric? (first lo1s)) (string=? (first p) "N")))
             (if (< (length (rest p)) (length (rest lo1s)))
                 true
                 (pattern-match? (rest p) (rest lo1s)))
             false)]))











