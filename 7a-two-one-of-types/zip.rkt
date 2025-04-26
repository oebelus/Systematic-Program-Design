;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname zip) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================
;; Data Definitions:

(define-struct entry (k v))
;; Entry is (make-entry Number Number)
;; Interp. an entry maps a key to a value
(define E1 (make-entry 3 12))

;; ListOfEntry is one of:
;;  - empty
;;  - (cons Entry ListOfEntry)
;; interp. a list of key value entries
(define LOE1 (list E1 (make-entry 1 11)))
(define LOE2 (list (make-entry 1 11) (make-entry 2 12)))

;; ==========
;; Functions:

;; ListOfNumber ListOfNumber -> ListOfEntry
;; Zip 2 list of numbers with equal length together into a list of Entry
(check-expect (zip empty empty) empty)
(check-expect (zip (list 1 2) (list 11 12)) LOE2)

(define (zip lon1 lon2)
  (cond [(empty? lon1) empty]
        [else
         (cons (make-entry (first lon1) (first lon2))
               (zip (rest lon1) (rest lon2)))]))






























