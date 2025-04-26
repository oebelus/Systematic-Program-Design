;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname concat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================
;; Data Definitions:

;; ListOfString is one of:
;;  - empty
;;  - (cons String ListOfString)
;; interp. a list of strings
(define LOS0 empty)
(define LOS1 (cons "c" (cons "d" empty)))
(define LOS2 (cons "a" (cons "b" empty)))

;; ==========
;; Functions:

;; ListOfString ListOfString -> ListOfString
;; concatenate 2 list of strings
(check-expect (concat empty empty) empty)
(check-expect (concat LOS1 empty) LOS1)
(check-expect (concat empty LOS2) LOS2)
(check-expect (concat LOS1 LOS2) (list "a" "b" "c" "d"))

(define (concat lsta lstb)
  (cond [(and (empty? lsta) (empty? lstb)) empty]
        [(empty? lsta) lstb]
        [(empty? lstb) lsta]
        [else
         (append lstb lsta)]))




















