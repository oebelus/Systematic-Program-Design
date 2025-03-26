;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bst-dd) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants
;; ==========
(define VSPACE (rectangle 1 50 "solid" "white"))
(define HSPACE (rectangle 1 10 "solid" "white"))
(define KEY-VAL-SEPARATOR ":")

(define BLANK (square 0 "solid" "white"))

(define TEXT-COLOR "black")
(define TEXT-SIZE 14)

;; Data Definitions
;; ==================

;; Design a data definition to represent a Binary Search Tree.

(define-struct node (key val l r))
;; BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are the left adnd right subtrees
;; INVARIANT: for a given node:
;;   key is > all keys in its l(eft) child
;;   key is < all keys in its r(right) child
;;   the same key doesn't appear twice in the tree

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))

#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)                    ;; Integer
              (node-val t)                     ;; String
              (fn-for-bst (node-l t))          ;; BST
              (fn-for-bst (node-r t)))]))      ;; BST

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - Self-reference: (node-l t) has type BST
;;  - Self-reference: (node-r t) has type BST

;; Functions
;; ===========

;; Design a look-up function.

;; BST Integer -> String or false
;; Find a node with a given key, if found produce value; if not found produce false.
(check-expect (lookup-key false 1) false)
(check-expect (lookup-key BST1 1) "abc")  ; L L Succeed
(check-expect (lookup-key BST1 0) false)  ; L Fail
(check-expect (lookup-key BST1 99) false) ; R Fail
(check-expect (lookup-key BST3 3) "ilk")
(check-expect (lookup-key BST3 4) "dcj")
(check-expect (lookup-key BST4 7) "ruf")
(check-expect (lookup-key BST4 15) false)

(define (lookup-key t k)
  (cond [(false? t) false]
        [else
         (cond [(= k (node-key t))
                (node-val t)]
               [(< k (node-key t))
                (lookup-key (node-l t) k)]
               [(> k (node-key t))
                (lookup-key (node-r t) k)])]))

;; BST Integer -> String or false
;; Produce the path of search for key in BST
;; path is list of "L"|"R", ends with "Succeed"|"Fail"
(check-expect (path BST0 10) (list "Fail"))
(check-expect (path BST3 -1) (list "L" "L" "Fail"))
(check-expect (path BST4 7) (list "R" "Succeed"))

(define (path t k)
  (cond [(false? t) (list "Fail")]
        [else
         (cond [(= k (node-key t))
                (list "Succeed")]
               [(< k (node-key t))
                (cons "L" (path (node-l t) k))]
               [(> k (node-key t))
                (cons "R" (path (node-r t) k))])]))

;; BST -> Image
;; Given a BST, generate an Image representing it
(check-expect (render-bst false) BLANK)
(check-expect (render-bst BST1)
              (above
               (text (string-append "1" KEY-VAL-SEPARATOR "abc")
                     TEXT-SIZE
                     TEXT-COLOR)
               VSPACE
               (beside (render-bst false)
                       HSPACE
                       (render-bst false))))

(define (render-bst t)
  (cond [(false? t) BLANK]
        [else
         (above
          (text (string-append (number->string (node-key t)) KEY-VAL-SEPARATOR (node-val t))
                TEXT-SIZE
                TEXT-COLOR)
          VSPACE
          (beside
           (render-bst (node-l t))
           HSPACE
           (render-bst (node-r t))))]))


