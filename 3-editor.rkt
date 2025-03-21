(require 2htdp/image)
(require 2htdp/universe)

;; A text editor.

;; Constants:
;; =================

(define WIDTH 700)
(define HEIGHT 40)
(define FONT-SIZE 30)

(define EDITOR (rectangle WIDTH HEIGHT "outline" "black"))

;; Data definitions:
;; =================

;; Editor is (make-editor String Number)
;; interp. the text in the editor and the Editor position
(define-struct editor [text cursor])

(define E1 (make-editor "" 0))
(define E2 (make-editor "hello" 6))

#;
(define (fn-for-Editor e)
  (... (editor-text e)     ; String
       (editor-cursor e))) ; Number

;; Template used:
;;  - one of: 2 cases
;;    - atomic non-distinct: String
;;    - atomic non-distinct: Natural

;; =================
;; Functions:

;; Editor -> Editor
;; start the world with (main (make-editor "" 0))
;; 
(define (main Editor)
  (big-bang Editor                   ; Editor
            ;; (on-tick   tock)     ; Editor -> Editor
            (to-draw   render)   ; Editor -> Image
            ;; (stop-when ...)      ; Editor -> Boolean
            ;;(on-mouse  on-click)      ; Editor Integer Integer MouseEvent -> Editor
            (on-key    handle-key)))    ; Editor KeyEvent -> Editor

;; Editor KeyEvent -> Editor
;; Hanfle key events to update text

(define (handle-key e k)
  (cond
    [(key=? k "left")
     (make-editor (editor-text e) (max 0 (- (editor-cursor e) 1)))]
    [(key=? k "right")
     (make-editor (editor-text e) (min WIDTH (+ (editor-cursor e) 1)))]
    [(key=? k "\b")
     (make-editor (substring (editor-text e) 0 (max 0 (- (string-length (editor-text e)) 1))) (- (string-length (editor-text e)) 1))]
    [else
     (make-editor (string-append (editor-text e) k) (string-length (string-append (editor-text e) k)))]))

;;(define (on-click

;; Editor -> Image
;; render the Editor
(check-expect (render (make-editor "hi" 3))
              (overlay/align "left" "center"
                             (beside
                              (text "hi" FONT-SIZE "black")
                              (rectangle 2 FONT-SIZE "solid" "red"))
                             EDITOR))

(define (render e)
  (overlay/align "left" "center"
                 (beside
                  (text (editor-text e) FONT-SIZE "black")
                  (rectangle 2 FONT-SIZE "solid" "red"))
                 EDITOR))

(main (make-editor "" 0))