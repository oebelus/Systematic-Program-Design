;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname image-organizer) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; =================
;; Constants:

(define BLANK (text "" 5 "white"))
(define TEXT-COLOR "black")
(define TEXT-SIZE 24)
(define VSPACE (rectangle 10 5 "solid" "white"))
(define HSPACE (rectangle 20 10 "solid" "white"))

;; =================
;; Data definitions:

(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. A directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.

;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;         a directory.

;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

(define I1 (square 10 "solid" "red"))
(define I2 (square 10 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))
(define D7 (make-dir "D7" (list D6 D4 D5) empty))

;; Problem A: A- Annotate the type comments with reference arrows and label each one to say 
;; whether it is a reference, self-reference or mutual-reference.


;; Problem A: B- Write out the templates for Dir, ListOfDir and ListOfImage. Identify for each 
;; call to a template function which arrow from part A it corresponds to.

#;
(define (fn-for-dir d)
  (... (dir-name d)
       (fn-for-lod (dir-sub-dirs d))
       (fn-for-loi (dir-images d))))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-dir (first lod))
              (fn-for-lod (rest lod)))]))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;; =================
;; Functions:
  
;; Problem B: Design a function to calculate the total size (width* height) of all the images 
;; in a directory and its sub-directories.

;; Dir -> Integer
;; ListOfDir -> ListOfImage
;; Loi -> Integer
;; Produces the sum of total size of images in a Dir and its sub-dirs
(check-expect (size (make-dir "D0" empty empty)) 0)
(check-expect (size D5) (* (image-width I3) (image-height I3)))
(check-expect (size D4) (+
                              (* (image-width I1) (image-height I1))
                              (* (image-width I2) (image-height I2))))
(check-expect (size D6) (+
                              (size D4)
                              (size D5)))

(define (size d)
  (local [(define (size--dir d)
            (if (not (empty? (dir-images d)))
                (size--images (dir-images d))
                (size--lod (dir-sub-dirs d))))

          (define (size--lod lod)
            (cond [(empty? lod) 0]
                  [else
                   (+ (size--dir (first lod))
                      (size--lod (rest lod)))]))

          (define (size--images loi)
            (cond [(empty? loi) 0]
                  [else
                   (+ (* (image-width (first loi)) (image-height (first loi)))
                      (size--images (rest loi)))]))]
    (size--dir d)))

;; Problem C: Design a function to produce rendering of a directory with its images. Keep it 
;; simple and be sure to spend the first 10 minutes of your work with paper and 
;; pencil!

;; Dir -> Image
;; Renders a Directory with its Images

(define (render d)
  (local [(define (render--dir d)
            (beside (text (dir-name d) TEXT-SIZE TEXT-COLOR)
                    (render--lod (dir-sub-dirs d))
                    VSPACE
                    (render--loi (dir-images d))))

          (define (render--lod lod)
            (cond [(empty? lod) BLANK]
                  [else
                   (above/align "left"
                                (render--dir (first lod))
                                HSPACE
                                (render--lod (rest lod)))]))

          (define (render--loi loi)
            (cond [(empty? loi) BLANK]
                  [else
                   (above/align "left"
                                (first loi)
                                HSPACE
                                (render--loi (rest loi)))]))]
    (render--dir d)))


          