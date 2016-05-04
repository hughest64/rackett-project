;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arrange-images-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; arrange-images-starter.rkt (problem statement)


;PROBLEM:
;
;In this problem imagine you have a bunch of pictures that you would like to 
;store as data and present in different ways. We'll do a simple version of that 
;here, and set the stage for a more elaborate version later.
;
;(A) Design a data definition to represent an arbitrary number of images.
;
;(B) Design a function called arrange-images that consumes an arbitrary number
;    of images and lays them out left-to-right in increasing order of size.
    

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; - interp. a list of images
(define LOI1 empty)
(define LOI2 (cons (rectangle 10 20 "solid" "blue")
                   (cons (rectangle 20 30 "solid" "red")
                         empty)))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;;functions:

;; ListOfImage -> Image
;; lay out images left to right in increasing order of size
;; sort images in increasing order of size and then lay them out left to right
(check-expect (arrange-images (cons (rectangle 20 30 "solid" "red")
                                    (cons (rectangle 10 20 "solid" "blue")
                                          empty)))
              (beside(rectangle 10 20 "solid" "blue")
                     (rectangle 20 30 "solid" "red")
                     empty-image))
(check-expect (arrange-images (cons (rectangle 10 20 "solid" "blue")
                                    (cons (rectangle 20 30 "solid" "red")
                                          empty)))
              (beside(rectangle 10 20 "solid" "blue")
                     (rectangle 20 30 "solid" "red")
                     empty-image))

;(define (arrange-images loi) empty-image) ;stub

(define (arrange-images loi)
  (layout-images (sort-images loi)))

;; ListOfImage -> Image
;; place images beside each other in order of list
(check-expect (layout-images empty) empty-image)
(check-expect (layout-images (cons (rectangle 10 20 "solid" "blue")
                                   (cons (rectangle 20 30 "solid" "red")
                                         empty)))
              (beside(rectangle 10 20 "solid" "blue")
                     (rectangle 20 30 "solid" "red")
                     empty-image))

;(define (layout-images loi) empty-image) ;stub

(define (layout-images loi)
  (cond [(empty? loi) empty-image]
        [else
         (beside (first loi)
                 (layout-images (rest loi)))]))

;; ListOfImage -> ListOfImage
;; sort images in increasing order of size
(check-expect (sort-images empty) empty)
(check-expect (sort-images (cons (rectangle 20 30 "solid" "red")
                                 (cons (rectangle 10 20 "solid" "blue")
                                       empty)))
              (cons (rectangle 10 20 "solid" "blue")
                    (cons (rectangle 20 30 "solid" "red")
                          empty)))
(check-expect (sort-images (cons (rectangle 20 30 "solid" "red")
                                 (cons (rectangle 10 20 "solid" "blue")
                                       (cons (rectangle 5 15 "solid" "green")
                                             empty))))
              (cons (rectangle 5 15 "solid" "green")
                    (cons (rectangle 10 20 "solid" "blue")
                          (cons (rectangle 20 30 "solid" "red")
                                empty))))

;(define (sort-images loi) loi) ;stub

(define (sort-images loi)
  (cond [(empty? loi) empty]
        [else
         (insert (first loi)
                 (sort-images (rest loi)))])) ;result of natural recursion will be sorted

;; Image ListOfImage -> ListOfImage
;; produce new list, with image in proper place in list (increasing size)
;; ASSUME: lst is already sorted
;; !!! (rename images used so far and used I1, I2 etc.

;(define (insert img loi) loi)

(define (insert img loi)
  (cond [(empty? loi) (cons img empty)]
        [else
         (if (larger? img (first loi))  ; knowledge domain shift
             (cons (first loi)
                   (insert img
                           (rest loi)))
             (cons img loi))]))

;; Image Image -> Boolean
;; produce true if image 1 is larger than image 2 by area
;; !!!
(define (larger? img1 img2) true)














