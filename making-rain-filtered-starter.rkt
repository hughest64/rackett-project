;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname making-rain-filtered-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 3)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
            (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
            (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position
(check-expect (handle-mouse empty 10 30 "button-down")
              (cons(make-drop 10 30) empty))
(check-expect (handle-mouse (cons (make-drop 10 20)
                                  (cons (make-drop 3 6)
                                        empty)) 50 60 "button-down")
              (cons (make-drop 50 60)
                    (cons (make-drop 10 20)
                          (cons (make-drop 3 6)
                                empty))))

(check-expect (handle-mouse LOD2 50 60 "drag") LOD2)

;(define (handle-mouse lod x y mevt) empty) ; stub

(define (handle-mouse lod x y mevt)
  (cond [(mouse=? mevt "button-down") (cons (make-drop x y) lod)]
        [else
         lod]))

;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops
(check-expect (next-drops empty) empty)
(check-expect (next-drops (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))
              (cons (make-drop 10 (+ 20 SPEED)) (cons (make-drop 3 (+ 6 SPEED)) empty)))
(check-expect (next-drops (cons (make-drop 30 40) (cons (make-drop 3 0) empty)))
              (cons (make-drop 30 (+ 40 SPEED)) empty))

;(define (next-drops lod)empty) ; stub

(define (next-drops lod)
  (cond [(empty? lod) empty]
        [(bottom? (first lod))
         (next-drops (rest lod))]
        [else
         (cons (one-drop (first lod))
               (next-drops (rest lod)))]))

;; Drop -> Boolean
;; produce the y coord of a Drop
(check-expect (bottom? (make-drop 20 20)) false)
(check-expect (bottom? (make-drop 50 0)) true)
(check-expect (bottom? (make-drop 100 -5)) true)

;(define (bottom? d) 0);stub

(define (bottom? d)
  (<= (drop-y d) 0))

;; Drop -> Drop
;; Change the x y coord of a given drop
(check-expect (one-drop (make-drop 20 30)) (make-drop 20 (+ 30 SPEED)))
(check-expect (one-drop (make-drop 50 0)) (make-drop 50 0))

;(define (one-drop d) (make-drop 0 0)) ;stub

(define (one-drop d)
  (if (> (drop-y d) 0)
      (make-drop (drop-x d) (+ (drop-y d) SPEED))
      d))

;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (render-drops empty) MTS)
(check-expect (render-drops (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))
              (place-image DROP 10 20 
                           (place-image DROP 3 6 
                                        MTS)))
(check-expect (render-drops (cons (make-drop 50 0) (cons (make-drop 3 6) empty)))
              (place-image DROP 3 6 MTS))
(check-expect (render-drops (cons (make-drop 50 20) (cons (make-drop 3 -3) empty)))
              (place-image DROP 50 20 MTS))

;(define (render-drops lod) MTS) ; stub

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [(bottom? (first lod))
         (render-drops (rest lod))]
        [else
         (render-drop (first lod)
                      (render-drops (rest lod)))]))

;; Drop Image -> Image
;; procude a composite image of drops on MTS from a listofimages
(check-expect (render-drop (make-drop 10 20) MTS)
              (place-image DROP 10 20 MTS))
(check-expect (render-drop (make-drop 50 60) (place-image DROP 10 20 MTS))
              (place-image DROP 50 60 (place-image DROP 10 20 MTS)))

;(define (render-drop d img) empty-image) ;stub

(define (render-drop d img)
  (place-image DROP (drop-x d) (drop-y d) img))
