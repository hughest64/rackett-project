;; constants:

(define WIDTH 400)
(define HEIGHT 200)

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define SPEED 20)

(define BEAR-IMG (square 25 "solid" "brown"))

(define MTS (empty-scene WIDTH HEIGHT "lightblue"))

;; data definitions:

(define-struct bear (s x y))
;; Bear (make-bear b x y)
;; interp. the angle and vertical/horizontal positions of bear       

(define B1 (make-bear 0 200 400))
(define B2 (make-bear 45 12 50))

;(define (fn-for-bear b)
;  (... (bear-s b)
;       (bear-x b)
;       (bear-y b)))

;; ListOfBear is one of:
;; - empty
;; - (cons Bear ListOfBear)
;; - a list of bears

(define LOB1 empty)
(define LOB2 (cons B1 (cons B2 empty)))

;(define (fn-for-lob lob)
;  (cond [(empty? lob) (...)]
;        [else
;         (fn-for-bear b (bear-s b)
;         (fn-for-los (rest los)))]))

;; Template rules used:
;; - atomic distinct: empty
;; - compund (cons Bear ListOfBear)
;; - reference rule: (first lob) is Bear
;; - self-refernce rule: (rest lob) is ListOfBear



;; Function defintions

; Bear -> Bear
;; start the world with (main (make-bear 0 CTR-X CTR-Y))
;; 
(define (main b)
  (big-bang b                   
            (on-tick   next-bear)    
            (to-draw   render-bear)   
            (on-mouse  handle-mouse)))

;; Bear -> Bear
;; create the next angle of bear
(check-expect (next-bear (make-bear 0 CTR-X CTR-Y)) 
              (make-bear (+ SPEED   0) CTR-X CTR-Y))
(check-expect (next-bear (make-bear 200 CTR-X CTR-Y)) 
              (make-bear (+ SPEED 200) CTR-X CTR-Y))

;(define (next-bear b) (make-bear 0 0 0)) ;stub

(define (next-bear b)
  (make-bear (+ (bear-s b) SPEED) 
             (bear-x b) 
             (bear-y b)))



;; Bear -> Image
;; produce the proper image of Bear on MTS
(check-expect (render-bear (make-bear 45 10 200))
              (place-image (rotate 45 BEAR-IMG) 10 200 MTS))                          
;(define (render-bear b) (square 0 "solid" "white")) ;stub

(define (render-bear b)
  (place-image (rotate (bear-s b) BEAR-IMG) 
               (bear-x b) 
               (bear-y b) MTS))


;; Bear Integer Integer MouseEvent -> Image
;; create a new Bear at current x/y coord of the mouse when clicked
(check-expect (handle-mouse (make-bear 45 0 0) 20 100 "button-down")
              (make-bear 0 20 100))

;(define (handle-mouse b x y me) (make-bear 0 0 0))

(define (handle-mouse b x y me)
  (cond [(mouse=? me "button-down") 
         (make-bear 0 x y)]
        [else
         b]))

;; ListOfString -> ListOfBear
;; produce an additonal bear on MTS where clicked
(check-expect (add-bear empty) (make-bear 0 0 0))
(check-expect (add-bear LOB2) (cons B1 (cons B2 empty)))

;(define (add-bear lob) LOB1) ;stub

(define (add-bear lob)
  (cond [(empty? lob) 
         (make-bear 0 0 0)]
        [else
         (cons (next-bear (first lob))
               (add-bear (rest lob)))]))
