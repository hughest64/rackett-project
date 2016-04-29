;;; constants:

(define WIDTH 400)
(define HEIGHT 200)

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define SPEED 20)

(define BEAR-IMG (square 25 "solid" "blue"))

(define MTS (empty-scene WIDTH HEIGHT "lightblue"))

;; data definitions

(define-struct bear (s x y))
;; Bear is (make-bear Number Number[0 WIDTH] Number [0 HEIGHT])
;; interp. (make-bear s x y) is a bear where
;; - s is the speed of roatation of a bear (angle in degrees)
;; - x is the horzontal position of the bear
;; - y is the verital position of the bear

(define B1 (make-bear 0 0 0))          ; bear at upper right hand
(define B2 (make-bear 45 CTR-X CTR-Y)) ; bear at center of MTS angle of 45 degrees

;(define (fn-for-bear s x y)
;  (... (bear-s b)
;       (bear-x b)
;       (bear-y b)))

;; template rules:
;; - compound: 3 fields



;; ListOfBear is one of:
;; - empty
;; - (cons Bear ListOfBear)

(define LOB1 empty)
(define LOB2 (cons (make-bear 0 CTR-X CTR-Y) 
                   (cons (make-bear 45 200 300) empty)))

;(define (fn-for-lob lob)
; (cond [(empty? lob) (...)]
;        [else
;         (... (fn-for-bear (first lob))
;              (fn-for-lob (rest lob)))]))

;; template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first lob) is Bear
;; - self-reference: (rest lob) is ListOfBear


;; function definitions

;; ListOfBear -> ListOfBear
;; start the world with (main empty)
;; 
(define (main lob)
  (big-bang lob                         ; WS
            (on-tick   all-bears)       ; WS -> WS
            (to-draw   render-bears)    ; WS -> Image
            (on-mouse  handle-mouse)))  ; WS Integer Integer MouseEvent -> WS



;; ListOfBear -> ListOfBear
;; produce all of the bears in ListOfBear
(check-expect (all-bears empty) empty)
(check-expect (all-bears (cons (make-bear 0 0 0) empty))
              (cons (make-bear (+ SPEED 0) 0 0 ) empty))
(check-expect (all-bears (cons (make-bear 20 CTR-X CTR-Y)
                               (cons (make-bear 90 100 80)
                                     empty)))
              (cons (make-bear (+ SPEED 20) CTR-X CTR-Y)
                    (cons (make-bear (+ SPEED 90) 100 80)
                          empty)))


;(define (all-bears lob) empty) ;stub

(define (all-bears lob)
  (cond [(empty? lob) empty]
        [else
         (cons (next-bear (first lob))
               (all-bears (rest lob)))]))


;; Bear -> Bear
;; produce the current angle, x-pos, and y-pos of a bear
(check-expect (next-bear (make-bear 0 0 0))
              (make-bear (+ SPEED 0) 0 0 ))
(check-expect (next-bear (make-bear 100 250 300))
              (make-bear (+ SPEED 100) 250 300))

;(define (next-bear b) (make-bear 0 0 0)) ;stub

(define (next-bear b)
  (make-bear (+ SPEED (bear-s b))
             (bear-x b)
             (bear-y b)))




;; ListOfBear -> Image
;; produce the correct image of bear on the correct image
(check-expect (render-bears empty) MTS)
(check-expect (render-bears (cons (make-bear 0 0 0) empty))
              (place-image (rotate 0 BEAR-IMG) 0 0 MTS))
(check-expect (render-bears (cons (make-bear 20 100 200)
                                  (cons (make-bear 100 75 300)
                                        empty)))
              (place-image (rotate 20 BEAR-IMG) 100 200
                           (place-image (rotate 100 BEAR-IMG) 75 300
                                        MTS)))

;(define (render-bears lob) MTS) ;stub

(define (render-bears lob)
  (cond [(empty? lob) MTS]
        [else
         (single-img (first lob) (render-bears (rest lob)))]))

;; Bear Image -> Image
;; produce a single bear on MTS
(check-expect (single-img (make-bear 0 0 0) MTS)
              (place-image (rotate 0 BEAR-IMG) 0 0 MTS))
(check-expect (single-img (make-bear 45 20 30) MTS)
              (place-image (rotate 45 BEAR-IMG) 20 30 MTS))


;(define (single-img b img) MTS) ;stub

(define (single-img b img)
  (place-image (rotate (bear-s b) BEAR-IMG)
               (bear-x b)
               (bear-y b)
               img))

;; ListOfBear Integer Integer MouseEvent-> ListOfBear
;; produce a new bear at the x y coord of the mouse
(check-expect (handle-mouse empty 20 30 "button-down")
              (cons (make-bear 0 20 30) empty))
(check-expect (handle-mouse empty 100 200 "button-up") 
              empty)
                            
                                  
;(define (handle-mouse lob x y me) empty) ; stub


(define (handle-mouse lob x y me)
  (cond [(mouse=? me "button-down") (cons (make-bear 0 x y) lob)]
        [else
         lob]))
