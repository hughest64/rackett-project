(define WIDTH 700)
(define HEIGHT WIDTH)
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))


(define GROW-RATE 1)
(define ROTATE-RATE 15)

(define-struct grapple (a s))

;; Grapple -> Grapple
;; start the world with (main (make-grapple 0 0))
;; 
(define (main b)
  (big-bang b                           ; Grapple
            (on-tick   next-grapple)     ; Grapple -> Grapple
            (to-draw   render-grapple)   ; Grapple -> Image                      
            (on-key    handle-key)))     ; Grapple KeyEvent -> WS

;; Grapple -> Grapple
;; produce then next size grapple at the next angle

(check-expect (next-grapple  (make-grapple 0 0)) 
              (make-grapple 1 15))
(check-expect (next-grapple  (make-grapple 2 30)) 
              (make-grapple (+ GROW-RATE 2) (+ ROTATE-RATE 30)))

;(define (next-grapple b) b) ;stub

;(define (fn-for-grapple b)  ;template
; (... (grapple-a b)         ;natural
;      (grapple-s b)))       ;natural


(define (next-grapple b)
  (cond [(and (number? (grapple-a b)) (number? (grapple-s b)))
         (make-grapple (+ GROW-RATE (grapple-a b)) (+ ROTATE-RATE (grapple-s b)))]
        [else b]))

;; Grapple -> Image
;; produe the correct image of grapple on MTS
(check-expect (render-grapple (make-grapple 45 20)) 
              (place-image (rotate 45 (square 20 "solid" "red")) CTR-Y CTR-X MTS))

;(define (render-grapple b) (square 15 "solid" "blue")) ;stub

(define (render-grapple b)  
 (if (and (number? (grapple-a b)) (number? (grapple-s b))) 
        (place-image (rotate (grapple-a b) (square (grapple-s b) "solid" "red")) 
                     CTR-Y 
                     CTR-X 
                     MTS)
     b))
     

;; Grapple KeyEvent -> Grapple
;; reset the program to 0 0 when space bar is pressed
(check-expect (handle-key (make-grapple 12 60) " ") (make-grapple 0 0))
(check-expect (handle-key (make-grapple 50 23) "a") (make-grapple 50 23))

;(define (handle-key b ke) b) ;stub
 

(define (handle-key b ke)  
 (cond [(and (number? (grapple-a b)) (number? (grapple-s b)) (key=? ke " "))    
      (make-grapple 0 0)]
      [else b]))
