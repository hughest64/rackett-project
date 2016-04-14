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
            (on-key    handle-key)     ; Grapple KeyEvent -> WS
            (stop-when last-world)))

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
  (make-grapple (+ (grapple-a b) ROTATE-RATE)
                (+ (grapple-s b) GROW-RATE)))

;; Grapple -> Image
;; produe the correct image of grapple on MTS
(check-expect (render-grapple (make-grapple 45 20)) 
              (place-image (rotate (remainder 45 360) (square 20 "solid" "red")) CTR-Y CTR-X MTS))

;(define (render-grapple b) (square 15 "solid" "blue")) ;stub

(define (render-grapple b)
        (place-image (rotate (remainder (grapple-a b) 360) (square (grapple-s b) "solid" "red")) 
                     CTR-Y 
                     CTR-X 
                     MTS))
     

;; Grapple KeyEvent -> Grapple
;; reset the program to 0 0 when space bar is pressed
(check-expect (handle-key (make-grapple 12 60) " ") (make-grapple 0 0))
(check-expect (handle-key (make-grapple 50 23) "a") (make-grapple 50 23))

;(define (handle-key b ke) b) ;stub
 

(define (handle-key b ke)  
 (cond [(key=? ke " "))    
      (make-grapple 0 0)]
      [else b]))
      
;; Grapple -> Boolean
;; stop the program when the grapple is > 800 pixels
(check-expect (last-world (make-grapple 250 50)) false)
(check-expect (last-world (make-grapple 36 850)) true)
(check-expect (last-world (make-grapple 100 50)) false)

;(define (last-world b) false) ;stub
     
(define (last-world b)  
  (if (< 800 (grapple-s b))
      true
      false))
