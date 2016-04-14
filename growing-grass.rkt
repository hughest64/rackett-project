;; A blade of grass that grows and resests to the right when any key is pressed

;; constants:

(define WIDTH 400)
(define HEIGHT 200)

(define MTS (empty-scene WIDTH HEIGHT "deepskyblue"))
(define RADIUS 20) ; the raidus of the blade and the distance the blade will move to the right

(define BLADE-COLOR "green")
(define GROW-RATE -1)
(define X-POS 15)
(define Y-START 0)


;; data definitions:

(define-struct blade (x y))
;; Blade is (make-blade Natural Natural)
;; interp.  (make-blade x y)
;;          - x is the current x position of the blade in pixels
;;          - y is the current height of the blade

(define B1 (make-blade 15   0))
(define B2 (make-blade 30 -20))
(define B3 (make-blade 45 -40))

(define (fn-for-blade b)
  (... (blade-x b) (blade-y b)))

;; template rules used:
;; - compound 2 fields

;; =================
;; Functions:

;; Blade -> Blade
;; start the world with (main (make-blade X-POS 0))
;; 
(define (main b)
  (big-bang b                          ; Blade
            (on-tick   next-blade)     ; Blade -> Blade
            (to-draw   render-blade)   ; Blade -> Image
            (on-key    handle-key)))   ; Blade KeyEvent -> Blade

;; Blade -> Blade
;; produce the next Blade
(check-expect (next-blade (make-blade 15   0)) (make-blade 15  1))
(check-expect (next-blade (make-blade 30  50)) (make-blade 30 51))
(check-expect (next-blade (make-blade 50  20)) (make-blade 50 21))

;(define (next-blade b) b) ;stub

;template from blade

(define (next-blade b)
  (make-blade (blade-x b) (+ (blade-y b) 1)))


;; Blade -> Image
;; render the correct image of Blade at the correct x-position on MTS
(check-expect (render-blade (make-blade 20  0)) 
              (place-image (rectangle RADIUS 0 "solid" BLADE-COLOR)  20 HEIGHT MTS))
(check-expect (render-blade (make-blade 45 20)) 
              (place-image (rectangle RADIUS 20 "solid" BLADE-COLOR) 45 HEIGHT MTS))
(check-expect (render-blade (make-blade 60 30)) 
              (place-image (rectangle RADIUS 30 "solid" BLADE-COLOR) 60 HEIGHT MTS))

;(define (render-blade b) (circle 10 "solid" "red")) ;stub

;template from Blade

(define (render-blade b)
  (place-image (rectangle RADIUS (blade-y b) "solid" BLADE-COLOR) (blade-x b) HEIGHT MTS))

;; Blade KeyEvent -> Blade
;; reset the blade and move it to the right by X-POS
(check-expect (handle-key (make-blade 20 100) " ")
              (make-blade (+ X-POS 20) Y-START))
;(check-expect (handle-key (make-blade 50 0) "H")
;            (make-blade (+ X-POS 50) Y-START))

;(define (handle-key b ke) b) ;stub

;template from blade

(define (handle-key b ke)
  (cond [(key=? " " ke) 
         (make-blade (+ X-POS (blade-x b)) Y-START)]
        [else b]))
