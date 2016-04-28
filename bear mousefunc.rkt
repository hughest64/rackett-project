;; data definitions:

;(define (fn-for-bear b)
;  (... b))

(define-struct bear (b x y))
;; Bear (make-bear bs bx by)
;; interp. the angle and vertical/horizontal positions of bear       

(define B1 (make-bear 0 200 400))
(define B2 (make-bear 45 12 50))


;; Function defintions

;; Bear Integer Integer MouseEvent -> Image
;; create a new Bear at current x/y coord of the mouse when clicked
(check-expect (handle-mouse (make-bear 45 0 0) 20 100 "button-down")
              (make-bear 45 20 100))

;(define (handle-mouse b x y me) (make-bear 0 0 0))

(define (handle-mouse b x y me)
  (cond [(mouse-event? me "button-down")
         (make-bear (bear-b b) x y)]
        [else b]))
