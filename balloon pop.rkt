;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |balloon pop|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A "balloon" tha changes colors and increases in size until it pops

;; =================
;; Constants:
(define WIDTH 400)
(define HEIGHT 200)

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define MTS (empty-scene WIDTH HEIGHT))

(define INC 5)              ;the rate at which the ballon increases in size
(define PTS 10)             ;# of points on exploded balloon
(define MIN-R 10)           ;minimum radius of balloon
(define MAX-R 100)          ;maximum radius of balloon
(define SLD "solid")        ;image type

;; =================
;; Data definitions:

(define-struct balloon (PTS R1 R2 SLD C))
;; BALLOON is (make-balloon (Natural Natural[MIN-R, MAX-R] Natural[MIN-R, MAX-R] String String))
;; interp. (make-balloon (PTS R1 R2 SLD C)) is a balloon with 
;;          PTS is the number of points on the exploded balloon (10)
;;          R1 is the inner diamter
;;          R2 is the outer diameter
;;          SLD is a solid shape
;;          C is of of:
;;          - "red"
;;          - "blue"
;;          - "green"
;;          - "purple"
(define B1 (make-balloon PTS MIN-R MIN-R SLD "red"))
(define B2 (make-balloon PTS MAX-R MAX-R SLD "blue"))
(define B3 (make-balloon PTS 20    20    SLD "green"))
(define B4 (make-balloon PTS MAX-R MIN-R SLD "purple"))
#;
(define (fn-for-baloon B)           ; !!!
  (... (balloon-SHP B)    ;String
       (balloon-PTS B)    ;Natural
       (balloon-R1  B)    ;Natural[MIN-R, MAX-R]
       (balloon-R2  B)    ;Natural[MIN-R, MAX-R]
       (balloon-SLD B)    ;String
       (balloon-C   B)))  ;String


;; Template rules used:
;;  - Compound 5 cases

;; =================
;; Functions:

;; BALLOON -> BALLOON
;; start the world with (make-balloon PTS MIN-R MIN-R SLD "red")
;; 
(define (main B)
  (big-bang B                             ; BALLOON
            (on-tick   next-balloon .5)   ; BALLOON -> BALLOON
            (to-draw   render-balloon)    ; BALLOON -> Image
            (on-key    key-handler)       ; BALLOON -> KeyEvent
            (on-mouse  mouse-handler)))   ; BALLOON Integer Integer MouseEvent -> BALLOON
            

;; BALLOON -> BALLOON
;; produce the next inner radius, outer radius, and color of BALLOON
(check-expect (next-balloon (make-balloon PTS MIN-R MIN-R SLD "red"))    (make-balloon PTS 15    15    SLD "blue"))
(check-expect (next-balloon (make-balloon PTS 30    30    SLD "blue"))   (make-balloon PTS 35    35    SLD "green"))
(check-expect (next-balloon (make-balloon PTS 50    50    SLD "green"))  (make-balloon PTS 55    55    SLD "purple"))

(check-expect (next-balloon (make-balloon PTS MAX-R MAX-R SLD "purple")) (make-balloon PTS MIN-R MAX-R SLD "red"))

;(define (next-balloon B) B)  ;stub

;template is from BALLOON !!!

(define (next-balloon B)
  (cond [(and (< (balloon-R1 B) MAX-R) (< (balloon-R2 B) MAX-R))
         (make-balloon PTS (+ INC (balloon-R1 B)) (+ INC (balloon-R2 B)) SLD (choose-color B))]
        [else
         (make-balloon PTS MIN-R MAX-R SLD "red")]))

;;; BALLOON -> String
;;; choose the next color of balloon
(check-expect (choose-color (make-balloon PTS 40  40 SLD "red"))      "blue")
(check-expect (choose-color (make-balloon PTS 40  40 SLD "blue"))    "green")
(check-expect (choose-color (make-balloon PTS 33  33 SLD "green"))  "purple")
(check-expect (choose-color (make-balloon PTS 60  60 SLD "purple"))    "red")

;;(define (choose-color B) "yellow") ;stub

;;template from BALLOON

(define (choose-color B)           
  (cond [(string=? (balloon-C B) "red")     "blue"]
        [(string=? (balloon-C B) "blue")   "green"]
        [(string=? (balloon-C B) "green") "purple"]
        [else "red"]))
        
;; BALLOON -> Image
;; render the next balloon
(check-expect (render-balloon  (make-balloon PTS 50    50 SLD "red"))              
              (place-image     (radial-star  PTS 50    50 SLD "blue")   CTR-X CTR-Y MTS))
(check-expect (render-balloon  (make-balloon PTS MIN-R 15 SLD "blue")) 
              (place-image     (radial-star  PTS MIN-R 15 SLD "green")  CTR-X CTR-Y MTS))

(check-expect (render-balloon  (make-balloon PTS MAX-R MAX-R SLD "blue"))
              (place-image     (radial-star  PTS MAX-R MIN-R SLD "red") CTR-X CTR-Y MTS))

;(define (render-balloon B) MTS) ;stub

(define (render-balloon B)           
  (cond [(and (< (balloon-R1 B) MAX-R) (< (balloon-R2 B) MAX-R))
         (place-image (radial-star PTS (balloon-R1 B) (balloon-R2 B) SLD (choose-color B)) CTR-X CTR-Y MTS)]
        [else 
         (place-image (radial-star PTS MAX-R          MIN-R          SLD "red") CTR-X CTR-Y MTS)]))
         
;;  BALLOON KeyEvent -> BALLOON
;;  press the spacebar to reset the balloon to MIN-R MIN-R !!!
(check-expect (key-handler (make-balloon PTS 20 20 SLD "blue") "a") (make-balloon PTS 20 20 SLD "blue"))
(check-expect (key-handler (make-balloon PTS 30 30 SLD "blue") " ") (make-balloon PTS MIN-R MIN-R SLD "red"))

;(define (key-handler B ke) B) ;stub

(define (key-handler B ke)
  (cond [(key=? ke " " ) (make-balloon PTS MIN-R MIN-R SLD "red")]
        [else
         B]))



;; BALLOON MouseEvent -> BALLOON
;; instantly pop the ballon when you click on it
(check-expect (mouse-handler (make-balloon PTS 30 30 SLD "green") CTR-X CTR-Y "button-down")
              (make-balloon PTS MAX-R MIN-R SLD "red"))

;(define (mouse-handler B x y me) B) ;stub

;(define (mouse-handler B x y me) B) ;stub
; !!! add mouse-event so that you have to clikc on the balloon (mouse-event? B)
(define (mouse-handler B x y me)
  (cond [(mouse=? me "button-down" CTR-X CTR-Y) (make-balloon PTS MAX-R MIN-R SLD "red")]
        [else
         B]))















