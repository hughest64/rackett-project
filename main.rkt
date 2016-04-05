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

(define SHP "radial-star")  ;shape of ballon
(define PTS 10)             ;# of points on exploded ballon
(define MIN-R 10)           ;minimum radius of ballon
(define MAX-R 100)          ;maximum radius of ballon3
(define SLD "solid")        ;image type

;; =================
;; Data definitions:

(define-struct ballon (SHP PTS R1 R2 SLD C))
;; BALLOON is (make-ballon (String Natural Natural[MIN-R, MAX-R] Natural[MIN-R, MAX-R] String String))
;; interp.  (make-ballon (SHP PTS R1 R2 C)) is a balloon with 
;;          SHP is the shape of the ballon (radial-star)
;;          PTS is the number of points on the exploded balloon (10)
;;          R1 is the inner diamter
;;          R2 is the outter diameter
;;          SLD is a solid shape
;;          C is the color of the balloon                              ;THIS WILL NEED TO BE AN ENUMERATION !!!
(define B1 (make-balloon (SHP PTS MIN MIN SLD "red")))
(define B2 (make-balloon (SHP PTS MAX MAX SLD "blue")))
(define B3 (make-balloon (SHP PTS 20  20  SLD "green")))
(define B4 (make-balloon (SHP PTS MAX MIN SLD "purple")))

(define (fn-for-baloon B)
  (... (balloon-SHP B)   ;String
       (balloon-PTS B)   ;Natural
       (balloon-R1 B)    ;Natural[MIN, MAX]
       (balloon-R2 B)    ;Natural[MIN, MAX]
       (balloon-SLD)     ;String
       (balloon-C B)))   ;String

;; Template rules used:
;;  - Compound 6 cases

;; =================
;; Functions:

;; BALLON -> BALLOON
;; start the world with (make-balloon ...)
;; 
(define (main ws)
  (big-bang ws                   ; WS
            (on-tick   tock)     ; WS -> WS
            (to-draw   render)   ; WS -> Image
;           (on-mouse  ...)      ; WS Integer Integer MouseEvent -> WS  ;leave out of the first build
            

;; WS -> WS
;; produce the next ...
;; !!!
(define (tock ws) ...)


;; WS -> Image
;; render ... 
;; !!!
(define (render ws) ...)
