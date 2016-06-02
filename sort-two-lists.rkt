;; ListOfNatural is one of:
;;  - empty
;;  - cons (Natrual ListOfNatural)
;; interp. A list of Naturals

(define LS0 empty)
(define LS1 (list 1))
(define LS2 (list 1 2))
(define LS3 (list 3 6 9))

#;
(define (fn-forlon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; ListOfNatural ListOfNatural -> ListOfNatural
;; produce one sorted (in numerical order) list from two sorted lists
(check-expect (big-list empty empty) empty)
(check-expect (big-list empty (list 1 2 3)) (list 1 2 3))
(check-expect (big-list (list 4 5 6) empty) (list 4 5 6))
(check-expect (big-list (list 1) (list 2)) (list 1 2))

(check-expect (big-list (cons 2 empty) (cons 1 empty))
              (cons 1 (cons 2 empty)))

(check-expect (big-list (list 1 4) (list 2 3)) (list 1 2 3 4))
(check-expect (big-list (list 2 3) (list 1 4)) (list 1 2 3 4))
(check-expect (big-list (list 2) (list 2)) (list 2 2))
(check-expect (big-list (list 1 2) (list 2)) (list 1 2 2))
#;
(define (big-list lsta lstb) empty)

(define (big-list lsta lstb)
  (cond [(empty? lsta) lstb]
        [(empty? lstb) lsta]
        [else
         (if (< (first lsta) (first lstb))
             (cons (first lsta) (big-list (rest lsta) lstb))
             (cons (first lstb) (big-list (rest lstb) lsta)))]))
