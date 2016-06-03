;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname evaluate-foo-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; evaluate-foo-starter.rkt


;PROBLEM:
;
;Hand step the evaluation of (foo 3) given the definition of foo below. 
;We know that you can use the stepper to check your work - please go
;ahead and do that AFTER you try hand stepping it yourself.
;
;(define (foo n)
;  (local [(define x (* 3 n))]
;    (if (even? x)
;        n
;        (+ n (foo (sub1 n))))))

(define (foo n)

  (local [(define x (* 3 n))]
    (if (even? x)
        n
        (+ n (foo (sub1 n))))))


(local [(define x (* 3 3))]
  (if (even? x)
      3
      (+ 3 (foo (sub1 3)))))


(local [(define x 9)]
  (if (even? x)
      3
      (+ 3 (foo (sub1 3)))))


(define x 9)
(if (even? x)
    3
    (+ 3 (foo (sub1 3))))


(if (even? x)
    3
    (+ 3 (foo (sub1 3))))


(if (even? 9)
    3
    (+ 3 (foo (sub1 3))))


(+ 3 (foo (sub1 3)))

(+ 3 (foo 2))

(+ 3 2)

5
