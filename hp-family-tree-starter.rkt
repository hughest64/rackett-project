;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hp-family-tree-starter) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

;; hp-family-tree-starter.rkt

;In this problem set you will represent information about descendant family 
;trees from Harry Potter and design functions that operate on those trees.
;
;To make your task much easier we suggest two things:
;  - you only need a DESCENDANT family tree
;  - read through this entire problem set carefully to see what information 
;    the functions below are going to need. Design your data definitions to
;    only represent that information.
;  - you can find all the information you need by looking at the individual 
;    character pages like the one we point you to for Arthur Weasley.


;PROBLEM 1:
;
;Design a data definition that represents a family tree from the Harry Potter 
;wiki, which contains all necessary information for the other problems.  You 
;will use this data definition throughout the rest of the homework.

(define-struct weasley (name patronus wand low))
;; Weasley is (make-weasley String String String)
;;          - name is a character name
;;          - patronus is a characters patronus type
;;          - wand is the material from which a characters wand is made
;;          - low is a ListOfWeasley
;;          - NOTE - the String " " is used a value is not specified.
;; interp. A member of the weasley family with name, patronus type andy descendants

;; ListOfWeasley is one of:
;; - empty
;; - (cons (weasley (make-weasley n p w low))) 
;; interp. a list of descendants from Arthur Weasley

(define G1 (make-weasley "lilly" " " " " empty))
(define G2 (make-weasley "Albus" " " " " empty))
(define G3 (make-weasley "James" " " " " empty))
(define G4 (make-weasley "Victoire" " " " " empty))
(define C5 (make-weasley "Bill" " " " " (list G4)))
(define C6 (make-weasley "Chalrie" " " "unicorn tail" empty))
(define C7 (make-weasley "Percy" "?" "?" empty))
(define C8 (make-weasley "Fred" "?" "?" empty))
(define C9 (make-weasley "George" "?" "?" empty))
(define C10 (make-weasley "Ron" "?" "?" empty))
(define C11 (make-weasley "Ginny" "?" "?" (list G1 G2 G3)))
(define ARTHUR (make-weasley "Arthur" "weasel" " " (list G1 G2 G3 G4 C5 C6 C7 C8 C9 C10 C11)))
#;
(define (fn-for-for-weasly w)
	(... (weasley-name w)              ;string
       	     (weasley-patronus w)          ;string
             (weasley-wand w)              ;string
             (fn-for-low (weasly-low w))))
#;      
(define (fn-for-low low)
  (cond [(empty? low) (...)]
        [else
         (... (fn-for-weasley (first low))
              (fn-for-low (rest low)))]))

;PROBLEM 2: 
;
;Define a constant named ARTHUR that represents the descendant family tree for 
;Arthur Weasley. You can find all the infomation you need by starting 
;at: http://harrypotter.wikia.com/wiki/Arthur_Weasley.
;
;You must include all of Arthur's children and these grandchildren: Lily, 
;Victoire, Albus, James.
;
;
;Note that on the Potter wiki you will find a lot of information. But for some 
;people some of the information may be missing. Enter that information with a 
;special value of "" (the empty string) meaning it is not present. Don't forget
;this special value when writing your interp.


;PROBLEM 3:
;
;Design a function that produces a pair list (i.e. list of two-element lists)
;of every person in the tree and his or her patronus. For example, assuming 
;that HARRY is a tree representing Harry Potter and that he has no children
;(even though we know he does) the result would be: (list (list "Harry" "Stag")).
;
;You must use ARTHUR as one of your examples.

;; Weasley -> ListOfPairs
;; ListOfWeasley -> ListOfPairs
;; produce a list of every  character with their name and patronus
(check-expect (name-pair--low empty) (list ))
(check-expect (name-pair--weasley G1) (list (list "Lilly" " ")))

(check-expect (name-pair--weasley C5) (list (list "Bill" " ") (list "Victoire" " ")))

(check-expect (name-pair--weasley ARTHUR) (list (list "Arthur" "weasel") 
                                                (list "Bill" " ")
                                                (list "Victoire" " ")
                                                (list "Charlie" "?")
                                                (list "Percy" "?")
                                                (list "Fred" "?")
                                                (list "George" "?")
                                                (list "Ron" "?")
                                                (list "Ginny" "?")
                                                (list "James" " ")
                                                (list "Albus" " ")
                                                (list "Lilly" " ")))

;(define (name-pair--weasley w) (list ))
;(define (name-pair--low low) (list ))

(define (name-pair--weasley w)
  (cons (list (weasley-name w) (weasley-patronus w))
        (name-pair--low (weasley-low w))))

(define (name-pair--low low)
  (cond [(empty? low) (list )]
        [else
         (append (name-pair--weasley (first low))
                 (name-pair--low (rest low)))]))


;PROBLEM 4:
;
;Design a function that produces the names of all descendants of a given person 
;whose wands are made of a given material. 
;
;You must use ARTHUR as one of your examples.

