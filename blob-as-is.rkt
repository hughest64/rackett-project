;Consider a test tube filled with solid blobs and bubbles.  Over time the
;solids sink to the bottom of the test tube, and as a consequence the bubbles
;percolate to the top.  Let's capture this idea in BSL.
;
;Complete the design of a function that takes a list of blobs and sinks each
;solid blob by one. It's okay to assume that a solid blob sinks past any
;neighbor just below it.

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))

;; ListOfBlob -> ListOfBlob
;; produce a list of blobs that sinks the given solid blobs by one
(check-expect (sink empty) empty)
(check-expect (sink (cons "bubble" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "bubble" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "bubble" (cons "solid" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid"
                          (cons "solid"
                                (cons "bubble" (cons "bubble" empty)))))
              (cons "bubble" (cons "solid" 
                                   (cons "solid" (cons "bubble" empty)))))

;(define (sink lob) empty) ; stub

(define (sink lob)
  (cond [(empty? lob) empty]
        [else
         (insert (which-blob (first lob))
                 (sink lob))])) ; must produce a list

;; Blob ListOfBlob -> ListOfBlob
;; move the first bubble to the front of the list
(check-expect (insert "solid" empty) (cons "solid" empty))
(check-expect (insert "solid" (cons "bubble" (cons "solid" (cons "bubble" empty))))
             (cons "bubble" (cons "solid" (cons "bubble" (cons "solid" empty)))))
(check-expect (insert "solid" (cons "bubble" (cons "bubble" (cons "solid" empty))))
             (cons "bubble" (cons "solid" (cons "bubble" (cons "solid" empty)))))

;(define (insert b lob) lob)

(define (insert b lob)
  (cond [(empty? lob) (cons b empty)]
        [else
         (if  (is-bubble? (which-blob (first lob)))
              (cons (first lob) (insert b (rest lob)))
              (cons b lob))]))

;; Blob -> Boolean
;; produce true if the blob is "bubble"
(check-expect (is-bubble? "bubble") true)
(check-expect (is-bubble? "solid") false)

;(define (is-bubble? b) true)

(define (is-bubble? b)
  (string=? b "bubble"))


;; Blob -> String 
;; Produce the type of a given blob
(check-expect (which-blob "solid") "solid")
(check-expect (which-blob "bubble") "bubble")

;(define (which-blob b) " ")

(define (which-blob b)
  (cond [(string=? b "solid") "solid"]
        [(string=? b "bubble") "bubble"]))


