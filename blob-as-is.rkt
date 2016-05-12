;Consider a test tube filled with solid blobs and bubbles.  Over time the
;solids sink to the bottom of the test tube, and as a consequence the bubbles
;percolate to the top.  Let's capture this idea in BSL.
;
;Complete the design of a function that takes a list of blobs and sinks each
;solid blob by one. It's okay to assume that a solid blob sinks past any
;neighbor just below it.

(check-expect (sink (cons "bubble" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))


#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))

;; ListOfBlob -> ListOfBlob
;; produce a list of blobs that sinks the given solid blobs by one
;; !!!
;(define (sink lob) empty) ; stub

(define (sink lob)
  (cond [(empty? lob) empty]
        [else
         (insert (which-blob (first lob))
                 (sink (rest lob)))])) ; must produce a list

;; Blob ListOfBlob -> ListOfBlob
;; move the first bubble to the front of the list
;; !!!
;(define (insert b lob) lob)

(define (insert b lob)
  (cond [(empty? lob) (cons b empty)]
        [else
         (if  (is-bubble? (which-blob (first lob)))
              (cons (first lob) (insert b (rest lob)))
              (insert b (rest lob)))]))

;; Blob -> Boolean
;; produce true if the blob is "bubble"
;; !!!
;(define (is-bubble? b) true)

(define (is-bubble? b)
  (string=? b "bubble"))

