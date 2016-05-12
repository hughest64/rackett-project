;; Blob is one of:
;; - "solid"
;; - "bubble"
;; interp.  a gelatinous blob, either a solid or a bubble
;; Examples are redundant for enumerations
#;
(define (fn-for-blob b)
  (cond [(string=? b "solid") (...)]
        [(string=? b "bubble") (...)]))

;; Template rules used:
;; - one-of: 2 cases
;; - atomic distinct: "solid"
;; - atomic distinct: "bubble"


;; ListOfBlob is one of:
;; - empty
;; - (cons Blob ListOfBlob)
;; interp. a sequence of blobs in a test tube, listed from top to bottom.
(define LOB0 empty) ; empty test tube
(define LOB2 (cons "solid" (cons "bubble" empty))) ; solid blob above a bubble

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))

;; Template rules used
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first lob) is Blob
;; - self-reference: (rest lob) is ListOfBlob

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

(check-expect (sink (cons "solid"(cons "solid"
                                       (cons "bubble" (cons "bubble" empty)))))
              (cons "bubble" (cons "solid" 
                                   (cons "solid" (cons "bubble" empty)))))

;(define (sink lob) empty) ; stub

(define (sink lob)
  (cond [(empty? lob) empty]
        [else
         (insert (blob-type (first lob))
                 (sink (rest lob)))]))

;; Blob ListOfBlob -> ListOfBlob
;; product a new list with blobs in the proper order
(check-expect (insert "bubble" empty) (cons "bubble" empty))
(check-expect (insert "solid" (cons "bubble" empty))
              (cons "solid" (cons "bubble" empty)))

;(define (insert b lob) lob)

(define (insert b lob)
  (cond [(empty? lob) (cons b empty)]
        [else
         (if (is-bubble? (first lob))
             (insert b (rest lob))
             (cons b lob))]))

;; Blob -> Boolean
;; produce true if the blob type is "bubble"
(check-expect (is-bubble? " ") false)
(check-expect (is-bubble? "bubble") true)

;(define (is-bubble? b) true) ;stub

(define (is-bubble? b)
  (string=? b "bubble"))

;; ListOfBlob -> Blob
;; determine which blob it is
(check-expect (blob-type empty) " ")
(check-expect (blob-type (cons "bubble" empty)) "bubble")
(check-expect (blob-type (cons "solid" (cons "bubble" empty))) "solid")

;(define (blob-type lob) " ") ;stub

(define (blob-type lob)
  (cond [(empty? lob) " "]
        [else
         (blob-type (rest lob))]))
