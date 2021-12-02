

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)






(check-expect (backwards "") "")
(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "kayak") "kayak")
(define (backwards str) "") 



(define-struct customer (firstName lastName age
                                   zipCode))






  


(define C1 (make-customer "Emilia" "Krum"
                          18 01609))
(define C2 (make-customer "Cara" "Salter"
                          18 01609))
(define BLANK (make-customer "" "" 0 0))





(check-expect (birthday C1) (make-customer
                             "Emilia" "Krum"
                             19 01609))
(check-expect (birthday BLANK) (make-customer
                             "" ""
                             1 0))



  

(define (birthday cust)
  (make-customer (customer-firstName cust)
                 (customer-lastName cust)
                 (+ 1 (customer-age cust))
                 (customer-zipCode cust)))




(check-expect (name-change "John" "Smith" C2) (make-customer "John" "Smith" 18 01609))
(check-expect (name-change "Blank" "Blankerson" BLANK) (make-customer "Blank" "Blankerson" 0 0))
  

(define (name-change newFirst newLast cust)
  (make-customer newFirst newLast (customer-age cust) (customer-zipCode cust)))



(define-struct simple-img (color type area))







(define TRIANGLE (make-simple-img "red" "triangle" 10))
(define SQUARE (make-simple-img "blue" "square" 20))
(define CIRCLE (make-simple-img "grey" "circle" 20))

  



(check-expect (bigger? TRIANGLE SQUARE) false)
(check-expect (bigger? SQUARE TRIANGLE) true)
(check-expect (bigger? CIRCLE SQUARE) false)

  

(define (bigger? img1 img2)
  (> (simple-img-area img1) (simple-img-area img2)))



(check-expect (build-img TRIANGLE) (triangle (sqrt (/ 40 (sqrt 3))) "solid" "red"))
(check-expect (build-img SQUARE) (square (sqrt 20) "solid" "blue"))
(check-expect (build-img CIRCLE) (circle (sqrt (/ 20 pi)) "solid" "grey"))
  

(define (build-img img)
  (cond [(string=? "triangle" (simple-img-type img))
         (triangle (triSideLength img) "solid" (simple-img-color img))]
        [(string=? "square" (simple-img-type img))
         (square (sqrSideLength img) "solid" (simple-img-color img))]
        [(string=? "circle" (simple-img-type img))
         (circle (circRadius img) "solid" (simple-img-color img))]))



(check-within (triSideLength TRIANGLE)(sqrt (/ 40 (sqrt 3))) 0.5)
(check-within (triSideLength (make-simple-img "green" "triangle" 15)) (sqrt (/ (* 4 15) (sqrt 3))) 0.5)
(check-within (triSideLength (make-simple-img "green" "triangle" 0)) (sqrt (/ (* 4 0) (sqrt 3))) 0.5)
  

(define (triSideLength img) (sqrt (/ (* 4 (simple-img-area img)) (sqrt  3))))



(check-within (sqrSideLength SQUARE) (sqrt 20) 0.5)
(check-within (sqrSideLength (make-simple-img "green" "square" 0)) (sqrt 0) 0.5)
  

(define (sqrSideLength img) (sqrt (simple-img-area img)))




(check-within (circRadius CIRCLE) (sqrt (/ 20 pi)) 0.5)
(check-within (circRadius (make-simple-img "red" "circle" 0)) 0 0.5)
  

(define (circRadius img)
  (sqrt (/ (simple-img-area img) pi)))



(define-struct diff (type change position))

























 





(define D1 (make-diff "insert" "gompei" 1))
(define D2 (make-diff "delete" 3 1))
(define D3 (make-diff "insert" "" 1))


(check-expect (update "goat" D1) "ggompeioat")
(check-expect (update "gompei" D2) "gei")
(check-expect (update "WPI" D3) "WPI")

  

(define (update txt dif)
  (cond
    [(string=? (diff-type dif) "insert")
     (string-append (substring txt 0 (diff-position dif))
                    (diff-change dif)
                    (substring txt (diff-position dif)))]
    [(string=? (diff-type dif) "delete")
     (string-append (substring txt 0 (diff-position dif))
                    (substring txt (+ (diff-position dif) (diff-change dif))))]))


(define ARE "How are you today?")
(define WERE "How were you yesterday?")

(define ARETMP1 "How re you today?")
(define ARETMP2 "How re you day?")
(define ARETMP3 "How were you day?")

(define ARED1 (make-diff "delete" 1 4))
(define ARED2 (make-diff "delete" 2 11))
(define ARED3 (make-diff "insert" "we" 4))
(define ARED4 (make-diff "insert" "yester" 13))

(check-expect (update ARE ARED1) ARETMP1)
(check-expect (update ARETMP1 ARED2) ARETMP2)
(check-expect (update ARETMP2 ARED3) ARETMP3)
(check-expect (update ARETMP3 ARED4) WERE)










(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))])) 



(check-expect (total-width (list (square 10 "solid" "white")
                                 (square 10 "solid" "white"))) 20)
(check-expect (total-width (list (circle 0 "solid" "white")
                                 (triangle 0 "solid" "white"))) 0)
(check-expect (total-width empty) 0)
  

(define (total-width loi)
  (cond [(empty? loi) 0]
        [else (+ (image-width (first loi)) (total-width (rest loi)))]))




(check-expect (taller-than (list (square 10 "solid" "white")
                                 (square 11 "solid" "white")) 10)
              (list (square 11 "solid" "white")))
(check-expect (taller-than (list (square 10 "solid" "white")
                                 (square 11 "solid" "white")) 15)
              empty)
(check-expect (taller-than empty 10) empty)
  

(define (taller-than loi cutoff)
  (cond
    [(empty? loi) empty]
    [else
     (if (> (image-height (first loi)) cutoff)
         (cons (first loi) (taller-than (rest loi) cutoff))
         (taller-than (rest loi) cutoff))]))






   



(check-expect (find-shape (list SQUARE CIRCLE TRIANGLE) "triangle") (list (build-img TRIANGLE)))
(check-expect (find-shape (list SQUARE CIRCLE CIRCLE) "triangle") empty)
(check-expect (find-shape empty "triangle") empty)
  

(define (find-shape losi type)
  (cond
    [(empty? losi) empty]
    [else
     (if (string=? (simple-img-type (first losi)) type)
         (cons (build-img (first losi)) (find-shape (rest losi) type))
         (find-shape (rest losi) type))]))




  




  

(check-expect (updates (list (make-diff "insert" "gomp" 0)
                             (make-diff "insert" "i" 5)) "e") "gompei")
(check-expect (updates empty "gompei") "gompei")

(define (updates lod text)
  (cond
    [(empty? lod) text]
    [else
     (updates (rest lod) (update text (first lod)))]))
     