

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Ellys Gorodisch, Micah Vargas - Assignment #1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)






(check-expect (backwards "") "")
(check-expect (backwards "a") "a")
(check-expect (backwards "abcd") "dcba")

(define (backwards str) "") 








(define-struct customer (fName lName age zipCode))


(define C1 (make-customer "" "" 0 0))
(define C2 (make-customer "Ellys" "Gorodisch" 18 08527))
(define C3 (make-customer "Micah" "Vargas" 18 01609))

  






(check-expect (birthday C1) (make-customer "" "" 1 0))
(check-expect (birthday C2) (make-customer "Ellys" "Gorodisch" 19 08527))
(check-expect (birthday C3) (make-customer "Micah" "Vargas" 19 01609))



(define (birthday c)
  (make-customer
   (customer-fName c)
   (customer-lName c)
   (+ (customer-age c) 1)
   (customer-zipCode c)))





(check-expect (name-change C1 "" "") (make-customer "" "" 0 0))
(check-expect (name-change C2 "Ellyoos" "Gooroodisch")
              (make-customer "Ellyoos" "Gooroodisch" 18 08527))
(check-expect (name-change C3 "Moocah" "Voorgas")
              (make-customer "Moocah" "Voorgas" 18 01609))



(define (name-change c newFName newLName)
  (make-customer
   newFName
   newLName
   (customer-age c)
   (customer-zipCode c)))





(define-struct simple-img (type color area))



(define SI1 (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI3 (make-simple-img "circle" "blue" 150))

  







(check-expect (bigger? SI1 SI2) true)
(check-expect (bigger? SI2 SI3) true)
(check-expect (bigger? SI3 SI1) false)



(define (bigger? si1 si2)
  (> (simple-img-area si1) (simple-img-area si2)))




(check-expect (build-img SI1) (triangle (sqrt (/ (* 250 4) (sqrt 3))) "solid" "red"))
(check-expect (build-img SI2) (square (sqrt 200) "solid" "purple"))
(check-expect (build-img SI3) (circle (sqrt (/ 150 pi)) "solid" "blue"))



(define (build-img si)
  (cond
    [(string=? (simple-img-type si) "triangle")
     (triangle
      (sqrt (/ (* (simple-img-area si) 4) (sqrt 3)))
      "solid"
      (simple-img-color si))]
    [(string=? (simple-img-type si) "square")
     (square
      (sqrt (simple-img-area si))
      "solid"
      (simple-img-color si))]
    [(string=? (simple-img-type si) "circle")
     (circle
      (sqrt (/ (simple-img-area si) pi))
      "solid"
      (simple-img-color si))]
    [else empty-image]))





(define-struct diff (pos type insText delChars))




(define D1 (make-diff 0 "insert" "" 0))
(define D2 (make-diff 1 "insert" "HELLO" 0))
(define D3 (make-diff 1 "delete" "" 2))

  






(check-expect (update "0123456789" (make-diff 1 "insert" "xyzzy" 0))
              "0xyzzy123456789")
(check-expect (update "0123456789" (make-diff 3 "delete" "" 2))
              "01256789")



(define (update text d)
  (cond
    [(string=? (diff-type d) "insert")
     (string-append
      (substring text 0 (diff-pos d))
      (diff-insText d)
      (substring text (diff-pos d))
      )]
    [(string=? (diff-type d) "delete")
     (string-append
      (substring text 0 (diff-pos d))
      (substring text (+ (diff-pos d) (diff-delChars d))))
     ]))



(define TEXT "How are you today?")
(define Diff1 (make-diff 4 "delete" "" 1))
(define Diff2 (make-diff 4 "insert" "we" 0))
(define Diff3 (make-diff 13 "delete" "" 2))
(define Diff4 (make-diff 13 "insert" "yester" 0))
(check-expect (update (update (update (update TEXT Diff1) Diff2) Diff3) Diff4)
              "How were you yesterday?")












(define LOI1 empty)
(define LOI2 (list (square 20 "solid" "red")))
(define LOI3 (list (square 20 "solid" "red") (square 40 "solid" "blue")))

 






(check-expect (total-width LOI1) 0)
(check-expect (total-width LOI2) 20)
(check-expect (total-width LOI3) 60)



(define (total-width loi)
  (cond [(empty? loi) 0]
        [else
         (+ (image-width (first loi))
            (total-width (rest loi)))]))





(check-expect (taller-than LOI1 0) empty)
(check-expect (taller-than LOI2 10) (list (square 20 "solid" "red")))
(check-expect (taller-than LOI3 30) (list (square 40 "solid" "blue")))



(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) cutoff)
         (cons (first loi) (taller-than (rest loi) cutoff))]
        [else
         (taller-than (rest loi) cutoff)]))









(define LOSI1 empty)
(define LOSI2 (list SI1))
(define LOSI3 (list SI1 SI2 SI3))

 







(check-expect (find-shape LOSI1 "triangle") empty)
(check-expect (find-shape LOSI2 "square") empty)
(check-expect (find-shape LOSI3 "circle") (list (build-img SI3)))



(define (find-shape losi type)
  (cond [(empty? losi) empty]
        [(string=? (simple-img-type (first losi)) type)
         (cons (build-img (first losi)) (find-shape (rest losi) type))]
        [else
         (find-shape (rest losi) type)]))








(define LOD1 empty)
(define LOD2 (list D1 D2 D3))

 







(define DEL (make-diff 3 "delete" "" 2))
(define INS (make-diff 1 "insert" "xyzzy" 0))
(check-expect (updates (list DEL INS) "0123456789") "0xyzzy1256789")
(check-expect (updates (list Diff1 Diff2 Diff3 Diff4) TEXT)
              "How were you yesterday?")



(define (updates lod text)
  (cond [(empty? lod) text]
        [else
         (update (updates (reverse (rest (reverse lod))) text)
                 (first (reverse lod)))]))