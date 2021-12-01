

#reader(lib "htdp-beginner-reader.ss" "lang")((modname assignment-1-use) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)














(define-struct customer (firstn lastn age zip))
(define (fn-for-customer customer)
  (...
   (customer-firstn customer)
   (customer-lastn customer)
   (customer-age customer)
   (customer-zip customer)
   ))

(define BOB (make-customer "Bob" "Robertson" 16 01581))
(check-expect (birthday BOB) (make-customer (customer-firstn BOB) (customer-lastn BOB) (+ (customer-age BOB) 1) (customer-zip BOB)))
(check-expect (birthday (make-customer "John" "Matthew" 0 00000)) (make-customer "John" "Matthew" 1 00000))



 

(define (birthday customer)
  (make-customer
   (customer-firstn customer)
   (customer-lastn customer)
   (+ (customer-age customer) 1)
   (customer-zip customer)
   ))

(check-expect (name-change BOB "Joe" "Jameson") (make-customer "Joe" "Jameson" (customer-age BOB) (customer-zip BOB)))
(check-expect (name-change BOB "" "") (make-customer "" "" (customer-age BOB) (customer-zip BOB)))


 

(define (name-change customer newfname newlname)
  (make-customer
   newfname
   newlname
   (customer-age customer)
   (customer-zip customer)
   ))





(define-struct simple-img (shape color area))
(define (fn-for-simple-img simple-img)
  (...
   (simple-img-shape simple-img)
   (simple-img-color simple-img)
   (simple-img-area simple-img)))



(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))

(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)



(check-expect (build-image SI) .)


  

(define (bigger? img1 img2)
  (cond [(> (simple-img-area img1) (simple-img-area img2)) true]
      [else false]))
      



(define IMAGE (make-simple-img "triangle" "red" 250))
(define (build-image img)
        (cond
          [(equal? (simple-img-shape img) "triangle") (triangle (sqrt (/ (* 4 (simple-img-area img)) (sqrt 3))) "solid" (simple-img-color img))]
          [(equal? (simple-img-shape img) "circle") (circle (sqrt (/ (simple-img-area img) pi)) "solid" (simple-img-color img))]
          [(equal? (simple-img-shape img) "square") (square (sqrt (simple-img-area img)) "solid" (simple-img-color img))]))





(define-struct diff (pos txt type))
 





















(check-expect (update "0123456789" (make-diff 1 "xyzzy" "insert")) "0xyzzy123456789")
(check-expect (update "0123456789" (make-diff 3 "2" "delete")) "01256789")


  

(define (update text diff)
        (cond
          [(equal? (diff-type diff) "insert")
           (string-append (substring text 0 (diff-pos diff)) (diff-txt diff) (substring text (diff-pos diff)))]
          [(equal? (diff-type diff) "delete")
           (string-append (substring text 0 (diff-pos diff)) (substring text (+ (string->number (diff-txt diff)) (diff-pos diff))))]))


(define DIFF1 (make-diff 4 "3" "delete"))
(define DIFF2 (make-diff 4 "were" "insert"))
(define DIFF3 (make-diff 13 "5" "delete"))
(define DIFF4 (make-diff 13 "yesterday" "insert"))
(define RESULT1 (update "How are you today?" DIFF1))
(define RESULT2 (update RESULT1 DIFF2))
(define RESULT3 (update RESULT2 DIFF3))
(define RESULT4 (update RESULT3 DIFF4))
(check-expect RESULT4 "How were you yesterday?")







 

(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)
(define LOI (list I1 I2 I3 I4))




(check-expect (total-width LOI) 265)
(check-expect (total-width empty) 0)
(define (total-width loi)
        (cond
         [(empty? loi) 0]
         [else
          (+ (image-width (first loi)) (total-width (rest loi)))]))




(check-expect (taller-than LOI 30)
              (list I1 I4))
(check-expect (taller-than empty 25) empty)
(define (taller-than loi cutoff)
        (cond
         [(empty? loi) empty]
         [else
          (if (<= cutoff (image-height (first loi)))
              (cons (first loi) (taller-than (rest loi) cutoff))
              (taller-than (rest loi) cutoff))]))




(define SI1 (make-simple-img "circle" "red" 120))
(define SI3 (make-simple-img "circle" "purple" 40))





 




(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
(define (find-shape losi type)
        (cond
         [(empty? losi) empty]
         [else
          (if (equal? type (simple-img-shape (first losi)))
              (cons (build-image (first losi)) (find-shape (rest losi) type))
              (find-shape (rest losi) type))]))






 



(check-expect (updates "0123456789" (list DEL INS)) "0xyzzy1256789")
(check-expect (updates "How are you today" (list DIFF1 DIFF2 DIFF3 DIFF4)) "How were you yesterday")

(define DEL (make-diff 3 "2" "delete"))
(define INS (make-diff 1 "xyzzy" "insert"))

(define (updates str lod)
        (cond
         [(empty? lod) str]
         [else
          (updates (update str (first lod)) (rest lod))]))
