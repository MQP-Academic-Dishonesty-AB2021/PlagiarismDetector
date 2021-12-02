

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname CS_Assignment1_AnnaMichael-Final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require racket/math)







(check-expect (backwards empty) "")
(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "a") "a")









(define-struct customer (fname lname age zip))

 
  


(check-expect (birthday empty) empty)
(check-expect (birthday (make-customer "Paul" "Smith" 8 "12345")) (make-customer "Paul" "Smith" 9 "12345"))



(define (birthday customer)
  (cond [(empty? customer) empty]
        [else
         (make-customer (customer-fname customer) (customer-lname customer) (+ 1 (customer-age customer)) (customer-zip customer))]))



(check-expect (name-change empty "" "") empty)
(check-expect (name-change (make-customer "Paul" "Smith" 8 "12345") empty "") empty)
(check-expect (name-change (make-customer "Paul" "Smith" 8 "12345") "John" "Richards") (make-customer "John" "Richards" 8 "12345"))



(define (name-change customer fname lname)
  (cond [(or (empty? customer) (empty? fname) (empty? lname)) empty]
        [else
         (make-customer fname lname (customer-age customer) (customer-zip customer))]))













(define-struct simple-img (shape color a))

(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))




(check-expect (bigger? SI SI2) true)
(check-expect (bigger? empty empty) empty)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI SI) false)

(define (bigger? img1 img2)
  (cond [(or (empty? img1) (empty? img2)) empty]
        [(> (simple-img-a img1) (simple-img-a img2)) true]
        [else
         false]))







(check-expect (build-image SI) .)
(check-expect (build-image empty) empty)
(check-expect (build-image SI2) (square 14.14 "solid" "purple"))
(check-expect (build-image (make-simple-img "circle" "blue" (* 144 pi))) (circle 12 "solid" "blue"))

(define (build-image img)
  (cond [(empty? img) empty]
        [(string=? "square" (simple-img-shape img))
                   (square (sqrt (simple-img-a img)) "solid" (simple-img-color img))]
        [(string=? "triangle" (simple-img-shape img))
                   (triangle (* (/ 2 3) (expt 3 (/ 3 4)) (sqrt (simple-img-a img))) "solid" (simple-img-color img))]
        [(string=? "circle" (simple-img-shape img))
                   (circle (sqrt (/ (simple-img-a img) pi)) "solid" (simple-img-color img))]
        [else
         empty]))













(define-struct diff (type position mod))

 

  



(check-expect (update "12345" (make-diff "insert" 2 "abc")) "12abc345")
(check-expect (update "12345" (make-diff "delete" 2 "2")) "125")
(check-expect (update "12345" (make-diff "" 0 "")) "12345")

  
 

(define (update text difference)
  (cond [(empty? difference) ""]
        [(string=? "insert" (diff-type difference))
         (string-append (substring text 0 (diff-position difference)) (diff-mod difference) (substring text (diff-position difference)))]
        [(string=? "delete" (diff-type difference))
         (string-append (substring text 0 (diff-position difference)) (substring text (+ (diff-position difference) (string->number (diff-mod difference)))))]
        [else
         text]))


(define DIFF1 (make-diff "delete" 4 "3"))
(define DIFF2 (make-diff "delete" 9 "5"))
(define DIFF3 (make-diff "insert" 4 "were"))
(define DIFF4 (make-diff "insert" 13 "yesterday"))
         




(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))




(check-expect (total-width LOI) 265) 

(check-expect (taller-than LOI 30)
              (list . .))







(define SI1 (make-simple-img "circle" "red" 120))
(define SI3 (make-simple-img "circle" "purple" 40))

(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))








 









(define (total-width ListOfImage)
  (cond [(empty? ListOfImage) 0]
        [else
         (+ (image-width (first ListOfImage)) (total-width (rest ListOfImage)))]))








(define (taller-than ListOfImage cutoff)
  (cond [(empty? ListOfImage) empty]
        [(> (image-height (first ListOfImage)) cutoff)
         (cons (first ListOfImage) (taller-than (rest ListOfImage) cutoff))] 
        [else
         (taller-than (rest ListOfImage) cutoff)]))








 






(define (find-shape ListOfSimpImg type)
  (cond [(empty? ListOfSimpImg) empty]
        [(string=? (simple-img-shape (first ListOfSimpImg)) type)
         (cons (build-image (first ListOfSimpImg)) (find-shape (rest ListOfSimpImg) type))] 
        [else
         (find-shape (rest ListOfSimpImg) type)]))







 






(check-expect (updates empty "12345") "12345")
(check-expect (updates (list DIFF1 DIFF2 DIFF3 DIFF4) "How are you today?") "How were you yesterday?")

(define (updates diffs text)
  (cond [(empty? diffs) text]
        [else
         (updates (rest diffs) (update text (first diffs)))]))
  