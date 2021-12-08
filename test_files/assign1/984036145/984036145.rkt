

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname BlakeJasonAssignment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))









(require 2htdp/image)






(check-expect (backwards "") "")
(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "xyz") "zyx")
(check-expect (backwards "1234") "4321")
(check-expect (backwards "a b cd") "dc b a")

(define (backwards sol) " ")



(define-struct customer (firstName lastName age zipCode))







(define C1 (make-customer "Jason" "Kardon" 18 10901))
(define C2 (make-customer "Blake" "Mcleod" 7 03669))
(define C3 (make-customer "Bob" "White" 101 63606))

 




(check-expect (birthday C1) (make-customer (customer-firstName C1)
                                           (customer-lastName C1)
                                           (+ 1(customer-age C1))
                                           (customer-zipCode C1)))

(check-expect (birthday C2) (make-customer (customer-firstName C2)
                                           (customer-lastName C2)
                                           (+ 1(customer-age C2))
                                           (customer-zipCode C2)))

(check-expect (birthday C3) (make-customer (customer-firstName C3)
                                           (customer-lastName C3)
                                           (+ 1(customer-age C3))
                                           (customer-zipCode C3)))
            











(define (birthday doc)
  (make-customer (customer-firstName doc)
                  (customer-lastName doc)
                  (+ 1(customer-age doc))
                  (customer-zipCode doc)))






(check-expect (name-change C1 "Robert" "Quid")(make-customer "Robert" "Quid"
                                           (customer-age C1)
                                           (customer-zipCode C1)))

(check-expect (name-change C1 "Reginald" "Savory")(make-customer "Reginald" "Savory"
                                           (customer-age C1)
                                           (customer-zipCode C1)))

(check-expect (name-change C2 "Patrick" "Star")(make-customer "Patrick" "Star"
                                           (customer-age C2)
                                           (customer-zipCode C2)))

(check-expect (name-change C3 "Johnny" "Cafe")(make-customer "Johnny" "Cafe"
                                           (customer-age C3)
                                           (customer-zipCode C3)))



(define (name-change doc firstName lastName)
  (make-customer
   firstName
   lastName
   (customer-age doc)
   (customer-zipCode doc)))








(define-struct simple-img (shape color area)) 





 



(define SI (make-simple-img "triangle" "red" 250)) 
(define SI1 (make-simple-img "circle" "blue" 60))
(define SI3 (make-simple-img "triangle" "green" 50))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI4 (make-simple-img "square" "purple" 0)) 

(check-expect (bigger? SI1 SI3) true)
(check-expect (bigger? SI1 SI2) false)
(check-expect (bigger? SI3 SI1) false)
(check-expect (bigger? SI4 SI2) false)




 

(define (bigger? img img2)
  (>
   (simple-img-area img)
   (simple-img-area img2)))



(check-within (triSide SI3) (sqrt (* (/ 4 (sqrt 3)) (simple-img-area SI3))) .0001)
(check-within (triSide (make-simple-img "triangle" "red" 40 )) (sqrt (* (/ 4 (sqrt 3)) 40)) .0001)
(check-within (triSide (make-simple-img "triangle" "yellow" 0 )) (sqrt (* (/ 4 (sqrt 3)) 0)) .0001)





 

(define (triSide img)
  (sqrt (* (/ 4 (sqrt 3)) (simple-img-area img))))

(check-within (cirSide SI1) (sqrt (* (/ 1 pi) (simple-img-area SI1))) .0001)
(check-within (cirSide (make-simple-img "circle" "blue" 25 )) (sqrt (* (/ 1 pi) 25)) .0001)
(check-within (cirSide (make-simple-img "cirlce" "orange" 0 )) (sqrt (* (/ 1 pi) 0)) .0001)





 

(define (cirSide img)
  (sqrt (* (/ 1 pi) (simple-img-area img))))

(check-within (sqSide SI2) (sqrt (simple-img-area SI2)) .0001)
(check-within (sqSide (make-simple-img "square" "magenta" 30 )) (sqrt 30) .0001)
(check-within (sqSide SI4) (sqrt (simple-img-area SI4)) .0001)





 

(define (sqSide img)
  (sqrt (simple-img-area img)))








(check-expect (build-image SI) .)
(check-expect (build-image SI1) (circle (cirSide SI1) "solid" "blue"))
(check-expect (build-image SI3) (triangle (triSide SI3) "solid" "green"))
(check-expect (build-image SI2) (square (sqSide SI2) "solid" "purple"))
(check-expect (build-image SI4) (square (sqSide SI4) "solid" "purple"))




 

(define (build-image img)
  (cond [(string=? (simple-img-shape img) "circle")
         (circle (cirSide img) "solid" (simple-img-color img))]
        [(string=? (simple-img-shape img) "triangle")
         (triangle (triSide img) "solid" (simple-img-color img))]
        [(string=? (simple-img-shape img) "square")
         (square (sqSide img) "solid" (simple-img-color img))]))






(define-struct diff-del ( postion length ))

 

(define D1 (make-diff-del 3 2))
(define D2 (make-diff-del 0 1))
(define D3 (make-diff-del 1 0))






(define-struct diff-in ( postion text ))
 

(define D4 (make-diff-in 1 "xyzzy"))
(define D5 (make-diff-in 0 "" ))
(define D6 (make-diff-in 3 "y"))













(check-expect (update "0123456789" D1) "01256789")
(check-expect (update "123abc" D3) "123abc")
(check-expect (update "0123456789" D4) "0xyzzy123456789")
(check-expect (update "efgh" D5) "efgh")
(check-expect (update "abcd" D6) "abcyd")





(define (update str dif)
  (cond[(diff-in? dif ) (insert str dif)]
       [(diff-del? dif ) (delete str dif)]))









(check-expect (insert "0123456789" D4) "0xyzzy123456789")
(check-expect (insert "efgh" D5) "efgh")
(check-expect (insert "abcd" D6) "abcyd")

 



(define (insert str dif)
  (string-append (substring str 0 (diff-in-postion dif)) (diff-in-text dif) (substring str (diff-in-postion dif))))







(check-expect (delete "0123456789" D1) "01256789")
(check-expect (delete "123abc" D3) "123abc")

 



(define (delete str dif)
  (string-append (substring str 0 (diff-del-postion dif)) (substring str (+ (diff-del-postion dif)(diff-del-length dif)))))



(define P3 "How are you today?")
(define P3-1 (make-diff-del 4 3))
(define P3-2 (make-diff-del 9 5))
(define P3-3 (make-diff-in 4 "were"))
(define P3-4 (make-diff-in 13 "yesterday"))

(update (update (update (update P3 P3-1) P3-2) P3-3) P3-4)





(define I1 .) 
(define I2 .) 
(define I3 .)
(define I4 .) 

(define LOI (list I1 I2 I3 I4))
(define LOI2 (list I1 I4)) 
(define LOI3 (list I2 I3)) 









 






(check-expect (total-width LOI) 265)
(check-expect (total-width LOI2) 160)
(check-expect (total-width LOI3) 105)
(check-expect (total-width empty) 0)

 

(define (total-width loi)
  (cond [(empty? loi) 0]
        [else
         (+ (image-width(first loi))
              (total-width(rest loi)))]))






(check-expect (taller-than LOI 30)
              (list . .))
(check-expect (taller-than LOI2 60) (list I1))
(check-expect (taller-than LOI3 50) empty)
(check-expect (taller-than empty 52) empty)

 


(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) cutoff)
         (cons (first loi) (taller-than(rest loi) cutoff))]
        [else (taller-than(rest loi) cutoff)]))









 








(define SI5 (make-simple-img "circle" "red" 120))
(define SI6 (make-simple-img "circle" "purple" 40))
 







(check-expect (find-shape (list SI SI5 SI2 SI6) "circle")
              (list . .))
(check-expect (find-shape (list SI SI5 SI2 SI6 SI4) "square")
              (list (build-image SI2) (build-image SI4)))
(check-expect (find-shape (list SI SI5 SI2 SI6 SI4) "triangle")
              (list (build-image SI)))
(check-expect (find-shape (list SI6 SI5 SI2) "triangle")
              empty)
(check-expect (find-shape empty "triangle")
              empty)

 

(define (find-shape losi shapeStr)
  (cond [(empty? losi) empty]
        [(string=?(simple-img-shape (first losi)) shapeStr)
         (cons (build-image (first losi)) (find-shape(rest losi) shapeStr))]
        [else (find-shape(rest losi) shapeStr)]))













 








 






(define DEL (make-diff-del 3 2))
(define INS (make-diff-in 1 "xyzzy"))
(define INS2 (make-diff-in 0 ":)"))
(define INS3 (make-diff-in 1 "^"))







(check-expect (updates (list P3-1 P3-2 P3-3 P3-4) P3) "How were you yesterday?")
(check-expect (updates (list DEL INS) "0123456789") "0xyzzy1256789")
(check-expect (updates (list INS DEL) "0123456789") "0xyy123456789")
(check-expect (updates empty "Java>Racket") "Java>Racket")
(check-expect (updates (list INS2 INS3) "") ":^)")

 

(define (updates lod inputStr)
  (cond [(empty? lod) inputStr]
        [else
         (updates (rest lod) (update inputStr (first lod)))]))
                