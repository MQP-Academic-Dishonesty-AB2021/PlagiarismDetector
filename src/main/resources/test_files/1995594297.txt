

#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image) 





 
 
 
 
 

 










(define-struct customer (firstName lastName age zipCode))

(define Jeff (make-customer "Jeff" "Johnson" 41 "11111"))
(define Sabina (make-customer "Sabina" "Wilson" 18 "06488"))
(define Ryan (make-customer "Ryan" "Offstein" 18 "55042"))

(define (fn-for-customer acustomer)
  (...
   (customer-firstName acustomer)
   (customer-lastName 	acustomer)
   (customer-age acustomer)
   (customer-zipcode acustomer)))




  


(check-expect (birthday (make-customer "David" "Jefferson" 20 "1231512")) (make-customer "David" "Jefferson" 21 "1231512"))
(check-expect (birthday (make-customer "Sabina" "Wilson" 18 "06488")) (make-customer "Sabina" "Wilson" 19 "06488"))
(check-expect (birthday (make-customer "Sarah" "Reed" 0 "89-0218")) (make-customer "Sarah" "Reed" 1 "89-0218"))
(check-expect (birthday Ryan) (make-customer (customer-firstName Ryan) (customer-lastName Ryan) (+ (customer-age Ryan) 1) (customer-zipCode Ryan)))

(define (birthday acustomer)
  (make-customer (customer-firstName acustomer) (customer-lastName acustomer) (+ (customer-age acustomer) 1) (customer-zipCode acustomer)))








(check-expect (name-change (make-customer "David" "Jefferson" 20 "1231512") "Daniel" "Clark") (make-customer "Daniel" "Clark" 20 "1231512"))
(check-expect (name-change (make-customer "Henry" "Davidson" 53 "55128") "Ryan" "Offstein" ) (make-customer "Ryan" "Offstein" 53 "55128"))
(check-expect (name-change (make-customer "Lucy" "Peanut" 11 "958734") "Charlie" "Brown") (make-customer "Charlie" "Brown" 11 "958734"))
(check-expect (name-change Ryan "Steven" "Jackson") (make-customer "Steven" "Jackson" (customer-age Ryan) (customer-zipCode Ryan)))

(define (name-change acustomer fname lname)
  (make-customer fname lname (customer-age acustomer) (customer-zipCode acustomer)))











 
(define-struct simple-img (shape color area))

(define tri1    (make-simple-img "triangle" "green" 100))
(define square1 (make-simple-img "square"   "red" 200))
(define circle1 (make-simple-img "circle"   "blue" 150))

(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))

(define (fn-for-simple-img img)
  (...
   (simple-img-shape img)
   (simple-img-color img)
   (simple-img-area  img)))
  
  






(check-expect (bigger? (make-simple-img "triangle" "green" 100) (make-simple-img "square" "red" 200)) false)
(check-expect (bigger? (make-simple-img "circle" "yellow" 80) (make-simple-img "triangle" "purple" 5)) true)
(check-expect (bigger? square1 circle1) (> (simple-img-area square1) (simple-img-area circle1)))


(check-expect (bigger? SI SI2) true)

(define (bigger? img1 img2)
  (> (simple-img-area img1) (simple-img-area img2)))













(check-expect (build-image (make-simple-img "triangle" "red" 100 )) (triangle 15.196 "solid" "red"))
(check-expect (build-image (make-simple-img "triangle" "blue" 40 )) (triangle (sqrt (/ (* 4 40) (sqrt 3))) "solid" "blue"))

(check-expect (build-image (make-simple-img "square" "yellow" 121 )) (square 11 "solid" "yellow"))
(check-expect (build-image (make-simple-img "square" "blue" (* 5 9) )) (square (sqrt (* 5 9)) "solid" "blue"))

(check-expect (build-image (make-simple-img "circle" "green" 314.16)) (circle 10 "solid" "green"))
(check-expect (build-image (make-simple-img "circle" "purple" 100)) (circle (sqrt (/ 100 pi)) "solid" "Purple"))

(check-expect (build-image SI) .)


(define (build-image img)
  (cond [ (string=? (simple-img-shape img) "square")
          (square (sqrt (simple-img-area img)) "solid" (simple-img-color img))]
        [ (string=? (simple-img-shape img) "circle")
          (circle (sqrt (/ (simple-img-area img) pi)) "solid" (simple-img-color img))]
        [ (string=? (simple-img-shape img) "triangle")
          (triangle (sqrt (/ (* 4 (simple-img-area img)) (sqrt 3))) "solid" (simple-img-color img))]))
		

























(define-struct insert (position text))

(define IN1 (make-insert 1 "xyzzy"))
(define IN2 (make-insert 0 "Hello"))
(define IN3 (make-insert 3 "efgh"))
(define IN4 (make-insert 4 ""))


(define (fn-for-insert ainsert)
  (...
   (insert-position ainsert)
   (insert-text     ainsert)))




(define-struct delete (position chars))





(define DEL1 (make-delete 1 2))
(define DEL2 (make-delete 0 5))
(define DEL3 (make-delete 3 2))


(define (fn-for-delete adelete)
  (...
   (delete-position adelete)
   (delete-chars    adelete)))






(check-expect (update "0123456789" (make-insert 1 "xyzzy")) "0xyzzy123456789")
(check-expect (update "Hello" (make-insert 5 " World!")) "Hello World!")
(check-expect (update "Hello" (make-insert 5 " World!")) (string-append (substring "Hello" 0 (insert-position (make-insert 5 " World!"))) (insert-text (make-insert 5 " World!")) (substring "Hello" (insert-position (make-insert 5 " World!")))))
(check-expect (update "Test" (make-insert 1 "")) "Test")
(check-expect (update "" (make-insert 0 "Blank")) "Blank")
(check-expect (update "" (make-insert 0 "")) "")

(check-expect (update "Hello My World!" (make-delete 6 3)) "Hello World!")
(check-expect (update "Hello My World!" (make-delete 6 3)) (string-append (substring "Hello My World!" 0 (delete-position (make-delete 6 3))) (substring "Hello My World!" (+ (delete-position (make-delete 6 3)) (delete-chars (make-delete 6 3))))))
(check-expect (update "" (make-delete 0 0)) "")
(check-expect (update "Test" (make-delete 1 0)) "Test")
(check-expect (update "Gone!" (make-delete 0 5)) "")
(check-expect (update "0123456789" (make-delete 3 2)) "01256789")

 

(define (update text diff)
  (if (insert? diff)
      (ins text diff)
      (del text diff)
      ))





 

(define (ins text diff)
  (string-append (substring text 0 (insert-position diff)) (insert-text diff) (substring text (insert-position diff))))





 

(define (del text diff)
  (string-append (substring text 0 (delete-position diff)) (substring text (+ (delete-position diff) (delete-chars diff)))))





(define greeting "How are you today?")
(define diff1 (make-delete 4 1))
(define diff2 (make-insert 4 "we"))
(define diff3 (make-delete 13 2))
(define diff4 (make-insert 13 "yester"))

(define new-greeting (update (update (update (update greeting diff1) diff2) diff3) diff4))
new-greeting











(define LOI1 empty)
(define LOI2 (cons (square 10 "solid" "red") empty))
(define LOI3 (cons (square 10 "solid" "red") (cons (circle 20 "solid" "blue") empty)))

 




(check-expect (total-width empty) 0)
(check-expect (total-width (cons (rectangle 10 20 "solid" "red") empty)) 10)
(check-expect (total-width (cons (rectangle 25 3 "outline" "blue") empty)) (image-width (rectangle 25 3 "outline" "blue")))
(check-expect (total-width (cons (rectangle 25 30 "solid" "red")
                                 (cons (triangle 10 "solid" "green")
                                       (cons (square 100 "solid" "Purple") empty))))
              135)

 

(define (total-width loi)
  (cond [(empty? loi) 0]
        [else 
         (+ (image-width (first loi)) 
            (total-width (rest loi)) 
            )]))





(check-expect (taller-than 5 empty) empty)
(check-expect (taller-than 20 (cons (rectangle 25 30 "solid" "red")
                                    (cons (triangle 10 "solid" "green")
                                          (cons (square 100 "solid" "Purple") empty))))

              (cons (rectangle 25 30 "solid" "red")
                    (cons (square 100 "solid" "Purple") empty)))
(check-expect (taller-than 1000 (cons (rectangle 25 30 "solid" "red")
                                      (cons (triangle 10 "solid" "green")
                                            (cons (square 100 "solid" "Purple") empty))))

              empty)

 

(define (taller-than cutoff loi)
  (cond [(empty? loi) empty]
        [else 
         (if(> (image-height (first loi)) cutoff)
            (cons (first loi) (taller-than cutoff (rest loi)))
            (taller-than cutoff (rest loi)))]))

(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))




(check-expect (total-width LOI) 265) 

(check-expect (taller-than 30 LOI)
              (list . .))









(define LOSI1 empty)
(define LOSI2 (cons (make-simple-img "triangle" "green" 100) empty))
(define LOSI3 (cons (make-simple-img "triangle" "green" 100)
                    (cons (make-simple-img "square" "red" 200)
                          (cons (make-simple-img "circle" "blue" 150) empty))))

(define (fn-for-losi losi)
  (cond [(empty? losi) ...]
        [else
         (...
          (fn-for-simple-img(first losi)) 
          (fn-for-losi(rest losi)))])) 





(define SI1 (make-simple-img "circle" "red" 120))
(define SI3 (make-simple-img "circle" "purple" 40))

(check-expect (find-shape empty "circle") empty)
(check-expect (find-shape (cons (make-simple-img "circle" "red" 120) (cons (make-simple-img "circle" "purple" 40) empty)) "triangle") empty)
(check-expect (find-shape (cons (make-simple-img "circle" "red" 120) (cons (make-simple-img "square" "purple" 100) empty)) "square") (cons (square 10 "solid" "purple") empty))

(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))

 

(define (find-shape losi type)
  (cond [(empty? losi) empty]
        [else
         (if (string=? type (simple-img-shape (first losi)))
             (cons (build-image(first losi)) (find-shape (rest losi) type))
             (find-shape(rest losi) type))])) 









(define LOD1 empty)
(define LOD2 (cons (make-insert 2 "Hello") empty))
(define LOD3 (cons (make-insert 2 "Hello") (cons (make-delete 3 1) (cons (make-insert 1 "I am ") empty))))

(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (...
          (fn-for-diff(first lod)) 
          (fn-for-lod(rest lod)))])) 





(define INS (make-insert 1 "xyzzy"))
(define DEL (make-delete 3 2))
(check-expect (revUpdate (list INS DEL) "0123456789") "0xyzzy1256789")

(check-expect (revUpdate empty "HELLO") "HELLO")
(check-expect (revUpdate (list diff4 diff3 diff2 diff1) greeting) new-greeting)

(check-expect (revUpdate (list  (make-delete 0 4) (make-insert 2 "ADD")) "12345678") (update (update "12345678" (make-insert 2 "ADD")) (make-delete 0 4)))

 

(define (revUpdate lod str)
  (cond [(empty? lod) str]
        [else
         (update (revUpdate (rest lod) str) (first lod))]))





(check-expect (updates (list DEL INS) "0123456789") "0xyzzy1256789")

(check-expect (updates empty "HELLO") "HELLO")
(check-expect (updates (list diff1 diff2 diff3 diff4) greeting) new-greeting)

(check-expect (updates (list (make-insert 2 "ADD") (make-delete 0 4)) "12345678") (update (update "12345678" (make-insert 2 "ADD")) (make-delete 0 4)))

 

(define (updates lod str)
  (revUpdate (reverse lod) str))

