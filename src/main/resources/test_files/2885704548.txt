

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |CS 1102 Assignment 1_final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)








(check-expect (backwards "") "")
(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "a") "a")
(check-expect (backwards "1234567") "7654321")








(define-struct customer (fname lname age zip))







(define bub (make-customer "bub" "bubsley" 34 01609))

(define (fn-for-customer cust)
  (...(customer-fname cust)
      (customer-lname cust)
      (customer-age cust)
      (customer-zip cust)))





(check-expect (birthday bub)
              (make-customer "bub" "bubsley" 35 01609))
(check-expect (birthday (make-customer "Bob" "Bobsley" 0 01609))
              (make-customer "Bob" "Bobsley" 1 01609))



(define (birthday cust)
  (make-customer
   (customer-fname cust)
   (customer-lname cust)
   (+ (customer-age cust) 1)
   (customer-zip cust)))






(check-expect (name-change (make-customer "bub" "bubsley" 35 01609) "John" "Smith")
              (make-customer "John" "Smith" 35 01609))
(check-expect (name-change (make-customer "bub" "bubsley" 27 98115) "" "")
              (make-customer "" "" 27 98115))

              


(define (name-change cust newFName newLName)
  (make-customer newFName newLName (customer-age cust) (customer-zip cust)))












(define-struct simple-img (img-shape color pArea))








(define SI (make-simple-img "triangle" "red" 250))
(define SI1 (make-simple-img "circle" "red" 120))


(define SI2 (make-simple-img "square" "purple" 200))
(define SI3 (make-simple-img "circle" "purple" 40))

(define (fn-for-simple-img img) 
  (cond [(string=? "triangle" (simple-img-img-shape img))
         (...(simple-img-color img) (simple-img-pArea img))]
        
        [(string=? "square" (simple-img-img-shape img))
         (...(simple-img-color img) (simple-img-pArea img))]
        
        [(string=? "circle" (simple-img-img-shape img))
         (...(simple-img-color img) (simple-img-pArea img))]))








(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI SI) false)
(check-expect (bigger? SI2 SI) false)



(define (bigger? SI SI2)
  (> (simple-img-pArea SI) (simple-img-pArea SI2)))






(define SI4 (make-simple-img "circle" "blue" 800))

(check-expect (build-image SI) .)
(check-expect (build-image SI2) (square (sqrt (simple-img-pArea SI2)) "solid" (simple-img-color SI2)))
(check-expect (build-image SI4) (circle (findRadiusCircle (simple-img-pArea SI4)) "solid" (simple-img-color SI4)))



(define (build-image img) 
  (cond [(string=? "triangle" (simple-img-img-shape img))
         (triangle (findSideTriangle (simple-img-pArea img)) "solid" (simple-img-color img))]
        
        [(string=? "square" (simple-img-img-shape img))
         (square (sqrt (simple-img-pArea img)) "solid" (simple-img-color img))]
        
        [(string=? "circle" (simple-img-img-shape img))
         (circle (findRadiusCircle (simple-img-pArea img)) "solid" (simple-img-color img))]))







(check-within (findSideTriangle 250) (sqrt (/ (* 250 4) (sqrt 3))) 0.001)
(check-within (findSideTriangle 0) (sqrt (/ (* 0 4) (sqrt 3))) 0.001)


(define (findSideTriangle area)
  (sqrt (/ (* area 4) (sqrt 3))))




(check-within (findRadiusCircle 20) (sqrt (/ 20 pi)) 0.001)
(check-within (findRadiusCircle 0) (sqrt (/ 0 pi)) 0.001)


(define (findRadiusCircle area)
  (sqrt(/ area pi)))






(define-struct diff (type index payload))








(define INS (make-diff "INS" 1 "xyzzy"))
(define DEL (make-diff "DEL" 3 2))  

 















(check-expect (update "0123456789" INS) "0xyzzy123456789")
(check-expect (update "0123456789" DEL) "01256789")
(check-expect (update "0123456789" (make-diff "DEL" 5 0)) "0123456789")
(check-expect (update "0123456789" (make-diff "INS" 5 "")) "0123456789")



(define (update inputString diff)
  (cond [(string=? "INS" (diff-type diff))    
         (string-insert inputString (diff-index diff) (diff-payload diff))]
        
        [(string=? "DEL" (diff-type diff))
         (string-delete inputString (diff-index diff) (diff-payload diff))]))




(check-expect (string-insert "ABCD" 2 "123") "AB123CD")
(check-expect (string-insert "ABCD" 1 "") "ABCD")
(check-expect (string-insert "" 0 "") "")



(define (string-insert insStr index diffStr)
  (string-append (substring insStr 0 index) diffStr (substring insStr index)))





(check-expect (string-delete "ABCDEF" 1 2) "ADEF")
(check-expect (string-delete "ABCDEF" 1 0) "ABCDEF")
(check-expect (string-delete "" 0 0) "")



(define (string-delete insStr index numChars)
  (string-append (substring insStr 0 index) (substring insStr (+ index numChars))))




(define diffTest1 "How are you today?")
(define diffTest2 "How were you yesterday?")
(define D1 (make-diff "DEL" 12 5))
(define D2 (make-diff "INS" 12 "yesterday"))
(define D3 (make-diff "DEL" 4 3))
(define D4 (make-diff "INS" 4 "were"))







(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))









(define LOI-1 empty)
(define LOI-2 (cons I1 empty))
(define LOI-3 (cons I2 (cons I3 empty)))

  





(check-expect (total-width empty) 0)
(check-expect (total-width LOI)
              (+ (image-width I1) (image-width I2) (image-width I3) (image-width I4)))



(define (total-width loi)
  (cond [(empty? loi) 0]                   
        [else (+ (image-width (first loi))                 
                 (total-width (rest loi)))]))






(check-expect (taller-than LOI 30)
              (list . .))
(check-expect (taller-than empty 99999999) empty)
(check-expect (taller-than (cons (square 30 "solid" "red")
                                 (cons (square 31 "solid" "red") empty)) 30)
              (list (square 31 "solid" "red")))



(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]                   
        [(> (image-height (first loi)) cutoff)                
         (cons (first loi) (taller-than (rest loi) cutoff))]
        [else (taller-than (rest loi) cutoff)]))










(define LOSI-1 empty)
(define LOSI-2 (cons SI empty))
(define LOSI-3 (cons SI (cons SI2 empty)))

                  








(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
(check-expect (find-shape empty "circle") empty)



(define (find-shape losi type)
  (cond [(empty? losi) empty]                   
        [(string=? (simple-img-img-shape (first losi)) type)                
         (cons (build-image (first losi)) (find-shape (rest losi) type))]
        [else (find-shape (rest losi) type)]))










(define LOD empty)
(define LOD2 (cons DEL empty))
(define LOD3 (cons INS (cons DEL empty)))

             






(check-expect (updates empty "ABCD") "ABCD")
(check-expect (updates (list (make-diff "INS" 1 "XYZ")) "012345") "0XYZ12345")
(check-expect (updates (list DEL INS) "0123456789") "0xyzzy1256789")

(check-expect (updates (list D1 D2 D3 D4) diffTest1) diffTest2)




(define (updates lod inputStr)
  (cond [(empty? lod) inputStr]                       
        [else (updates (rest lod) (update inputStr (first lod)))]))