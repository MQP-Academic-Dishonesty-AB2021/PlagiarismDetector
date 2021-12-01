

#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Vivek Voleti & Kashvi Singh - FINAL Assignment 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)







(check-expect (backwards "") "") 
(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "#asdf1.0") "0.1fdsa#") 

(define (backwards str) "") 






(define-struct customer (fname lname age zip))









(define JOHN (make-customer "John" "Smith" 33 23422))
(define JANE (make-customer "Jane" "Smith" 35 23422))
(define BABY (make-customer "Baby" "Smith" 0 23422))


(define (fn-for-customer cust)
  (... (customer-fname cust) 
       (customer-lname cust) 
       (customer-age cust) 
       (customer-zip cust))) 




         


(check-expect (birthday JOHN) (make-customer (customer-fname JOHN)
                                             (customer-lname JOHN)
                                             (+ (customer-age JOHN) 1)
                                             (customer-zip JOHN)))

(check-expect (birthday JANE) (make-customer (customer-fname JANE)
                                             (customer-lname JANE)
                                             (+ (customer-age JANE) 1)
                                             (customer-zip JANE)))

(check-expect (birthday BABY) (make-customer (customer-fname BABY) 
                                             (customer-lname BABY)
                                             (+ (customer-age BABY) 1)
                                             (customer-zip BABY)))

 


(define (birthday cust)
  (make-customer (customer-fname cust)
                 (customer-lname cust)
                 (+ (customer-age cust) 1)
                 (customer-zip cust)))




         
         


(check-expect (name-change JOHN "Chad" "Brown")
              (make-customer "Chad" "Brown" (customer-age JOHN) (customer-zip JOHN)))

(check-expect (name-change JANE "Olivia" "Peters")
              (make-customer "Olivia" "Peters" (customer-age JANE) (customer-zip JANE)))

(check-expect (name-change BABY "" "")
              (make-customer "" "" (customer-age BABY) (customer-zip BABY)))

 


(define (name-change cust new-fname new-lname)
  (make-customer new-fname new-lname
                 (customer-age cust)
                 (customer-zip cust)))



(define-struct simple-img (shape color area))





               






              
              



(define SQUARE (make-simple-img "square" "blue" 0))
(define CIRCLE (make-simple-img "circle" "red" (* (sqr 20) pi)))
(define TRIANGLE (make-simple-img "triangle" "green" 0))


(define (fn-for-simple-img image)
  (...
   (simple-img-shape image) 
   (simple-img-color image) 
   (simple-img-area image))) 




         
         

 

(check-expect (bigger? SQUARE CIRCLE) false)
(check-expect (bigger? CIRCLE TRIANGLE) true)
(check-expect (bigger? TRIANGLE SQUARE) false)

(define (bigger? image1 image2)
  (cond [(> (simple-img-area image1) (simple-img-area image2)) true]
        [else false]))




         
         
         

 

(check-expect (build-image SQUARE) (square 0 "solid" "blue"))
(check-expect (build-image CIRCLE) (circle 20 "solid" "red"))
(check-expect (build-image TRIANGLE) (triangle 0 "solid" "green"))
(check-expect (build-image (make-simple-img "sQUaRe" "bLUe" 121))
              (square 11 "solid" "blue")) 
                           

(define (build-image img)
  (cond [(string=? (string-downcase (simple-img-shape img)) "square") 
         (square (sqrt (simple-img-area img)) "solid"
                 (string-downcase (simple-img-color img)))]
        
        [(string=? (string-downcase (simple-img-shape img)) "circle") 
         (circle (sqrt (/ (simple-img-area img) pi)) "solid"
                 (string-downcase (simple-img-color img)))]
        
        [(string=? (string-downcase (simple-img-shape img)) "triangle") 
         (triangle (sqrt (/ (* (simple-img-area img) 4) (sqrt 3))) "solid"
                   (string-downcase (simple-img-color img)))]))



(define-struct diff (type pos change))









              
              
              



             
             
             
             



                
                
                




         
         
         
         
         
         
         

 


(check-expect (update "" (make-diff "ins" 0 "xyzzy")) "xyzzy") 

(check-expect (update "0123456789" 
                      (make-diff "ins" 3 "")) "0123456789")

(check-expect (update "0123456789" 
                      (make-diff "ins" 1 "xyzzy")) "0xyzzy123456789")

(check-expect (update "0123456789" 
                      (make-diff "ins" 0 "xyzzy")) "xyzzy0123456789")

(check-expect (update "0123456789" 
                      (make-diff "ins" 10 "xyzzy")) "0123456789xyzzy")

(check-expect (update "0123456789" 
                      (make-diff "ins" -2 "xyzzy")) "xyzzy0123456789")

(check-expect (update "0123456789" 
                      (make-diff "ins" 489 "xyzzy")) "0123456789xyzzy")

(check-expect (update "0123456789" 
                      (make-diff "INS" 1 "xyzzy")) "0xyzzy123456789")


(check-expect (update "" (make-diff "del" 3 2)) "") 

(check-expect (update "0123456789" 
                      (make-diff "del" 6 0)) "0123456789")

(check-expect (update "0123456789" 
                      (make-diff "del" 3 2)) "01256789")

(check-expect (update "0123456789" 
                      (make-diff "del" 0 4)) "456789")

(check-expect (update "0123456789" 
                      (make-diff "del" 9 1)) "012345678")

(check-expect (update "0123456789" 
                      (make-diff "del" -5 5)) "56789")

(check-expect (update "0123456789" 
                      (make-diff "del" 111 3)) "0123456789")

(check-expect (update "0123456789" 
                      (make-diff "del" 8 7)) "01234567")

(check-expect (update "0123456789" 
                      (make-diff "dEL" 3 2)) "01256789")


(define (update text diff)
  (cond [(string=? (string-downcase (diff-type diff)) "ins") 
         (string-append (substring text 0 (round-pos text diff))
                        (diff-change diff)
                        (substring text (round-pos text diff) (string-length text)))]
        
        [(string=? (string-downcase (diff-type diff)) "del") 
         (cond [(<= (+ (diff-pos diff) (diff-change diff)) (string-length text))
                (string-append (substring text 0 (round-pos text diff))
                               (substring text (+ (round-pos text diff) (diff-change diff))
                                               (string-length text)))]
               [else (substring text 0 (round-pos text diff))])])) 
                                                                   
                                                                   




          
          

 


(check-expect (round-pos "0123456789" (make-diff "del" -5 5)) 0)
(check-expect (round-pos "0123456789" (make-diff "ins" 56 "hi")) 10)


(define (round-pos text diff)
  (cond [(< (diff-pos diff) 0) 0] 
        [(> (diff-pos diff) (string-length text)) (string-length text)] 
        [else (diff-pos diff)])) 


(define D1 (make-diff "del" 4 3))
(define D2 (make-diff "ins" 4 "were"))
(define D3 (make-diff "del" 12 6))
(define D4 (make-diff "ins" 12 " yesterday"))


(check-expect (update (update (update (update "How are you today?" D1) D2) D3) D4)
              "How were you yesterday?")












(define loi1 empty)
(define loi2 (cons (rectangle 10 40 "solid" "red") (cons (square 90 "solid" "green") empty)))
(define loi3 (cons (square 30 "solid" "blue") (cons . (cons (triangle 20 "solid" "red") empty))))


(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (first loi)
              (fn-for-loi (rest loi)))]))




          

 


(check-expect (total-width loi1) 0) 
(check-expect (total-width loi2) (+ (image-width (first loi2)) (image-width (second loi2))))


(define (total-width loi)
  (cond [(empty? loi) 0]
        [else (+ (image-width (first loi))
                 (total-width (rest loi)))]))




         

 


(check-expect (taller-than loi1 40) empty) 
(check-expect (taller-than loi2 100) empty) 

(check-expect (taller-than loi2 40)
              (cons (square 90 "solid" "green") empty)) 
                                                        

(check-expect (taller-than loi3 16) 
              (cons (square 30 "solid" "blue") (cons (triangle 20 "solid" "red") empty)))

(check-expect (taller-than loi3 10) loi3) 


(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [else (cond [(> (image-height (first loi)) cutoff)
                     (cons (first loi) (taller-than (rest loi) cutoff))]
                    [else (taller-than (rest loi) cutoff)])]))







        


(define losi1 empty)
(define losi2 (cons (make-simple-img "triangle" "red" 800)
                    (cons (make-simple-img "square" "green" 8100) empty)))

(define losi3 (cons (make-simple-img "circle" "blue" 2800)
                    (cons (make-simple-img "square" "pink" 100)
                          (cons (make-simple-img "circle" "red" 10) empty))))

(define losi4 (cons (make-simple-img "square" "blue" 2800)
                    (cons (make-simple-img "square" "pink" 100)
                          (cons (make-simple-img "square" "red" 10) empty))))


(define (fn-for-losi losi)
  (cond [(empty? losi) (...)]
        [else (... (first losi)
              (fn-for-losi (rest losi)))]))




         
         
         
         
         

 


(check-expect (find-shape losi1 "triangle") empty) 
(check-expect (find-shape losi2 "circle") empty) 
(check-expect (find-shape losi3 "circle") 
              (cons (build-image (make-simple-img "circle" "blue" 2800))
                    (cons (build-image (make-simple-img "circle" "red" 10)) empty)))
(check-expect (find-shape losi4 "square") 
              (cons (build-image (make-simple-img "square" "blue" 2800))
                    (cons (build-image (make-simple-img "square" "pink" 100))
                          (cons (build-image (make-simple-img "square" "red" 10)) empty))))
(check-expect (find-shape losi4 "SQUaRe") 
              (cons (build-image (make-simple-img "square" "blue" 2800))
                    (cons (build-image (make-simple-img "square" "pink" 100))
                          (cons (build-image (make-simple-img "square" "red" 10)) empty))))


(define (find-shape losi type)
  (cond [(empty? losi) empty]
        [else (cond [(string=? (string-downcase type) (simple-img-shape (first losi)))
                     (cons (build-image (first losi)) (find-shape (rest losi) type))]
                    [else (find-shape (rest losi) type)])]))









(define lod1 (cons (make-diff "ins" 3 "champ") empty))
(define lod2 (cons (make-diff "del" 1 2) (cons (make-diff "ins" 1 "mao") empty)))


(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else (... (first lod)
              (fn-for-lod (rest lod)))]))




         
         

 


(check-expect (updates empty "bruh") "bruh") 
(check-expect (updates lod1 "pog") "pogchamp") 
(check-expect (updates lod2 "lol") "lmao") 


(define (updates lod orig-string)
  (cond [(empty? lod) orig-string]
        [else (updates (rest lod) (update orig-string (first lod)))]))