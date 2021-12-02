

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assingment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))









(check-expect (backwards "") "")
(check-expect (backwards "a") "a")
(check-expect (backwards "dhcb") "bchd")
(check-expect (backwards "racket") "tekcar")

(define (backwards string) "")











(define-struct customer (fname lname age zip))

(define customer1 (make-customer "Bob" "Racket" 12 29672))
(define customer2 (make-customer "Bob" "Ross" 50 62109))
(define customer3 (make-customer "John" "Paul" 30 12358))

    
    
    






(check-expect (birthday customer1) 
   (make-customer (customer-fname customer1) (customer-lname customer1) (+ (customer-age customer1) 1) (customer-zip customer1)))
(check-expect (birthday customer2) 
   (make-customer (customer-fname customer2) (customer-lname customer2) (+ (customer-age customer2) 1) (customer-zip customer2)))
(check-expect (birthday customer3) 
   (make-customer (customer-fname customer3) (customer-lname customer3) (+ (customer-age customer3) 1) (customer-zip customer3)))









(define (birthday customer)
   (make-customer 
      (customer-fname customer)
      (customer-lname customer)
      (+ (customer-age customer) 1) 
      (customer-zip customer))) 
      






(check-expect (name-change customer1 "" "") 
	(make-customer "" "" (customer-age customer1) (customer-zip customer1)))
(check-expect (name-change customer2 "Gregor" "Kiczales") 
  (make-customer "Gregor" "Kiczales" (customer-age customer2) (customer-zip customer2)))
(check-expect (name-change customer3 "John" "Doe") 
  (make-customer "John" "Doe" (customer-age customer3) (customer-zip customer3)))
  









(define (name-change customer fname lname)
   (make-customer fname lname (customer-age customer) (customer-zip customer)))   
   
   
   

(require 2htdp/image)







(check-expect (bigger? SI SI2) true)




(check-expect (build-image SI) .)







(define-struct simple-img (shape area color))

(define SI (make-simple-img "triangle" 250 "red"))
(define SI1 (make-simple-img "circle" 120 "red"))
(define SI2 (make-simple-img "square" 200 "purple"))
(define SI3 (make-simple-img "circle" 40 "purple"))


 				







(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI SI3) true)


 
  
(define (bigger? simple-img1 simple-img2) 
	(> (simple-img-area simple-img1) (simple-img-area simple-img2)))







(check-expect (build-image SI1) (circle (sqrt (/ (simple-img-area SI1) pi)) "solid" "red"))
(check-expect (build-image SI2) (square (sqrt (simple-img-area SI2)) "solid" "purple"))





(define (build-image simple-img)
	(cond [(string=? (simple-img-shape simple-img) "triangle") 
           (triangle (triangle-side simple-img) "solid" (simple-img-color simple-img))]
        [(string=? (simple-img-shape simple-img) "circle") 
           (circle (circle-radius simple-img) "solid" (simple-img-color simple-img))]
        [(string=? (simple-img-shape simple-img) "square")
           (square (square-side simple-img) "solid" (simple-img-color simple-img))]))
     
     





(check-within (triangle-side SI) (sqrt (/ (* (simple-img-area SI) 4) (sqrt 3))) .1)



  
(define (triangle-side simple-img) 
	(sqrt (/ (* (simple-img-area simple-img) 4) (sqrt 3))))
        
        





(check-within (circle-radius SI1) (sqrt (/ (simple-img-area SI1) pi)) .1)




   
(define (circle-radius simple-img) 
   (sqrt (/ (simple-img-area simple-img) pi)))
   
   





(check-within (square-side SI2) (sqrt (simple-img-area SI2)) .1)





(define (square-side simple-img)
   (sqrt (simple-img-area simple-img)))
   
   
   







(define-struct diff (string insert? start-pos))

(define diff1 (make-diff "" false 0))
(define diff2 (make-diff "hello" true 3))
(define diff3 (make-diff "delete" false 1))
(define diff4 (make-diff "xyzzy" true 1))
(define diff5 (make-diff "34" false 3))

(define (fn-for-diff diff)
	(...
  	diff-string		
    diff-insert?	
    diff-start-pos))	
    










(check-expect (update "0123456789" diff4) "0xyzzy123456789")
(check-expect (update "0123456789" diff5) "01256789")

 
    
(define (update string diff)
  (if (diff-insert? diff)
    (insert-diff string diff)
    (delete-diff string diff)))
    





(check-expect (insert-diff "0123456789" diff4) "0xyzzy123456789")
(check-expect (insert-diff "yes" diff2) "yeshello")


 
    
(define (insert-diff string diff)
   (string-append (substring string 0 (diff-start-pos diff)) (diff-string diff) 
   (substring string (diff-start-pos diff) (string-length string))))
   





(check-expect (delete-diff "0123456789" diff5) "01256789")
(check-expect (delete-diff "yes" diff1) "yes")


 
    
(define (delete-diff string diff)
   (string-append (substring string 0 (diff-start-pos diff)) 
   (substring string (+ (diff-start-pos diff) (string-length (diff-string diff)))
              (string-length string))))
   
   


(define delete-a (make-diff "a" false 4))
(define delete-to (make-diff "to" false 11))
(define insert-we (make-diff "we" true 4))
(define insert-yester (make-diff "yester" true 13))











(define loi1 (cons (square 10 "solid" "blue") empty))
(define loi2 empty)
(define loi3 (cons (square 10 "solid" "blue")
                   (cons (circle 12 "solid" "red") empty)))


 
      
      





(check-expect (total-width loi2) 0)
(check-expect (total-width loi1) (image-width (square 10 "solid" "blue")))
(check-expect (total-width loi3) (+ (image-width (square 10 "solid" "blue"))
                                    (image-width (circle 12 "solid" "red"))))


 
                
(define (total-width loi)
	(cond [(empty? loi) 0]
        [else
           (+ (image-width (first loi))
          		  (total-width (rest loi)))]))
                





(define loi4 (cons (square 10 "solid" "blue")
                   (cons (circle 20 "solid" "yellow")
                         (cons (triangle 25 "solid" "purple") empty))))
(define loi5 empty)
(define loi6 (cons (circle 12 "solid" "red")
                   (cons (triangle 15 "solid" "blue")
                         (cons (square 20 "solid" "green")
                               (cons (circle 5 "outline" "orange") empty)))))

(check-expect (taller-than loi4 15) (cons (circle 20 "solid" "yellow")
                                          (cons (triangle 25 "solid" "purple") empty)))
(check-expect (taller-than loi5 1) empty)
(check-expect (taller-than loi6 15) (cons (circle 12 "solid" "red")
                                          (cons (square 20 "solid" "green") empty)))


 
                
(define (taller-than loi cutoff)
	(cond [(empty? loi) empty]
  			[(> (image-height (first loi)) cutoff) (cons (first loi) (taller-than (rest loi) cutoff))]
				[else (taller-than (rest loi) cutoff)]))
                





(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))

(check-expect (total-width LOI) 265) 

(check-expect (taller-than LOI 30)
              (list . .))











(define losi1 (cons (make-simple-img "triangle" 40 "red")
                    (cons (make-simple-img "square" 30 "blue")
                          (cons (make-simple-img "triangle" 52 "yellow") empty))))
(define losi2 (cons (make-simple-img "circle" 32 "orange")
                    (cons (make-simple-img "triangle" 26 "green") 
                    (cons (make-simple-img "circle" 15 "red")
                          (cons (make-simple-img "circle" 10 "blue") empty)))))
(define losi3 (cons (make-simple-img "square" 50 "blue")
                    (cons (make-simple-img "square" 18 "red")
                          (cons (make-simple-img "triangle" 30 "yellow") empty))))



 
                





(check-expect (find-shape losi1 "triangle") (cons (build-image (make-simple-img "triangle" 40 "red"))
                                                  (cons (build-image (make-simple-img "triangle" 52 "yellow")) empty)))
(check-expect (find-shape losi2 "circle") (cons (build-image (make-simple-img "circle" 32 "orange"))
                                                (cons (build-image (make-simple-img "circle" 15 "red"))
                                                      (cons (build-image (make-simple-img "circle" 10 "blue")) empty))))
(check-expect (find-shape losi3 "circle") empty)






(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))


 
                
(define (find-shape losi shape)
	(cond [(empty? losi) empty]
        [else
           (if (string=? (simple-img-shape (first losi)) shape)
               (cons (build-image (first losi)) (find-shape (rest losi) shape))
          		  (find-shape (rest losi) shape))]))








(define lod1 (cons (make-diff "hello" true 2) (cons (make-diff "car" false 2)
                                                    (cons (make-diff "whale" true 4) empty))))
(define lod2 empty)
(define lod3 (cons (make-diff "xyz" true 4) (cons (make-diff "abcd" true 1) empty)))
(define lod4 (cons (make-diff "lmnop" false 4) (cons (make-diff "qrs" false 1) empty)))


 
                





(check-expect (updates lod1 "abcdef") (update (update (update "abcdef" (make-diff "hello" true 2))
                                                      (make-diff "car" false 2)) (make-diff "whale" true 4)))
(check-expect (updates lod2 "hello") "hello")
(check-expect (updates lod3 "work") (update (update "work"
                                                    (make-diff "xyz" true 4)) (make-diff "abcd" true 1)))
(check-expect (updates lod4 "xqrslmnop") (update (update "xqrslmnop"
                                                         (make-diff "lmnop" false 4)) (make-diff "qrs" false 1)))


 
                
(define (updates lod String)
	(cond [(empty? lod) String]
        [else
           (updates (rest lod) (update String (first lod)))]))