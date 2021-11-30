

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Assignment1ColeConnor-9-1-21_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))







(define (backwards str) "") 

(check-expect (backwards "") "")
(check-expect (backwards "a") "a")
(check-expect (backwards "ab") "ba")
(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "aaaa") "aaaa")





(define-struct customer (firstname lastname age zipcode))       







(define Cole (make-customer "Cole" "Welcher" 18 90275))
(define Connor (make-customer "Connor" "Ehrensperger" 18 18634))     

(define (fn-for-customer customer)
(... (customer-firstname customer)   
     (customer-lastname customer)    
     (customer-age customer)         
     (customer-zipcode customer)     
))  











(check-expect (birthday Cole) (make-customer (customer-firstname Cole)
                                             (customer-lastname Cole)
                                             (+ (customer-age Cole) 1)
                                             (customer-zipcode Cole)))

(check-expect (birthday Connor) (make-customer (customer-firstname Connor)
                                               (customer-lastname Connor)
                                               (+ (customer-age Connor) 1)
                                               (customer-zipcode Connor)))

(define (birthday cust)
   (make-customer (customer-firstname cust)  
                  (customer-lastname cust)   
                  (+ (customer-age cust) 1)  
                  (customer-zipcode cust)    
                       
	 )
)








(check-expect (name-change Connor "Connor" "E") (make-customer "Connor" "E"
                                                               (customer-age Connor)
                                                               (customer-zipcode Connor)))
(check-expect (name-change Cole "Cole" "Welcher") (make-customer "Cole" "Welcher"
                                                               (customer-age Cole)
                                                               (customer-zipcode Cole)))
(check-expect (name-change Connor "" "") (make-customer "" ""
                                                        (customer-age Connor)
                                                        (customer-zipcode Connor)))

(define (name-change cust nfn nln)
  (make-customer        nfn                        
                        nln                        
                        (customer-age cust)        
                        (customer-zipcode cust)    
  )
 )



(require 2htdp/image)
(define BLANK (square 0 "outline" "white"))










(define shape1 "triangle")
(define shape2 "square")
(define shape3 "circle")


(define (fn-for-shape shape)           
  (cond [(string=? "tirangle" shape) (...)]
        [(string=? "square" shape) (...)]
        [(string=? "square" shape) (...)]))


(define-struct simple-img (shape color area))






(define SI (make-simple-img shape1 "red" 250))
(define SI2 (make-simple-img shape2 "purple" 200))
(define SI6 (make-simple-img shape1 "red" 30))
(define SI4 (make-simple-img shape2 "blue" 20))
(define SI5 (make-simple-img shape3 "green" 20))



(define (fn-for-simple-img image)
                   (... (fn-for-shape (simple-img-shape image))
                                      (simple-img-color image)
                                      (simple-img-area image)))








(check-expect (build-image SI) .)

(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI6 SI2) false)
(check-expect (bigger? SI6 SI) false)
(check-expect (bigger? SI4 SI5) false)

(define (bigger? image1 image2)
                   (if (> (simple-img-area image1)
                          (simple-img-area image2))
                       true
                       false))







(check-expect (build-image SI6)
              (triangle
               (expt
                (/ (* (simple-img-area SI6) 4)
                   (expt 3 1/2))
                1/2)
               "solid"
               (simple-img-color SI6)))

(check-expect (build-image SI4)
              (square
               (expt (simple-img-area SI4) 1/2)
               "solid"
               (simple-img-color SI4)))

(check-expect (build-image SI5)
              (circle
               (expt (/ (simple-img-area SI5) pi)
                     1/2)
               "solid"
               (simple-img-color SI5)))

(check-expect (build-image (make-simple-img "oval" "red" 400)) BLANK)

(define (build-image image)
  (cond [(string=? "triangle" (simple-img-shape image)) (triangle   
               (expt                                                
                (/ (* (simple-img-area image) 4)
                   (expt 3 1/2))
                1/2)
               "solid"
               (simple-img-color image))]
        [(string=? "square" (simple-img-shape image)) (square       
               (expt (simple-img-area image) 1/2)                   
               "solid"
               (simple-img-color image))]
        [(string=? "circle" (simple-img-shape image)) (circle       
               (expt (/ (simple-img-area image) pi)                 
                     1/2)
               "solid"
               (simple-img-color image))]
        [else BLANK]))







(define-struct insertion-diff (position text))






(define INSERTION1 (make-insertion-diff 0 "insertion")) 
                                                        
                                                        

(define INSERTION2 (make-insertion-diff 1 "insertion")) 
                                                        
                                                        

(define INSERTION3 (make-insertion-diff 3 "insertion")) 
                                                        
                                                        
(define INSERTION4 (make-insertion-diff 1 "xyzzy"))     
(define INS INSERTION4)                                 


(define (fn-for-insertion-diff insertion-diff)
  (... (insertion-diff-position insertion-diff)
       (insdertion-diff-text insertion-diff)))

(define-struct deletion-diff (position num-of-chars))







(define DELETION1 (make-deletion-diff 0 0)) 
                                            

(define DELETION2 (make-deletion-diff 1 1)) 
                                            

(define DELETION3 (make-deletion-diff 0 3)) 
                                            
(define DELETION4 (make-deletion-diff 3 2)) 
(define DEL DELETION4)


(define (fn-for-deletion-diff deletion-diff)
  (... (deletion-diff-position deletion-diff)
       (deletion-diff-text deletion-diff)))








(define DIFF1 INSERTION1)
(define DIFF2 DELETION1)


(define (fn-for-diff diff)
  (cond [(insertion-diff? diff)
         (... (fn-for-insertion-diff diff))]
        [else
         (... (fn-for-deletion-diff diff))]))







(check-expect (update "" INSERTION1) "insertion")
(check-expect (update "abc" INSERTION2) "ainsertionbc")
(check-expect (update "abc" INSERTION3) "abcinsertion")
(check-expect (update "abc" DELETION1) "abc")
(check-expect (update "abc" DELETION2) "ac")
(check-expect (update "abc" DELETION3) "")
(check-expect (update "0123456789" INS) "0xyzzy123456789")
(check-expect (update "0123456789" DEL) "01256789")

(define (update text diff)
  (cond [(insertion-diff? diff)
         (insert text diff)]
        [else
         (delete text diff)]))




(check-expect (update "" INSERTION1) "insertion")
(check-expect (update "abc" INSERTION2) "ainsertionbc")
(check-expect (update "abc" INSERTION3) "abcinsertion")

(define (insert text diff)
  (string-append 
         (string-append (substring text 0 (insertion-diff-position diff))
          (insertion-diff-text diff))
          (substring text (insertion-diff-position diff) (string-length text))))



(check-expect (update "abc" DELETION1) "abc")
(check-expect (update "abc" DELETION2) "ac")
(check-expect (update "abc" DELETION3) "")

(define (delete text diff)
  (string-append
         (substring text 0 (deletion-diff-position diff))
              (substring text
                             (+
                              (deletion-diff-position diff)
                              (deletion-diff-num-of-chars diff))
                             (string-length text))
                   ))





(define deletion (make-deletion-diff 4 1))
(define deletion2 (make-deletion-diff 11 2))
(define insertion (make-insertion-diff 4 "we"))
(define insertion2 (make-insertion-diff 13 "yester"))

(update
 (update
  (update
   (update "How are you today?"
   deletion)
  deletion2)
 insertion)
insertion2)













(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))
(check-expect (total-width LOI) 265) 

(check-expect (taller-than LOI 30)
              (list . .))



(define LOI1 empty)
(define LOI2 (list (square 40 "solid" "blue")))
(define LOI3 (list (square 40 "solid" "blue") BLANK))
(define LOI4 (list (square 40 "solid" "blue")
                   (rectangle 20 30 "solid" "yellow")))
(define LOI5 (list (square 40 "solid" "blue")
                   (rectangle 20 30 "solid" "yellow")
                   (circle 50 "solid" "green")))

(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (first loi)
                   (fn-for-loi (rest loi)))]))





(check-expect (total-width empty) 0)
(check-expect (total-width LOI2) (image-width (first LOI2)))
(check-expect (total-width LOI4)
              (+ (image-width (first LOI4))
                 (image-width (first (rest LOI4)))))



(define (total-width loi)
  (cond [(empty? loi) 0]
        [else (+ (image-width (first loi))
                   (total-width (rest loi)))]))





(check-expect (taller-than empty 0) empty)
(check-expect (taller-than LOI4 30) (list (first LOI4)))
(check-expect (taller-than LOI5 0) LOI5)
(check-expect (taller-than LOI5 30) (list (first LOI5)
                                         (first (rest (rest LOI5))))) 



(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) cutoff)
         (cons (first loi) (taller-than (rest loi) cutoff))]
        [else (taller-than (rest loi) cutoff)]))






(define LOSI empty)
(define LOSI1 (list SI SI2 SI6))
(define LOSI2 (list SI))


(define (fn-for-losi losi)
  (cond [(empty? losi) (...)]
        [else (... (fn-for-simple-img (first losi))
                   (fn-for-losi (rest losi)))]))










(define SI1 (make-simple-img shape3 "red" 120))
(define SI3 (make-simple-img shape3 "purple" 40))
(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))


(check-expect (find-shape empty "circle") empty)
(check-expect (find-shape LOSI1 "triangle") (list (build-image
                                                   (first LOSI1))
                                                  (build-image
                                                   (first (rest (rest LOSI1))))))
(check-expect (find-shape LOSI1 "oval") empty)

(check-expect (find-shape LOSI1 "square") (list (build-image (first (rest LOSI1)))))


(define (find-shape losi type)
  (cond [(empty? losi) empty]
        [(string=?(simple-img-shape (first losi)) type) 
          (cons (build-image (first losi)) (find-shape (rest losi) type))]
        [else (find-shape (rest losi) type)]))







(define DIFF3 DELETION2)
(define LODS1 empty)
(define LODS2 (list DIFF1))
(define LODS3 (list DIFF1 DIFF3))
(define LODS4 (list DIFF3))


(define (fn-for-lods lods)
 (cond [(empty? lods) (...)]
       [else (... (fn-for-diff (first lods))
                  (fn-for-lods (rest lods)))]))
                  







(check-expect (updates LODS2 "hi") (update "hi" (first LODS2)))
(check-expect (updates LODS1 "hi") "hi")
(check-expect (updates LODS3 "hi") (update
                                    (update "hi" (first LODS3))
                                   (first (rest LODS3))))
(check-expect (updates LODS4 "hi") (update "hi" (first LODS4)))


(check-expect (updates (list DEL INS) "0123456789") "0xyzzy1256789")

(define (updates lods text)
  (cond 
       [(empty? lods) text]
       [else (updates (rest lods) (update text (first lods)))]
  )
)
