

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assignment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)








(check-expect (backwards "") "") 
(check-expect (backwards "forward") "drawrof") 
(check-expect (backwards "racecar") "racecar") 

(define (backwards word) 
  "")







(define-struct customer (first last age zip))





(define obama (make-customer "barack" "obama" 50 "20500"))

 




(check-expect (birthday (make-customer "john" "smith" 20 "13040")) 
              (make-customer "john" "smith" 21 "13040"))
(check-expect (birthday (make-customer "dave" "jones" 0 "34029")) 
              (make-customer "dave" "jones" 1 "34029"))

(define (birthday person)
  (make-customer
   (customer-first person)
   (customer-last person)
   (+ (customer-age person) 1)
   (customer-zip person)))




(check-expect (name-change (make-customer "john" "smith" 20 "13040") "dave" "jones") 
              (make-customer "dave" "jones" 20 "13040")) 
(check-expect (name-change (make-customer "bob" "ross" 37 "41567") "" "")
              (make-customer "" "" 37 "41567")) 
(check-expect (name-change (make-customer "" "" 28 "12345") "goat" "man")
              (make-customer "goat" "man" 28 "12345")) 

(define (name-change person new_f new_l)
  (make-customer
  new_f
  new_l
  (customer-age person)
  (customer-zip person)))




(define-struct simple-img (shape color area))





(define blue-circle (make-simple-img "circle" "blue" "200"))

 




(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "circle" "purple" 200))
(define SI3 (make-simple-img "square" "green" 250))
(define SI4 (make-simple-img "circle" "yellow" 100))
(define SI5 (make-simple-img "triangle" "red" 0))

(check-expect (bigger? SI SI2) #true) 
(check-expect (bigger? SI SI3) #false) 
(check-expect (bigger? SI4 SI3) #false) 

(define (bigger? simp1 simp2)
  (> (simple-img-area simp1) (simple-img-area simp2)))
   




(check-expect (build-image SI)
              (triangle (sqrt (/ (* 4 250) (sqrt 3))) "solid" "red")) 
(check-expect (build-image SI2)
              (circle (sqrt (/ 200 pi)) "solid" "purple")) 
(check-expect (build-image SI3)
              (square (sqrt 250) "solid" "green")) 
(check-expect (build-image SI5)
              (triangle 0 "solid" "red")) 

(define (build-image simp)
  (cond
  [(string=? (simple-img-shape simp) "triangle")
   (triangle
   (sqrt(/ (* 4 (simple-img-area simp)) (sqrt 3))) 
   "solid"
   (simple-img-color simp))] 
  
  [(string=? (simple-img-shape simp) "circle")
   (circle
   (sqrt (/ (simple-img-area simp) pi)) 
   "solid"
   (simple-img-color simp))]
  
  [(string=? (simple-img-shape simp) "square")
   (square
   (sqrt (simple-img-area simp)) 
   "solid"
   (simple-img-color simp))]))



(define-struct diff (ins pos edit num))









(define diff_insert (make-diff #true 5 "ghj" 0)) 
(define diff_delete (make-diff #false 10 "" 4))  

 





(check-expect (update "abcdhij"
              (make-diff #true 4 "efg" 0)) 
              "abcdefghij") 
(check-expect (update "klmnoxxxp"
              (make-diff #false 5 "" 3))
              "klmnop")     


(check-expect (update "345"
              (make-diff #true 0 "12" 0))
              "12345")   
(check-expect (update "123"
              (make-diff #true 3 "45" 0))
              "12345")   
(check-expect (update "67891234"
              (make-diff #false 0 "" 4))
              "1234")    
(check-expect (update "67891234"
              (make-diff #false 4 "" 4))
              "6789")    

(define (update base change)
  (cond
    [(diff-ins change)
     (string-append (substring base 0 (diff-pos change))
                    (diff-edit change)
                    (substring base (diff-pos change) ))]
    [else
     (string-append (substring base 0 (diff-pos change))
                    (substring base (+ (diff-pos change) (diff-num change))))]
     ))



(define greet "How are you today?")
(define d1 (make-diff #false 4 "" 3)) 
(define d2 (make-diff #false 9 "" 5))
(define i1 (make-diff #true 4 "were" 0)) 
(define i2 (make-diff #true 13 "yesterday" 0))

(check-expect (update (update (update (update greet d1) d2) i1) i2)
              "How were you yesterday?")












(define loi1 empty) 
(define loi2 (list (triangle 10 "solid" "red")
                   (triangle 20 "solid" "red")
                   (triangle 30 "solid" "red"))) 
(define loi3 (cons (triangle 10 "solid" "red")
             (cons (triangle 20 "solid" "red")
             (cons (triangle 30 "solid" "red") empty)))) 

  




(check-expect (total-width empty) 0) 
(check-expect (total-width (list (triangle 50 "solid" "blue")
                                 (square 30 "solid" "green")
                                 (circle 200 "solid" "purple"))) 480) 
(check-expect (total-width (list (rectangle 0 0 "solid" "yellow")
                                 (square 0 "solid" "green"))) 0) 
                                 
               
(define (total-width loi)
  (cond [(empty? loi) 0]                      
        [(cons? loi) (+ (image-width (first loi))
                        (total-width (rest loi)))]))




(check-expect (taller-than empty 100) empty) 
(check-expect (taller-than (list (circle 60 "solid" "purple")
                                 (triangle 50 "solid" "blue")
                                 (square 30 "solid" "green")
                                 (circle 60 "solid" "purple"))
                                 40)
                           (list (circle 60 "solid" "purple")
                                 (triangle 50 "solid" "blue")
                                 (circle 60 "solid" "purple"))) 
(check-expect (taller-than (list (circle 60 "solid" "purple")
                                 (triangle 50 "solid" "blue")
                                 (square 30 "solid" "green")
                                 (circle 60 "solid" "purple")) 
                                 2000)
                           empty)


(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]                     
        [(cons? loi)
         (if (> (image-height (first loi)) cutoff)
             (cons (first loi) (taller-than (rest loi) cutoff))
             (taller-than (rest loi) cutoff))]))








(define losi1 empty) 
(define losi2 (list (make-simple-img "triangle" "blue" 50)
                    (make-simple-img "square" "orange" 20)
                    (make-simple-img "circle" "white" 40))) 
(define losi3 (cons (make-simple-img "circle" "white" 40)
              (cons (make-simple-img "square" "orange" 20)
              (cons (make-simple-img "triangle" "blue" 50)
                    empty))))                               

  




(check-expect (find-shape empty "circle") empty) 
(check-expect (find-shape (list (make-simple-img "circle" "yellow" 600)
                                (make-simple-img "triangle" "blue" 50)
                                (make-simple-img "square" "orange" 20)
                                (make-simple-img "circle" "white" 400))
                          "circle")
                          (list (circle (sqrt (/ 600 pi)) "solid" "yellow" )
                                (circle (sqrt (/ 400 pi)) "solid" "white" ))) 
(check-expect (find-shape (list (make-simple-img "circle" "yellow" 600)
                                (make-simple-img "triangle" "blue" 50))                        
                          "square")
              empty) 

(define (find-shape losi shape)
  (cond [(empty? losi) empty]                       
        [(cons? losi)
         (if (string=? (simple-img-shape (first losi)) shape)
             (cons (build-image (first losi)) (find-shape (rest losi) shape))
             (find-shape (rest losi) shape))]))








(define lod1 empty) 
(define lod2 (list (make-diff #t 6 "abc" 0)
                   (make-diff #f 4 "" 2))) 
(define lod3 (cons (make-diff #t 6 "abc" 0)
             (cons (make-diff #f 4 "" 2)
              empty))) 

  





(check-expect (updates empty "Shashkov") "Shashkov") 
(check-expect (updates (list d1 d2 i1 i2) "How are you today?") 
              "How were you yesterday?")
(check-expect (updates (list (make-diff #f 4 "" 2)
                             (make-diff #t 7 "hi" 0))
                       "abcdxxefgjk")
              "abcdefghijk") 

(define (updates lod text)
  (cond [(empty? lod) text]                       
        [(cons? lod)
         (updates (rest lod) (update text (first lod)))]))
         



  




  