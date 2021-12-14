

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Assignment1CS1102) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)






(check-expect (backwards "abc") "cba")
(check-expect (backwards "b") "b")
(check-expect (backwards "") "")
(check-expect (backwards "racecar") "racecar")

(define (backwards str) "") 











(define-struct customer (fn ln age zip))

(define Stu1 (make-customer "Aashi" "Mehta" 17 "01601"))
(define Stu2 (make-customer "Jon" "Cili" 12 "01504"))
(define Stu3 (make-customer "Joseph" "Beck" 45 "01789"))

 




(check-expect (birthday Stu1)
              (make-customer "Aashi" "Mehta" (+ 1 17) "01601"))
(check-expect (birthday Stu2)
              (make-customer "Jon" "Cili" (+ 1 12) "01504"))
(check-expect (birthday Stu3)
              (make-customer "Joseph" "Beck" (+ 1 45) "01789"))



(define (birthday cust) 
  (make-customer (customer-fn cust)
                 (customer-ln cust)
                 (+ 1  (customer-age cust))
                 (customer-zip cust)))




(check-expect (name-change Stu1 "Morgan" "Smith")
              (make-customer "Morgan" "Smith" 17 "01601"))
(check-expect (name-change Stu2 "Joshua" "Beckman")
              (make-customer "Joshua" "Beckman" 12 "01504"))
(check-expect (name-change Stu3 "Stanley" "Beck")
              (make-customer "Stanley" "Beck" 45 "01789"))



(define (name-change cust afn aln) 
  (make-customer afn aln (customer-age cust) (customer-zip cust)))   
                           
                           







              





              
(define-struct simple-img (shape col area))  

(define simg1 (make-simple-img "circle" "red" 50))
(define simg2 (make-simple-img "circle" "red" 60))
(define simg3 (make-simple-img "square" "blue" 25))
(define simg4 (make-simple-img "square" "yellow" 10))
(define simg5 (make-simple-img "triangle" "red" 50))
(define simg6 (make-simple-img "circle" "purple" 50))
  
 
   



(check-expect (bigger? simg1 simg2) false)
(check-expect (bigger? simg3 simg4) true)
(check-expect (bigger? simg5 simg6) false)
   


(define (bigger? simg1 simg2)
  (if (> (simple-img-area simg1)
         (simple-img-area simg2))
      true
      false))



(check-expect (build-img simg1)
              (circle (sqrt (/ (simple-img-area simg1) pi))
                      "solid" (simple-img-col simg1)))
(check-expect (build-img simg2)
              (circle (sqrt (/ (simple-img-area simg2) pi))
                      "solid" (simple-img-col simg2)))
(check-expect (build-img simg3)
              (square (sqrt (simple-img-area simg3))
                      "solid" (simple-img-col simg3)))
(check-expect (build-img simg4)
              (square (sqrt (simple-img-area simg4))
                      "solid" (simple-img-col simg4)))  
(check-expect (build-img simg5)
              (triangle (sqrt (/ (* 4 (simple-img-area simg5)) (sqrt 3)))
                        "solid" (simple-img-col simg5)))   
(check-expect (build-img simg6)
              (circle (sqrt (/ (simple-img-area simg6) pi))
                      "solid" (simple-img-col simg6)))  
              


(define (build-img simg)
  (cond
    [(equal? "circle" (simple-img-shape simg))
     (circle (sqrt (/ (simple-img-area simg) pi))
             "solid" (simple-img-col simg))]
    [(equal? "triangle" (simple-img-shape simg)) 
     (triangle (sqrt (/ (* 4 (simple-img-area simg)) (sqrt 3)))
               "solid" (simple-img-col simg))]
    [(equal? "square" (simple-img-shape simg))
     (square (sqrt (simple-img-area simg))
             "solid" (simple-img-col simg))]))




















(define-struct diff (pos len cont))

(define diff1 (make-diff 11 4 "Jon "))   
(define diff2 (make-diff  11 4 ""))      
(define diff3 (make-diff 7 4 "WPI "))    
(define diff4 (make-diff 7 4 ""))        
(define diff5 (make-diff 0 6 "Sure, "))  
(define diff6 (make-diff 0 6 ""))

 





(check-expect (update "My name is Cili" diff1)
              "My name is Jon Cili")
(check-expect (update "My name is Jon Cili" diff2)
              "My name is Cili")
(check-expect (update "I am a student" diff3)
              "I am a WPI student")
(check-expect (update "I am a WPI student" diff4)
              "I am a student")
(check-expect (update "I would be happy to join you." diff5)
              "Sure, I would be happy to join you.")
(check-expect (update "Sure, I would be happy to join you." diff6)
              "I would be happy to join you.")



(define (update str adiff)
  (cond
    [(> (string-length (diff-cont adiff)) 0) 
     (string-append 
      (substring str 0 (diff-pos adiff)) 
      (diff-cont adiff) 
      (substring str (diff-pos adiff)))]
    [else
     (string-append 
      (substring str 0 (diff-pos adiff)) 
      (substring str (+ (diff-pos adiff) (diff-len adiff))))]))






(define STR1-INIT "How are you today?")       
(define STR1-END "How were you yesterday?")   





(define diffset1-del-1 (make-diff 4 4 ""))
(define diffset1-del-2 (make-diff 7 6 ""))
(define diffset1-ins-3 (make-diff 4 5 "were "))
(define diffset1-ins-4 (make-diff 12 10 " yesterday"))

(check-expect (update STR1-INIT diffset1-del-1) "How you today?")
(check-expect (update "How you today?" diffset1-del-2) "How you?")
(check-expect (update "How you?" diffset1-ins-3) "How were you?")
(check-expect (update "How were you?" diffset1-ins-4) STR1-END)









 

(define Img1 (square 50 "solid" "green"))
(define Img2 (circle 35 "outline" "purple"))
(define Img3 (rectangle 100 20 "solid" "yellow"))
(define Img4 (square 21 "outline" "orange"))
(define Img5 (rectangle 81 60 "outline" "blue"))
(define Img6 (circle 26 "solid" "red"))



(check-expect (total-width (list Img1)) 50)
(check-expect (total-width empty) 0)
(check-expect (total-width (list Img1 Img2 Img3)) 
              (+ 50 (* 2 35) 100))



(define (total-width aloi)
  (cond
    [(empty? aloi) 0]
    [else
     (+ (image-width (first aloi))
        (total-width (rest aloi)))]))




(check-expect (taller-than (list Img4) 20)
              (list Img4))
(check-expect (taller-than empty 100)
              empty)
(check-expect (taller-than (list Img1 Img2 Img3 Img4 Img5 Img6) 50) 
              (list Img2 Img5 Img6))
             
              


(define (taller-than aloi num)
  (cond
    [(empty? aloi) empty]
    [else
     (if (> (image-height (first aloi)) num)
         (cons (first aloi) (taller-than (rest aloi) num))
         (taller-than (rest aloi) num))]))









 



(check-expect (find-shape (list simg1 simg4 simg5 simg6) "circle") 
              (list (build-img simg1) (build-img simg6)))
(check-expect (find-shape empty "square") empty)
(check-expect (find-shape (list simg1 simg3 simg5) "triangle") 
              (list (build-img simg5)))



(define (find-shape alosi shape)
  (cond
    [(empty? alosi) empty]
    [else
     (if (equal? (simple-img-shape (first alosi)) shape)
         (cons (build-img (first alosi))
               (find-shape (rest alosi) shape))
         (find-shape (rest alosi) shape))]))



          





 

(define alod1            
  (list diffset1-del-1
        diffset1-del-2
        diffset1-ins-3
        diffset1-ins-4))

(define STR2-INIT "Professor Becker taught CS2102 at WPI.")
(define STR2-END "Professor Beck teaches CS1102 at WPI.")

(define diffset2-del-1 (make-diff 14 2 ""))
		
(define diffset2-ins-2 (make-diff 16 6 "eaches"))
		
(define diffset2-del-3 (make-diff 22 5 ""))
		
(define diffset2-del-4 (make-diff 25 1 ""))
		
(define diffset2-ins-5 (make-diff 26 1 "1"))
		

(define alod2
  (list diffset2-del-1
        diffset2-ins-2
        diffset2-del-3
        diffset2-del-4
        diffset2-ins-5))

(define STR3-INIT "WPI has 150 graduate programs.")
(define STR3-END "WPI has 33 undergraduate programs.")

(define diffset3-del-1 (make-diff 8 4 ""))
  	
(define diffset3-ins-2 (make-diff 8 8 "33 under"))
		

(define alod3
  (list diffset3-del-1
        diffset3-ins-2))




(check-expect (updates STR1-INIT alod1) STR1-END)
(check-expect (updates STR2-INIT alod2) STR2-END)
(check-expect (updates STR3-INIT alod3) STR3-END)
(check-expect (updates "This sentence should not change." empty)
              "This sentence should not change.")



(define (updates str alod)
  (cond
    [(empty? alod) str] 
    [else
     (updates (update str (first alod)) (rest alod))]))