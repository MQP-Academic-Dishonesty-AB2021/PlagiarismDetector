

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
































(define-struct customer (fname lname age zip))

(define C1 (make-customer "Billy" "Bob" 32 95032))
(define C2 (make-customer "John" "Smith" 150 87654))
(define C3 (make-customer "Frank" "Joe" 0 12345))
(define C4 (make-customer "" "" 100 54321))

 
		




(check-expect (birthday C1)
              (make-customer (customer-fname C1)
                             (customer-lname C1)
                             (+ (customer-age C1) 1)
                             (customer-zip C1)))
(check-expect (birthday C2)
              (make-customer (customer-fname C2)
                             (customer-lname C2)
                             (+ (customer-age C2) 1)
                             (customer-zip C2)))
(check-expect (birthday C3)
              (make-customer (customer-fname C3)
                             (customer-lname C3)
                             (+ (customer-age C3) 1)
                             (customer-zip C3)))



(define (birthday c)  
  (make-customer (customer-fname c)
                 (customer-lname c)
                 (+ 1 (customer-age c))
                 (customer-zip c)))




(check-expect (name-change C1 "Samuel" "Adams")
              (make-customer "Samuel" "Adams"
                             (customer-age C1)
                             (customer-zip C1)))
(check-expect (name-change C2 "Sydney" "Crosby")
              (make-customer "Sydney" "Crosby"
                             (customer-age C2)
                             (customer-zip C2)))
(check-expect (name-change C4 "Cristiano" "Ronaldo")
              (make-customer "Cristiano" "Ronaldo"
                             (customer-age C4)
                             (customer-zip C4)))
(check-expect (name-change C1 "" "")
              (make-customer "" ""
                             (customer-age C1)
                             (customer-zip C1)))


(define (name-change c new-first new-last)
  (make-customer new-first
                 new-last
                 (customer-age c)
                 (customer-zip c)))















(define-struct simple-img (shape color area))

(define SI1 (make-simple-img
             "triangle"
             "red"
             250))
(define SI2 (make-simple-img
             "square"
             "purple"
             200))
(define SI3 (make-simple-img
             "circle"
             "blue"
             250))

 




(check-expect (bigger? SI1 SI2) true)
(check-expect (bigger? SI2 SI3) false)
(check-expect (bigger? SI1 SI3) false)



(define (bigger? SI1 SI2)
  (> (simple-img-area SI1) (simple-img-area SI2)))




(check-expect (build-image SI1) .)
(check-expect (build-image SI1) (triangle
                                 (sqrt (/ (* 4 (simple-img-area SI1)) (sqrt 3)))
                                 "solid"
                                 (simple-img-color SI1)))
(check-expect (build-image SI2) (square
                                 (sqrt (simple-img-area SI2))
                                 "solid"
                                 (simple-img-color SI2)))
(check-expect (build-image SI3) (circle
                                 (sqrt (/ (simple-img-area SI3) pi))
                                 "solid"
                                 (simple-img-color SI3)))

(check-expect (build-image (make-simple-img "square" "red" 0))
              (square
               0
               "solid"
               "red"))
(check-expect (build-image (make-simple-img "circle" "purple" 0))
              (circle
               0
               "solid"
               "purple"))
(check-expect (build-image (make-simple-img "triangle" "orange" 0))
              (triangle
               0
               "solid"
               "orange"))



(define (build-image SI)
  (cond [(string=? (string-downcase (simple-img-shape SI)) "triangle")
         (triangle 
          (sqrt (/ (* 4 (simple-img-area SI)) (sqrt 3)))
          "solid"
          (simple-img-color SI))]
        [(string=? (string-downcase (simple-img-shape SI)) "square")
         (square 
          (sqrt (simple-img-area SI))
          "solid"
          (simple-img-color SI))]
        [else
         (circle 
          (sqrt (/ (simple-img-area SI) pi))
          "solid"
          (simple-img-color SI))]))








(define-struct diff (is-adding? where to-add num-to-remove))






(define D1 (make-diff true 1 "abc" 0))    
(define D2 (make-diff false 0 "" 1))      
(define D3 (make-diff true 2 "diff" 0))   
(define D4 (make-diff true 0 "" 0))       
(define D5 (make-diff false 2 "" 2))      

 






(check-expect (update "Computer" D1) "Cabcomputer")
(check-expect (update "" D3) "diff")
(check-expect (update "Potato" D2) "otato")
(check-expect (update "Tomato" D4) "Tomato") 



(define (update text diff)
  (if (diff-is-adding? diff)
      (insert-text text diff)
      (delete-text text diff)))



(check-expect (insert-text "dog" D1) "dabcog")
(check-expect (insert-text "" D3) "diff")
(check-expect (insert-text "cat" D4) "cat")



(define (insert-text text diff)     
  (if (string=? "" text)
      (diff-to-add diff)
      (string-append (substring text 0 (diff-where diff)) 
                  (diff-to-add diff) 
                  (substring text (diff-where diff) (string-length text)))))



(check-expect (delete-text "dog" D2) "og")
(check-expect (delete-text "dogcat" D5) "doat")
(check-expect (delete-text "" D2) "")



(define (delete-text text diff)
  (if (string=? "" text)
      text
      (string-append (substring text 0 (diff-where diff))  
                     (substring text
                                (+ (diff-num-to-remove diff) (diff-where diff))
                                (string-length text)))))









(define question "How are you today?")

(define Step1 (make-diff false 12 "" 5))
(define Step2 (make-diff true 12 "yesterday" 0))
(define Step3 (make-diff false 4 "" 3))
(define Step4 (make-diff true 4 "were" 0))

(update (update (update (update question Step1) Step2) Step3) Step4)















 






(check-expect (total-width [list (square 0 "solid" "white")]) 0)
(check-expect (total-width [list (square 10 "solid" "white")
                                 (square 20 "solid" "white")
                                 (triangle 30 "solid" "white")])
              (+ (image-width (square 10 "solid" "white"))
                 (image-width (square 20 "solid" "white"))
                 (image-width (triangle 30 "solid" "white"))))
(check-expect (total-width [list (square 0 "solid" "white")
                                 (square 20 "solid" "white")
                                 (triangle 30 "solid" "white")])
              (+ (image-width (square 0 "solid" "white"))
                 (image-width (square 20 "solid" "white"))
                 (image-width (triangle 30 "solid" "white"))))
                           


(define (total-width loi)
  (cond [(empty? loi) 0]
        [else
         (+ (image-width (first loi))
            (total-width (rest loi)))]))







(check-expect (taller-than [list (square 0 "solid" "white")] 10) empty)
(check-expect (taller-than empty 10) empty)
(check-expect (taller-than [list (square 10 "solid" "white")
                                 (square 20 "solid" "white")
                                 (triangle 30 "solid" "white")]
                           0)
              [list (square 10 "solid" "white")
                    (square 20 "solid" "white")
                    (triangle 30 "solid" "white")])
(check-expect (taller-than [list (square 10 "solid" "white")
                                 (square 20 "solid" "white")
                                 (triangle 30 "solid" "white")]
                           10)
              [list (square 20 "solid" "white")
                    (triangle 30 "solid" "white")])
                                 



(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [else
         (if (> (image-height (first loi)) cutoff)
             (cons (first loi) (taller-than (rest loi) cutoff))
             (taller-than (rest loi) cutoff))]))












(define LOI1 [list SI1 SI2 SI3])

 
   





(check-expect (find-shape empty "square") empty)
(check-expect (find-shape LOI1 "circle") [list (build-image SI3)])
(check-expect (find-shape LOI1 "triangle") [list (build-image SI1)])
(check-expect (find-shape LOI1 "square") [list (build-image SI2)])



(define (find-shape losi shape)
  (cond [(empty? losi) empty]
        [else
         (if (string=? (simple-img-shape (first losi)) shape)
             (cons (build-image (first losi)) (find-shape (rest losi) shape))
             (find-shape (rest losi) shape))]))












 

(define LOD1 [list Step1 Step2 Step3 Step4])







(check-expect (updates empty "abc") "abc")
(check-expect (updates LOD1 "How are you today?") "How were you yesterday?")
(check-expect (updates LOD1 "") "")



(define (updates lod str)
  (cond [(empty? lod) str]
        [(string=? "" str) str]
        [else
         (updates (rest lod) (update str (first lod)))]))













