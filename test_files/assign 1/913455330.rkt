

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






(define (backwards string) "")

(check-expect (backwards "") "")













(define-struct customer (firstName lastName zipCode age))

 





(define (birthday oldCustomer)
  (make-customer
   (customer-firstName oldCustomer)
   (customer-lastName oldCustomer)
   (customer-zipCode oldCustomer)
   (+ (customer-age oldCustomer) 1)))

(check-expect (birthday (make-customer "John" "Smith" "11111" 37))
              (make-customer "John" "Smith" "11111" 38))

(check-expect (birthday (make-customer "Jane" "Doe" "22222" 0))
              (make-customer "Jane" "Doe" "22222" 1))






(define (name-change newFirstName newLastName c)
  (make-customer newFirstName
                 newLastName
                 (customer-zipCode c)
                 (customer-age c)))

(check-expect (name-change "Joseph" "Beck" (make-customer "John" "Doe" "00000" 45))
              (make-customer "Joseph" "Beck" "00000" 45))

(check-expect (name-change "" "" (make-customer "Jane" "Doe" "11111" 54))
              (make-customer "" "" "11111" 54))




(require 2htdp/image)




(define-struct simple-img (color area type))

 


(define SI (make-simple-img "red" 250 "triangle")) 
(define SI2 (make-simple-img "purple" 200 "square")) 






(define (bigger? img1 img2)
  (if (> (simple-img-area img1) (simple-img-area img2))
      #t
      #f))

(check-expect (bigger? (make-simple-img "red" 50 "square") (make-simple-img "blue" 30 "triangle")) #t)
(check-expect (bigger? (make-simple-img "green" 40 "circle") (make-simple-img "red" 50 "square")) #f)

(check-expect (bigger? SI SI2) true) 





(define (build-image img)
  (cond [(equal? (simple-img-type img) "square")
         (square (sqrt (simple-img-area img)) "solid" (simple-img-color img))]
        [(equal? (simple-img-type img) "triangle")
         (triangle (sqrt (/ (* 4 (simple-img-area img)) (sqrt 3))) "solid" (simple-img-color img))]
        [(equal? (simple-img-type img) "circle")
         (circle (sqrt (/ (simple-img-area img) pi)) "solid" (simple-img-color img))]
        )
  )

(check-expect (build-image (make-simple-img "red" 81 "square")) (square 9 "solid" "red"))
(check-expect (build-image (make-simple-img "blue" 81 "triangle")) (triangle 13.67704234 "solid" "blue"))
(check-expect (build-image (make-simple-img "yellow" 10 "circle")) (circle 1.78412411615 "solid" "yellow"))
(check-expect (build-image SI) .) 






(define-struct diff (pos del-chars insert))






(define (fn-for-diff d)
  (... (diff-pos d)        
       (diff-del-chars d)  
       (diff-insert d)))   





(define (del text pos num-chars)
  (string-append
   (substring text 0 pos)
   (substring text (+ pos num-chars) (string-length text))))





(define (insert text pos str)
  (string-append
   (substring text 0 pos)
   str
   (substring text pos (string-length text))))



(define (update text diff)
  (insert (del text
               (diff-pos diff)
               (diff-del-chars diff))
          (diff-pos diff)
          (diff-insert diff)))




(define DEL (make-diff 3 2 ""))
(check-expect (update "0123456789" DEL)
              "01256789")
(check-expect (update "abcd" (make-diff 1 1 ""))
              "acd")
(check-expect (update "abcd" (make-diff 0 4 ""))
              "")
(check-expect (update "abcd" (make-diff 1 0 ""))
              "abcd")



(define INS (make-diff 1 0 "xyzzy"))
(check-expect (update "0123456789" INS)
              "0xyzzy123456789")
(check-expect (update "abcd" (make-diff 0 0 "x"))
              "xabcd")
(check-expect (update "abcd" (make-diff 4 0 "xyz"))
              "abcdxyz")
(check-expect (update "abcd" (make-diff 1 0 ""))
              "abcd")


(define d1 (make-diff 4 3 ""))
(define d2 (make-diff 4 0 "were"))
(define d3 (make-diff 13 2 ""))
(define d4 (make-diff 13 0 "yester"))
(define message "How are you today?")
(define target-message "How were you yesterday?")
(check-expect (update(update(update(update message d1) d2) d3) d4)
              target-message)










(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]                   
        [else (... (first loi)                
                   (fn-for-loi (rest loi)))]))




(define (total-width loi)
  (cond [(empty? loi) 0]                   
        [else (+ (image-width (first loi))                
                   (total-width (rest loi)))]))

(define IM1 (circle 100 "solid" "green"))
(define IM2 empty-image)
(define IM3 (square 15 "outline" "blue"))
(define IM4 (rectangle 100 200 "solid" "red"))

(check-expect (total-width (list IM1)) (image-width IM1))
(check-expect (total-width (list IM1 IM2)) (+ (image-width IM1) (image-width IM2)))
(check-expect (total-width empty) 0)
(check-expect (total-width (list IM2)) 0)




(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) cutoff)
         (cons (first loi) (taller-than (rest loi) cutoff))]
        [else (taller-than (rest loi) cutoff)]))

(check-expect (taller-than (list IM1 IM2 IM3 IM4) 0) (list IM1 IM3 IM4))
(check-expect (taller-than (list IM1 IM2 IM3 IM4) 150) (list IM1 IM4))
(check-expect (taller-than (list IM1 IM2 IM3 IM4) 1000) empty)
(check-expect (taller-than empty 100) empty)






(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define SI1 (make-simple-img "red" 120 "circle"))
(define SI3 (make-simple-img "purple" 40 "circle"))



(define LOI (list I1 I2 I3 I4))

 








(define (find-shape l shape)
  (cond [(empty? l) empty]
        [(equal? (simple-img-type (first l)) shape)
         (cons (build-image (first l)) (find-shape (rest l) shape))]
        [else
         (find-shape (rest l) shape)]
        )
  )

(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
               (list . .))










(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]                   
        [else (... (first lod)                
                   (fn-for-lod (rest lod)))]))






(define (updates lod text)
  (cond [(empty? lod) text]                   
        [else (updates
               (rest lod)
               (update text (first lod)))]))


(check-expect (updates (list d1 d2 d3 d4) message)
              target-message)
(check-expect (updates empty "abcd")
              "abcd")
(check-expect (updates (list DEL INS) "0123456789") 
              "0xyzzy1256789")
