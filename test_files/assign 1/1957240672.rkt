

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |assigment 1 done|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))











(require 2htdp/image)








(check-expect (backwards "abc") "cba")
(check-expect (backwards "") "")
(check-expect (backwards "a12") "21a")
(check-expect (backwards " bc d ") " d cb ")


(define (backwards str) "BACKWARDS FUNCTION TEST MEANT TO FAIL")







(define-struct customer( first last age zip))








(define (fn-for-customer cstmr)
  (... (customer-first cstmr)     
       (customer-last cstmr) 
       (customer-age cstmr) 
       (customer-zip cstmr)))   







(define customer1 (make-customer "a" "x" 50 "01609"))
(define customer2 (make-customer "b" "y" 1 "01609"))
(define customer3 (make-customer "c" "z" 0 "01609"))

(check-expect (birthday customer1) (make-customer "a" "x" (+ 1 (customer-age customer1)) "01609"))
(check-expect (birthday customer2) (make-customer "b" "y" (+ 1 (customer-age customer2)) "01609"))
(check-expect (birthday customer3) (make-customer "c" "z" (+ 1 (customer-age customer3)) "01609"))



(define (birthday cstmr)
  (make-customer(customer-first cstmr) 
       (customer-last cstmr)           
       (+ 1 (customer-age cstmr))      
       (customer-zip cstmr)))          







(check-expect (name-change customer1 "d" "e") (make-customer "d" "e" 50 "01609"))
(check-expect (name-change customer2 "" "") (make-customer "" "" 1 "01609"))

(define (name-change cstmr first last)
  (make-customer first last
                 (customer-age cstmr)
                 (customer-zip cstmr)))








(define-struct simple-img (shape color area))



(define (fn-for-simple-img SI)
  (... (simple-img-shape SI) 
       (simple-img-color SI) 
       (simple-img-area SI)))


(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI3 (make-simple-img "circle" "blue" 600))








 


(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI SI) false)

(define (bigger? img1 img2)
  (> (simple-img-area img1) (simple-img-area img2)))









(check-expect (build-image SI) .)
(check-expect (build-image SI2) .)
(check-expect (build-image SI3) .)



(define (build-image SI)
  (cond
    [(string=? (simple-img-shape SI) "circle")
     (circle (sqrt (/ (simple-img-area SI) pi)) "solid" (simple-img-color SI))]
    
    [(string=? (simple-img-shape SI) "triangle")
     (triangle (tri-side (simple-img-area SI)) "solid" (simple-img-color SI))]
    
    [(string=? (simple-img-shape SI) "square")
     (square (sqrt (simple-img-area SI)) "solid" (simple-img-color SI))]
    [else ...]))



(define (tri-side area)
  (sqrt (* (/ area (sqrt 3)) 4)))

(check-within (tri-side (/ (sqrt 3) 4)) 1 0.1)




(define-struct diff(insert? pos text del))







     








(check-expect (update "0123456789" (make-diff true 1 "xyzzy" 0)) "0xyzzy123456789")
(check-expect (update "0123456789" (make-diff false 3 "" 2)) "01256789")
(check-expect (update "0123456789" (make-diff true 9 " abc" 0)) "012345678 abc9")
(check-expect (update "0123456789" (make-diff false 0 "" 10)) "")
(check-expect (update "0123456789" (make-diff false 4 "" 0)) "0123456789")


(define (update str d)
  (if (diff-insert? d)
      (string-append 
       (substring str 0 (diff-pos d))
       (diff-text d)
       (substring str (diff-pos d)))

      (string-append 
       (substring str 0 (diff-pos d))
       (substring str (+ (diff-pos d) (diff-del d)))) 
  ))









(define diff1 (make-diff false 4 "" 1))
(define diff2 (make-diff false 11 "" 2))
(define diff3 (make-diff true 4 "we" 0))
(define diff4 (make-diff true 13 "yester" 0))

(check-expect (update (update (update (update "How are you today?" diff1) diff2) diff3) diff4) "How were you yesterday?")









(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))












  




  

(define (total-width loi)
  (cond [(empty? loi) 0]                     
        [else (+ (image-width (first loi))  
                 (total-width (rest loi)))]))



(check-expect (total-width LOI) 265)
(check-expect (total-width empty) 0)
(check-expect (total-width (list I1)) 100)





  

(define (taller-than loi cut)
  (cond
    [(empty? loi) empty]
    [else
     (if (> (image-height (first loi)) cut)
         (cons (first loi) (taller-than (rest loi) cut))
         (taller-than (rest loi) cut))]))

(check-expect (taller-than LOI 30)
              (list . .))

(check-expect (taller-than LOI 20)
              (list . . . ))

(check-expect (taller-than LOI 200)
              empty)

(check-expect (taller-than empty 0)
              empty)







(define SI1 (make-simple-img "circle" "red" 120))
(define SI4 (make-simple-img "circle" "purple" 40))









  




  

(define (find-shape losi shape)
  (cond
    [(empty? losi) empty]                                     
        [else
         (if (string=? (simple-img-shape (first losi)) shape) 
             (cons
              (build-image (first losi))                      
              (find-shape (rest losi) shape))
             (find-shape (rest losi) shape))]))               


    

(check-expect (find-shape (list SI SI1 SI2 SI4) "circle")
              (list . .))
(check-expect (find-shape (list SI SI3) "triangle")
              (list .))
(check-expect (find-shape (list SI SI3) "square")
              empty)

(check-expect (find-shape empty "square")
              empty)










  







(define listDiffs (list diff1 diff2 diff3 diff4)) 

(check-expect (updates "How are you today?" listDiffs) "How were you yesterday?")
(check-expect (updates "" empty) "")

(define DEL (make-diff false 3 "" 2))
(define INS (make-diff true 1 "xyzzy" 0))
(check-expect (updates "0123456789" (list DEL INS)) "0xyzzy1256789")


(define (updates str lod)
  (cond [(empty? lod) str]                      
        [else
         (updates (update str (first lod)) (rest lod))])) 
