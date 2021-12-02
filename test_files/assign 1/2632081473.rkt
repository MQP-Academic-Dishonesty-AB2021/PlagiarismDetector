

#reader(lib "htdp-beginner-reader.ss" "lang")((modname VenatArjunAssignment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)








(define (backwards str) "owiebvrouwerbviwervouweirnveru")                                   


(check-expect (backwards "") "")
(check-expect (backwards "a") "a")
(check-expect (backwards "bb") "bb")
(check-expect (backwards "ccc") "ccc")
(check-expect (backwards "abba") "abba")
(check-expect (backwards "aba") "aba")
(check-expect (backwards "aeiouy") "yuoiea")
(check-expect (backwards "woecinw") "wniceow")












(define-struct customer (fname lname age zip))








 






(define C1 (make-customer "Bob" "Gnarly" 130 12093))
(define C2 (make-customer "Freddie" "Venus" 29 34349))
(define C3 (make-customer "Kanye" "East" 0 30853))
(define C4 (make-customer "Walter" "White" 840 29387))




(check-expect (birthday C1) (make-customer "Bob" "Gnarly" 131 12093))
(check-expect (birthday C2) (make-customer "Freddie" "Venus" (+ 29 1) 34349))
(check-expect (birthday C3) (make-customer "Kanye" "East" (+ 0 1) 30853))
(check-expect (birthday C4) (make-customer "Walter" "White" (+ 840 1) 29387))



(define (birthday custard)
  (make-customer (customer-fname custard) (customer-lname custard) (+ (customer-age custard) 1) (customer-zip custard)
                 ))









(check-expect (name-change C1 "Robert" "Marley") (make-customer "Robert" "Marley" 130 12093))
(check-expect (name-change C2 "" "") (make-customer "" "" 29 34349))
(check-expect (name-change C3 "Kanye" "West") (make-customer "Kanye" "West" 0 30853))
(check-expect (name-change C4 "Flynn" "White") (make-customer "Flynn" "White" 840 29387))



(define (name-change custard first last)
  (make-customer first last (customer-age custard) (customer-zip custard))
)












(define-struct simple-img (shape color area))


 



(define SI (make-simple-img "triangle" "red" 250))
(define SI1 (make-simple-img "circle" "red" 120))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI3 (make-simple-img "circle" "purple" 40))
(define SI5 (make-simple-img "circle" "green" 200))











(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI SI1) true)
(check-expect (bigger? SI3 SI1) (> (simple-img-area SI3) (simple-img-area SI1)))
(check-expect (bigger? SI2 SI5) (> (simple-img-area SI2) (simple-img-area SI5)))


 

(define (bigger? img1 img2)
(> (simple-img-area img1) (simple-img-area img2)))







(check-expect (build-image SI) .)
(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))
(check-expect (build-image SI5) (circle (sqrt (/ 200 pi)) "solid" "green"))


 

(define (build-image img)
     (cond [(string=? (simple-img-shape img) "triangle") (triangle (* (/ 2 3) (expt 3 (/ 3 4)) (sqrt (simple-img-area img))) "solid" (simple-img-color img))] 
           [(string=? (simple-img-shape img) "square")   (square (sqrt (simple-img-area img)) "solid" (simple-img-color img))]
           [(string=? (simple-img-shape img) "circle")   (circle (sqrt (/ (simple-img-area img) pi)) "solid" (simple-img-color img))]
           ))


















(define-struct diff (added part1 part2))


 

(define D1 (make-diff "xyzzy" 1 0))
(define D2 (make-diff "" 3 2))
(define D3 (make-diff "" 3 0))
(define D4 (make-diff "abcdef" 0 0))
(define D5 (make-diff "abcdef" 10 0))









 


(check-expect (update "0123456789" D1) "0xyzzy123456789")
(check-expect (update "0123456789" D2) "01256789")
(check-expect (update "0123456789" D3) (string-append "012" "" "3456789"))
(check-expect (update "0123456789" D4) (string-append "" "abcdef" "0123456789"))
(check-expect (update "0123456789" D5) (string-append "0123456789" "abcdef" ""))

(define (update received dif)
 (string-append (substring received 0 (diff-part1 dif)) (diff-added dif) (substring received (+ (diff-part1 dif) (diff-part2 dif))))
  )




(check-expect S4 "How were you yesterday?")

(define S1 (update "How are you today?" (make-diff "" 4 3)))
(define S2 (update S1 (make-diff "were" 4 0)))
(define S3 (update S2 (make-diff "" 13 5)))
(define S4 (update S3 (make-diff "yesterday" 13 0)))












 



(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define J1 (square 10 "solid" "green"))
(define J2 (rectangle 40 14 "outline" "red"))
(define J3 (triangle 90 "solid" "blue"))
(define J4 (circle 100 "outline" "purple"))


(define LOI (list I1 I2 I3 I4))
(define LOI2 (list J1 J2 J3 J4))





(check-expect (total-width LOI) 265)
(check-expect (total-width LOI2) (+ 10 40 90 (* 100 2))) 


                                                               
 
    
    

(define (total-width loi)
   (cond [(empty? loi) 0]
         [else
          (+ (image-width (first loi)) (total-width (rest loi)))]))










 
(check-expect (taller-than LOI 30)  (list . .))

(check-expect (taller-than LOI 10)  (list .. ..))
(check-expect (taller-than LOI 5)  (list .. ..))
(check-expect (taller-than LOI 20)  (list .. .))


(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [else
         (if (> (image-height (first loi)) cutoff)
             (cons (first loi) (taller-than (rest loi) cutoff))
             (taller-than (rest loi) cutoff))

         ]))












 







 

(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
(check-expect (find-shape (list SI SI1 SI2 SI3) "triangle" )(list (build-image SI)))
(check-expect (find-shape (list SI SI1 SI2 SI3) "square" )(list (build-image SI2)))


(define (find-shape losi shape)
  (cond [(empty? losi) empty]
        [else
         (if (string=? (simple-img-shape (first losi)) shape) 
             (cons (build-image (first losi)) (find-shape (rest losi) shape))
             (find-shape (rest losi) shape)
             
             )]))








  


















(check-expect (updates (list D2 D1) "0123456789" ) "0xyzzy1256789")
(check-expect (updates (list D1 D2) "0123456789" ) "0xyy123456789")
(check-expect (updates (list D1 D2 D3) "0123456789" ) "0xyy123456789")
(check-expect (updates (list D1 D2 D3 D4) "0123456789" ) "abcdef0xyy123456789")
(check-expect (updates (list D1 D2 D3 D4 D5) "0123456789" ) "abcdef0xyyabcdef123456789")



(define (updates lod input)
  (cond [(empty? lod) input]
        [else
            (updates (rest lod) (update input (first lod)))]))
