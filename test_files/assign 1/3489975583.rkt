

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname assignment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)


















(define-struct customer (fname lname age zip))









(check-expect (customer-age (birthday (make-customer "John" "Cena" 25 "01729"))) 26)

(define (birthday acustomer)
  (make-customer (customer-fname acustomer) (customer-lname acustomer) (+ (customer-age acustomer) 1) (customer-zip acustomer))
)









(check-expect (name-change (make-customer "Daniel" "Radcliffe" 15 "45678") "Harry" "Potter") (make-customer "Harry" "Potter" 15 "45678"))
(check-expect (name-change (make-customer "William" "Jefferson" 15 "12345") "Bill" "Clinton") (make-customer "Bill" "Clinton" 15 "12345"))

(define (name-change acustomer new-fname new-lname)
  (make-customer new-fname new-lname (customer-age acustomer) (customer-zip acustomer))
)









(define-struct simple-img (shape color area))



(define SI (make-simple-img "triangle" "red" 250))
(define SI1 (make-simple-img "circle" "red" 120))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI3 (make-simple-img "circle" "purple" 40))




(define (bigger? img1 img2)
  (> (simple-img-area img1) (simple-img-area img2))
)

(check-expect (bigger? SI1 SI3) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI3 SI3) false)




(define (build-image img)
  (cond
    [(equal? (simple-img-shape img) "triangle") (triangle (sqrt (* (simple-img-area img) 4 (/ 1 (sqrt 3)))) "solid" (simple-img-color img))]
    [(equal? (simple-img-shape img) "square") (square (sqrt (simple-img-area img)) "solid" (simple-img-color img))]
    [(equal? (simple-img-shape img) "circle") (circle (sqrt (/ (simple-img-area img) pi)) "solid" (simple-img-color img))]
    [else empty-image]
  )
)

(check-expect (build-image SI) (triangle (sqrt (* 250 4 (/ 1 (sqrt 3)))) "solid" "red"))
(check-expect (build-image SI1) (circle (sqrt (/ 120 pi)) "solid" "red"))
(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))
(check-expect (build-image SI3) (circle (sqrt (/ 40 pi)) "solid" "purple"))
(check-expect (build-image (make-simple-img "circle" "green" 0)) (circle 0 "solid" "green"))










 
(define-struct diff (dtype pos value))




(check-expect (update "this test" (make-diff "INS" 4 " is a")) "this is a test")
(check-expect (update "nothing" (make-diff "INS" 0 "")) "nothing")



(check-expect (update "mother" (make-diff "DEL" 0 1)) "other")
(check-expect (update "no change" (make-diff "DEL" 0 0)) "no change")






(define (update text adiff)
  (cond 
    [(equal? (diff-dtype adiff) "INS") (insert text adiff)]
    [(equal? (diff-dtype adiff) "DEL") (delete text adiff)]
    [else text]
  )
)

(check-expect (insert "0123456789" (make-diff "INS" 5 "text")) "01234text56789")
(check-expect (insert "" (make-diff "INS" 0 "text")) "text")
(check-expect (insert "0123456789" (make-diff "INS" 5 "")) "0123456789")




(define (insert text adiff)
  (string-append (substring text 0 (diff-pos adiff)) (diff-value adiff) (substring text (diff-pos adiff)))
)

(check-expect (delete "0123456789" (make-diff "DEL" 5 4)) "012349")
(check-expect (delete "text" (make-diff "DEL" 2 0)) "text")




(define (delete text adiff)
  (string-append (substring text 0 (diff-pos adiff))
                 (substring text (+ (diff-pos adiff) (diff-value adiff)))))

  
  
  

  
  
  
  
  
  
  

  
  
  
  

  (check-expect (update (update (update (update "How are you today?" (make-diff "DEL" 4 1)) (make-diff "DEL" 11 2)) (make-diff "INS" 4 "we")) (make-diff "INS" 13 "yester")) "How were you yesterday?")






(define I1 (circle 100 "solid" "red"))
(define I2 (square 25 "outline" "blue"))
(define I3 (ellipse 80 20 "solid" "purple"))
(define I4 (circle 60 "solid" "green"))

(define LOI (list I1 I2 I3 I4))
  











(define (total-width loi)
  (cond [(empty? loi) 0]
        [else 
          (+ (image-width (first loi)) (total-width (rest loi)))]))

(check-expect (total-width LOI) 425)
(check-expect (total-width (list I1 I2)) 225)
(check-expect (total-width empty) 0)



(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) cutoff) (cons (first loi) (taller-than (rest loi) cutoff))]
        [else (taller-than (rest loi) cutoff)]))

(check-expect (taller-than LOI 30) (list I1 I4))
(check-expect (taller-than LOI 0) (list I1 I2 I3 I4))
(check-expect (taller-than LOI 500) empty)












(define (find-shape losi image-type)
  (cond [(empty? losi) empty]
        [(equal? (simple-img-shape (first losi)) image-type) (cons (first losi) (find-shape (rest losi) image-type))]
        [else (find-shape (rest losi) image-type)]
  )
)

(check-expect (find-shape (list SI SI1 SI2 SI3) "") empty)
(check-expect (find-shape (list SI SI1 SI2 SI3) "dodecahedron") empty)
(check-expect (find-shape (list SI SI1 SI2 SI3) "circle") (list SI1 SI3))
(check-expect (find-shape (list SI SI1 SI2 SI3) "triangle") (list SI))
(check-expect (find-shape (list SI SI1 SI2 SI3) "square") (list SI2))
















(check-expect (updates "How are you today?" 
(list (make-diff "DEL" 4 1) (make-diff "DEL" 11 2) (make-diff "INS" 4 "we") (make-diff "INS" 13 "yester")))
"How were you yesterday?")
(check-expect (updates "I can't wait till I'm done with Racket" empty)
"I can't wait till I'm done with Racket")

(define (updates text lod)
  (cond [(empty? lod) text]
        [else (updates (update text (first lod)) (rest lod))]
))