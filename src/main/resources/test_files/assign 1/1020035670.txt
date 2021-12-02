

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assignment1-9-4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)





(define (backwards str) "") 
(check-expect (backwards "") "")
(check-expect (backwards "a") "a")
(check-expect (backwards "amanaplanacanalpanama") "amanaplanacanalpanama")
(check-expect (backwards "abcdef") "fedcba")




(define-struct customer (first-name last-name age zip-code))







(define CustomerBob (make-customer "Bob" "Bobbertson" 2 "22989"))

(define (fn-for-customer acustomer)
  (... (customer-first-name acustomer)
       (customer-last-name acustomer)
       (customer-age acustomer)
       (customer-zip-code acustomer)
       )
  )





(check-expect (birthday (make-customer "Doesn't" "Matter" 0 "23421")) (make-customer "Doesn't" "Matter" (+ 0 1) "23421"))
(check-expect (birthday (make-customer "Milo" "Jacobs" 18 "22989")) (make-customer "Milo" "Jacobs" (+ 18 1) "22989"))

(define (birthday acustomer)
  (make-customer (customer-first-name acustomer)
                 (customer-last-name acustomer)
                 (+ (customer-age acustomer) 1)
                 (customer-zip-code acustomer)
                 )
  )





(check-expect (name-change (make-customer "" "" 0 "") "John" "Doe") (make-customer "John" "Doe" 0 ""))
(check-expect (name-change (make-customer "Jane" "Doe" 56 "77777") "Scrooge" "Potter") (make-customer "Scrooge" "Potter" 56 "77777"))

(define (name-change acustomer fname lname)
  (make-customer fname lname (customer-age acustomer) (customer-zip-code acustomer))
)








(define-struct simple-img (shape color area))


(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI4 (make-simple-img "circle" "green" 100))

 











(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI SI) false)


(define (bigger? img1 img2)
  (> (simple-img-area img1) (simple-img-area img2)))







(check-expect (build-image SI) .)
(check-expect (build-image (make-simple-img "square" "purple" 200)) (square (sqrt 200) "solid" "purple"))
(check-expect (build-image (make-simple-img "circle" "green" 100)) (circle (sqrt (/ 100 pi)) "solid" "green"))



(define (build-image si)
  (cond
    [(string=? (simple-img-shape si) "triangle")
     (triangle (side-length-triangle (simple-img-area si)) "solid" (simple-img-color si))]
    [(string=? (simple-img-shape si) "square")
     (square (side-length-square (simple-img-area si)) "solid" (simple-img-color si))]
    [(string=? (simple-img-shape si) "circle")
     (circle (radius-circle (simple-img-area si)) "solid" (simple-img-color si))]))






(check-expect (side-length-square 0) 0)
(check-within 0.0001  (side-length-square 100) (sqrt 100))



(define (side-length-square area)
  (sqrt area))






(check-expect (side-length-triangle 0) 0)
(check-within 0.0001 (side-length-triangle 100) (sqrt (/ (* 4 100) (sqrt 3))))



(define (side-length-triangle area)
  (sqrt (/ (* 4 area) (sqrt 3))))






(check-expect (radius-circle 0) 0)
(check-within 0.0001  (radius-circle 100) (sqrt (/ 100 pi)))




(define (radius-circle area)
  (sqrt (/ area pi)))




(define-struct insert (str index))




(define INS (make-insert "xyzzy" 1))
(define INS1 (make-insert "" 0))

 



(define-struct delete (length index))




(define DEL (make-delete 2 3))
(define DEL1 (make-delete 0 0))

 







 













(check-expect (update "0123456789" (make-insert "xyzzy" 1))
              (string-append (substring "0123456789" 0 1) "xyzzy" (substring "0123456789" 1)))
(check-expect (update "0123456789" (make-delete 2 3))
              (string-append (substring "0123456789" 0 3) (substring "0123456789" (+ 3 2))))
(check-expect (update "" (make-insert "hi" 0)) "hi")





(define (update str diff)
  (cond [(insert? diff)
         (string-append (substring str 0 (insert-index diff))
                        (insert-str diff) (substring str (insert-index diff)))]
        [(delete? diff)
         (string-append (substring str 0 (delete-index diff))
                        (substring str (+ (delete-index diff) (delete-length diff))))])
)




(define STR1 "How are you today?")
(define STR2 (update STR1 (make-delete 1 4)))
(define STR3 (update STR2 (make-insert "we" 4)))
(define STR4 (update STR3 (make-delete 2 13)))
(define STR5 (update STR4 (make-insert "yester" 13)))

STR5





(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)
  





  
  

(define LOI (list I1 I2 I3 I4))
(define LOI2 empty)

 











(check-expect (total-width LOI2) 0)
(check-expect (total-width LOI) 265)

(define (total-width loi)
  (cond [(empty? loi) 0]
        [else (+ (image-width (first loi)) (total-width (rest loi)))])
)







(check-expect (taller-than LOI 30)
              (list . .))
(check-expect (taller-than LOI2 453.24) empty)
(check-expect (taller-than LOI 1) LOI)
(check-expect (taller-than LOI 5000) empty)

(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) cutoff)
            (cons (first loi) (taller-than (rest loi) cutoff))]
        [else
            (taller-than (rest loi) cutoff)])
)










(define SI1 (make-simple-img "circle" "red" 120))
(define SI3 (make-simple-img "circle" "purple" 40))





(define LOSI empty)
(define LOSI2 (list SI1 SI2 SI3))

 







(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
(check-expect (find-shape empty "square") empty)
(check-expect (find-shape (list SI2 SI3) "triangle") empty)
(check-expect (find-shape (list SI3 SI) "triangle") (cons (build-image SI) empty))
(check-expect (find-shape (list SI2 SI3) "square") (cons (build-image SI2) empty))

(define (find-shape losi shape)
  (cond [(empty? losi) empty]
        [(match-shape? (first losi) shape)
         (cons (build-image (first losi)) (find-shape (rest losi) shape))]
        [else
         (find-shape (rest losi) shape)])
)







(check-expect (match-shape? SI "triangle") true)
(check-expect (match-shape? SI1 "square") false)

(define (match-shape? si shape)
  (string=? (simple-img-shape si) shape)
)














 

(define LOD (list DEL INS))
(define LOD1 (list INS DEL))
(define LOD2 empty)







(check-expect (updates (list DEL INS) "0123456789") "0xyzzy1256789")
(check-expect (updates (list INS DEL) "0123456789") "0xyy123456789")
(check-expect (updates empty "hi") "hi")
(check-expect (updates (list (make-insert "h" 0)) "") "h")

(define (updates lod str)
  (cond [(empty? lod) str]
        [else 
             (updates (rest lod) (update str (first lod)))])
)