

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment 1 - James Schibley & Jay Hokkanen|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))








(define (backwards string) "")

(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "") "")
(check-expect (backwards "a") "a")
(check-expect (backwards "   test") "tset   ")








(define-struct customer (first last age zip))
 





(check-expect (birthday (make-customer "John" "Smith" 20 11111))
              (make-customer "John" "Smith" 21 11111))

(check-expect (birthday (make-customer "George" "Dylan" 0 22222))
              (make-customer "George" "Dylan" 1 22222))

(define (birthday acustomer)
  (make-customer
   (customer-first acustomer)
   (customer-last acustomer)
   (+ 1 (customer-age acustomer))
   (customer-zip acustomer)
   )
  )




 
 
(check-expect (name-change
               (make-customer "John" "Smith" 21 11111)
               ""
               "")
              (make-customer "" "" 21 11111))

(check-expect (name-change
               (make-customer "George" "Dylan" 25 22222)
               "New"
               "Name")
              (make-customer "New" "Name" 25 22222))

(define (name-change acustomer newfirst newlast)
  (make-customer
   newfirst
   newlast
   (customer-age acustomer)
   (customer-zip acustomer)
   )
  )





(require 2htdp/image)

















(define-struct simple-img (shape color area))

(define SI (make-simple-img 2 "red" 250))
(define SI2 (make-simple-img 0 "purple" 200))

 
       





(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI SI) false)


(define (bigger? img1 img2)
  (> (simple-img-area img1) (simple-img-area img2))
  )





(define (build-image img)
  (cond
    [(= (simple-img-shape img) 0)
     (square (sqrt (simple-img-area img)) "solid" (simple-img-color img)
             )]
    [(= (simple-img-shape img) 1)
     (circle (sqrt (/ (simple-img-area img) pi)) "solid" (simple-img-color img)
             )]
    [else
     (triangle (sqrt (/ (* 4 (simple-img-area img)) (sqrt 3))) "solid" (simple-img-color img)
               )]
    )
  )





(check-expect (build-image SI) .)
(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))
(check-expect (build-image (make-simple-img 0 "blue" 0)) (square 0 "solid" "blue"))
(check-expect (build-image (make-simple-img 1 "green" 50)) (circle (sqrt (/ 50 pi)) "solid" "green"))

















(define-struct diff (position add-text delete-number))
















(define (update text diff)
  (if
   (= (diff-delete-number diff) 0)
   (string-append (string-append (substring text 0 (diff-position diff)) (diff-add-text diff)) (substring text (diff-position diff))) 
   (string-append (substring text 0 (diff-position diff)) (substring text (+ (diff-position diff) (diff-delete-number diff))))
   )
  )

(check-expect (update "test" (make-diff 2 "hello" 0)) "tehellost")
(check-expect (update "" (make-diff 0 "a" 0)) "a")
(check-expect (update "longstring" (make-diff 4 "" 3)) "longing")
(check-expect (update "qwerty" (make-diff 0 "" 6)) "")










(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))









 
                   




(define (total-width loi)
  (cond
    [(empty? loi) 0]
    [else
     (+ (image-width (first loi))
        (total-width (rest loi)))]
    )
  )


(check-expect (total-width LOI) 265)
(check-expect (total-width empty) 0)
(check-expect (total-width (list I2)) (image-width I2))






(define (taller-than loi num)
  (cond
    [(empty? loi) empty]
    [else
     (if (> (image-height (first loi)) num)
         (cons (first loi) (taller-than (rest loi) num))
         (taller-than (rest loi) num))]
    )
  )

(check-expect (taller-than LOI 30)
              (list . .))

(check-expect (taller-than empty 20) empty)
(check-expect (taller-than (list I3) 20) empty)
(check-expect (taller-than (list I3) 19) (list I3))








(define SI1 (make-simple-img 1 "red" 120))
(define SI3 (make-simple-img 1 "purple" 40))
(define SI4 (make-simple-img 2 "blue" 30))





 




(define (find-shape losi shape)
  (cond
    [(empty? losi) empty]
    [(and (string=? shape "square") (= 0 (simple-img-shape (first losi))))
     (cons (build-image (first losi)) (find-shape (rest losi) shape))]
    [(and (string=? shape "circle") (= 1 (simple-img-shape (first losi))))
     (cons (build-image (first losi)) (find-shape (rest losi) shape))]
    [(and (string=? shape "triangle") (= 2 (simple-img-shape (first losi))))
     (cons (build-image (first losi)) (find-shape (rest losi) shape))]
    [else (find-shape (rest losi) shape)]
    )
  )
      

(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))

(check-expect (find-shape empty "circle")
              empty)

(check-expect (find-shape (list SI SI1 SI2 SI3 SI4) "square")
              (list (build-image SI2)))

(check-expect (find-shape (list SI SI1 SI2 SI3 SI4) "triangle")
              (list (build-image SI) (build-image SI4)))











 



(define LOD1 (list (make-diff 2 "test" 0) (make-diff 3 "" 1)))
(define LOD2 (list (make-diff 2 "test" 0) (make-diff 3 "" 1) (make-diff 5 "" 5) (make-diff 0 "aaaaaaaaaaaaaa" 0)))


(check-expect (updates empty "") "")
(check-expect (updates empty "test") "test")
(check-expect (updates LOD1 "initial string") "intstitial string")
(check-expect (updates LOD2 "qwertyuiop") "aaaaaaaaaaaaaaqwtstiop")

(define (updates lod text)
    (cond
      [(empty? lod) text]
      [else
       (updates (rest lod) (update text (first lod)))]
      )
    )