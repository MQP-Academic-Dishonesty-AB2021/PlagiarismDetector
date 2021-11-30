

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment #1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)








(check-expect (backwards "") "")    



(define (backwards stl1) "")    











(define-struct customer (first last age zip))

(define c1 (make-customer "John" "Doe" 18 03456))

 





(check-expect (birthday (make-customer "john" "doe" 25 04959)) (make-customer "john" "doe" 26 04959))
(check-expect (birthday (make-customer "john" "doe" 0 04959)) (make-customer "john" "doe" 1 04959))
(check-expect (birthday (make-customer "john" "doe" -999 04959)) (make-customer "john" "doe" -998 04959))



(define (birthday c1) (make-customer (customer-first c1) (customer-last c1) (+ (customer-age c1) 1) (customer-zip c1)))





(check-expect (name-change (make-customer "john" "doe" 25 04959) "Improved" "John") (make-customer "Improved" "John" 25 04959))
(check-expect (name-change (make-customer "john" "doe" 25 04959) "" "") (make-customer "" "" 25 04959))
(check-expect (name-change (make-customer "john" "doe" 25 04959) "a" "b") (make-customer "a" "b" 25 04959))
(check-expect (name-change (make-customer "john" "doe" 25 04959) "10" "2") (make-customer "10" "2" 25 04959))



(define (name-change c1 str1 str2)
  (make-customer str1 str2 (customer-age c1) (customer-zip c1)))













(define-struct simple-img (shape color area))

(define img1 (make-simple-img "square" "blue" 100))
(define img2 (make-simple-img "triangle" "red" 50))
(define img3 (make-simple-img "circle" "green" 75))

 




(check-expect (bigger? (make-simple-img "square" "blue" 100) (make-simple-img "triangle" "red" 50)) true)
(check-expect (bigger? (make-simple-img "square" "blue" 50) (make-simple-img "triangle" "red" 100)) false)
(check-expect (bigger? (make-simple-img "square" "blue" 100) (make-simple-img "triangle" "red" 100)) false)



(define (bigger? img1 img2)
  (> (simple-img-area img1) (simple-img-area img2)))




(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI3 (make-simple-img "circle" "blue" 150))




(define (find-radius area)
  (sqrt (/ area pi))
  )

(check-within (find-radius 1) 3.14159 pi)
(check-expect (find-radius 0) 0)




(define (find-side area)
  (sqrt (/ (* 2 area) (sin (/ pi 3)))))

(check-within (find-side 250) 24.02811 24.02812)
(check-expect (find-side 0) 0)
(check-expect (build-image SI) .)
(check-within (find-side 250) 24.02811 24.02812)
(check-expect (find-side 0) 0)

(check-expect (build-image SI3) (circle (find-radius 150) "solid" "blue"))
(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))
(check-expect (build-image (make-simple-img "oval" "blue" 150)) "?")




(define (build-image img1)
  (cond [(string=? "square" (simple-img-shape img1))
           (square (sqrt (simple-img-area img1)) "solid" (simple-img-color img1))]
        [(string=? "triangle" (simple-img-shape img1))
           (triangle (find-side (simple-img-area img1)) "solid" (simple-img-color img1))]
        [(string=? "circle" (simple-img-shape img1))
           (circle (find-radius (simple-img-area img1)) "solid" (simple-img-color img1))]
        [else "?"]))





(define-struct diff (del? item1 item2))
                




 




(check-expect (update "0123456789" (make-diff false 1 "xyzzy")) "0xyzzy123456789")
(check-expect (update "0123456789" (make-diff true 3 2)) "01256789")
(check-expect (update "Bananaman" (make-diff true 0 6)) "man")
(check-expect (update "ZacharyRioux" (make-diff false 7 " ")) "Zachary Rioux")
(check-expect (update "" (make-diff true 0 0)) "")



(define (update str1 Adiff)
  (if (diff-del? Adiff)
      (string-append (substring str1 0 (diff-item1 Adiff)) (substring str1 (+ (diff-item1 Adiff) (diff-item2 Adiff))))
      (string-append (substring str1 0 (diff-item1 Adiff)) (diff-item2 Adiff) (substring str1 (diff-item1 Adiff)))))


(define delA (make-diff true 4 3))
(define insA (make-diff false 4 "were"))
(define delB (make-diff true 13 5))
(define insB (make-diff false 13 "yesterday"))
(define string1 "How are you today?")

(update (update (update (update string1 delA) insA) delB) insB)
















  

(define Img1 (square 25 "solid" "blue"))
(define Img2 (circle 10 "solid" "blue"))
(define Img3 (rectangle 15 20 "solid" "blue"))
(define Img4 (triangle 30 "solid" "blue"))

(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))

(define loi1 (list Img1 Img2 Img3 Img4))
(define loi2 (list Img1 Img1 Img1 Img1))




(check-expect (total-width loi1) 90)
(check-expect (total-width loi2) 100)

(check-expect (total-width LOI) 265)

(check-expect (taller-than LOI 30) (list . .))



(define (total-width loi)
  (cond
    [(empty? loi) 0]
    [else
     (+ (image-width (first loi)) (total-width (rest loi)))])) 





(check-expect (taller-than loi1 5) loi1)
(check-expect (taller-than loi1 20) (cons Img1 (cons Img4 empty)))
(check-expect (taller-than loi1 50) empty)



(define (taller-than loi num)
  (cond
    [(empty? loi) empty]
    [(> (image-height (first loi)) num)
     (cons (first loi) (taller-than (rest loi) num))]
    [else
     (taller-than (rest loi) num)]))

















 









(define img4 (make-simple-img "square" "orange" 200))
(define img5 (make-simple-img "triangle" "purple" 60))
(define img6 (make-simple-img "circle" "pink" 95))

(define los-i1 (list img1 img2 img3))
(define los-i2 (list img1 img2 img3 img4 img5 img6))
(define los-i3 (list img1 img4))




(check-expect (find-shape los-i1 "square") (list img1))
(check-expect (find-shape los-i2 "triangle") (list img2 img5))
(check-expect (find-shape los-i3 "circle") empty)

(define (find-shape los-i str)
  (cond
    [(empty? los-i) empty]
    [(string=? str (simple-img-shape (first los-i)))
     (cons (first los-i) (find-shape (rest los-i) str))]
    [else
     (find-shape (rest los-i) str)]))











 










(define lod (list delA insA delB insB))

(check-expect (updates lod "How are you today?") "How were you yesterday?")

(define (updates lod str)
  (cond
    [(not (empty? lod)) (updates (rest lod) (update str (first lod)))]
    [else str]))