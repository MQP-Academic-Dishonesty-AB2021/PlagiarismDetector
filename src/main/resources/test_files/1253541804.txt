

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Assignment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)






(check-expect (backwards "") "")
(check-expect (backwards "a") "a")
(check-expect (backwards "2a") "a2")
(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "a t") "t a")

(define (backwards string) "Hello, World!")




(define-struct customer (first last age zip))








 






(check-expect (birthday (make-customer "Tom" "Holland" 24 01010))
              (make-customer "Tom" "Holland" 25 01010))
(check-expect (birthday (make-customer "Sean" "Lendrum" 0 02090))
              (make-customer "Sean" "Lendrum" 1 02090))
(check-expect (birthday (make-customer "Zilin" "Dai" 60 01890))
              (make-customer "Zilin" "Dai" 61 01890))

(define (birthday cust)
  (make-customer (customer-first cust)
                 (customer-last cust)
                 (+ (customer-age cust) 1)
                 (customer-zip cust)))







(check-expect (name-change
               (make-customer "Tom" "Holland" 24 01010) "Zendaya" "Coleman")
              (make-customer "Zendaya" "Coleman" 24 01010))

(check-expect (name-change
               (make-customer "Sean" "Lendrum" 18 02090) "Jack" "Danger")
              (make-customer "Jack" "Danger" 18 02090))

(check-expect (name-change
               (make-customer "Zilin" "Dai" 60 01890) "Thumb" "Han")
              (make-customer "Thumb" "Han" 60 01890))

(define (name-change cust new-first new-last)
  (make-customer
   new-first
   new-last
   (customer-age cust)
   (customer-zip cust)))



(define-struct simple-img (shape side-length color))







 



(define SI (make-simple-img "triangle" 24.02811 "red")) 
(define SI2 (make-simple-img "square" 14 "purple"))
(define SI4 (make-simple-img "circle" 0 "orange"))












(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI4 SI) false)


(define (bigger? img1 img2)
  (> (img-size img1) (img-size img2)))






(check-within 1 (img-size SI) 250)
(check-within 1 (img-size SI2) 200)
(check-expect (img-size SI4) 0)

(define (img-size img)
  (cond [(string=? (simple-img-shape img) "triangle")
         (* (/ (sqrt 3) 4) (sqr (simple-img-side-length img)))]
        [(string=? (simple-img-shape img) "circle")
         (* pi (sqr (simple-img-side-length img)))]
        [else
         (sqr (simple-img-side-length img))]))

(check-expect (bigger? SI SI2) true)










(check-expect (build-image SI) .)
(check-expect (build-image SI2) (square 14 "solid" "purple"))
(check-expect (build-image SI4) (circle 0 "solid" "orange"))

(define (build-image img)
  (cond [(string=? (simple-img-shape img) "square")
         (square (simple-img-side-length img) "solid" (simple-img-color img))]
        [(string=? (simple-img-shape img) "circle")
         (circle (simple-img-side-length img) "solid" (simple-img-color img))]
        [(string=? (simple-img-shape img) "triangle")
         (triangle (simple-img-side-length img) "solid" (simple-img-color img))]))



(define-struct diff (insertion? position content))

























 



(define D1 (make-diff true 0 "apple"))
(define D2 (make-diff true 5 "bottom"))
(define D3 (make-diff true 11 "jeans"))

(define D4 (make-diff false 0 "5"))
(define D5 (make-diff false 5 "6"))
(define D6 (make-diff false 11 "5"))

(define D7 (make-diff true 3 ""))       
(define D8 (make-diff false 3 "0"))     

(define D9 (make-diff false 10 "8"))
(define D10 (make-diff true 10 "addition"))

(define D15 (make-diff true 0 "hi"))
(define D16 (make-diff false 0 "2"))










(check-expect (update "bottomjeans" D1) "applebottomjeans")
(check-expect (update "applejeans" D2) "applebottomjeans")
(check-expect (update "applebottom" D3) "applebottomjeans")

(check-expect (update "applebottomjeans" D4) "bottomjeans")
(check-expect (update "applebottomjeans" D5) "applejeans")
(check-expect (update "applebottomjeans" D6) "applebottom")

(check-expect (update "edge case" D7) "edge case")
(check-expect (update "another edge case" D8) "another edge case")
(check-expect (update "" D15) "hi")
(check-expect (update "hi" D16) "")

(check-expect (update (update "I made an addendum to my house" D9) D10)
              "I made an addition to my house") 


(define (update text diff)

  (if (diff-insertion? diff)
        
      (string-append 
       (substring text 0 (diff-position diff))
       (diff-content diff)
       (substring text (diff-position diff)))

      (string-append 
       (substring text 0 (diff-position diff))
       (substring text (+ (diff-position diff) (string->number (diff-content diff)))))
      )
  )
  




(define D11 (make-diff false 4 "3"))
(define D12 (make-diff true 4 "were"))
(define D13 (make-diff false 13 "5"))
(define D14 (make-diff true 13 "yesterday"))

(check-expect (update
               (update
                (update
                 (update "How are you today?" D11) D12) D13) D14)
              "How were you yesterday?")








(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define I5 (triangle 0 "solid" "black"))

(define LOI (list I1 I2 I3 I4))









 










(check-expect (total-width LOI)
              (+ (image-width I1) (image-width I2)
                 (image-width I3) (image-width I4)))
(check-expect (total-width (list I1 I2))
              (+ (image-width I1) (image-width I2)))
(check-expect (total-width (list I3 I5))
              (+ (image-width I3) (image-width I5)))
(check-expect (total-width empty) 0)

(define (total-width loi)
  (cond [(empty? loi) 0]
        [else
         (+ (image-width (first loi))
              (total-width (rest loi)))]))








(check-expect (taller-than LOI 30)
              (list . .))
(check-expect (taller-than (list I1 I2) 50)
              (list I1))
(check-expect (taller-than (list I3 I4) 0)
              (list I3 I4))
(check-expect (taller-than empty 0)
              empty)


(define (taller-than loi num)
  (cond [(empty? loi) empty]
        [else
         (if (> (image-height (first loi)) num)
             (cons (first loi) (taller-than (rest loi) num))
              (taller-than (rest loi) num))]))








 






(define SI1 (make-simple-img "circle" 6.18039 "red"))
(define SI3 (make-simple-img "circle" 3.56825 "purple"))









(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
(check-expect (find-shape (list SI) "square")
              empty)
(check-expect (find-shape (list SI SI1 SI2 SI4) "square")
              (list (square 14 "solid" "purple")))
(check-expect (find-shape (list SI SI2 SI3) "triangle")
              (list .))

(define (find-shape losi shp)
  (cond [(empty? losi) empty]
        [else
         (if (string=? (simple-img-shape (first losi))
                       shp)
             (cons (build-image (first losi))
             (find-shape (rest losi) shp))
             (find-shape (rest losi) shp))]))









 



(define D17 (make-diff false 0 "6"))
(define D18 (make-diff true 0 "apple"))
(define D19 (make-diff false 5 "3"))
(define D20 (make-diff true 5 "bottom"))










(check-expect (updates (list D1 D2 D3) "")
              "applebottomjeans")

(check-expect (updates (list D17 D18 D19 D20) "bananatopjeans")
              "applebottomjeans")

(check-expect (updates (list D9 D10) "I made an addendum to my house")
              "I made an addition to my house")

(check-expect (updates (list D11 D12 D13 D14) "How are you today?")
              "How were you yesterday?")

(define (updates LOD String)
  (cond [(empty? LOD) String]
        [else
         (updates (rest LOD) (update String (first LOD)))]))

