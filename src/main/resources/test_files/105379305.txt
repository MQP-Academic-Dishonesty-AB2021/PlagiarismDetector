

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Lab1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)






(check-expect (backwards "") "")
(check-expect (backwards "Hey") "yeH")
(check-expect (backwards "a") "a")

(define (backwards str) "") 









(define-struct customer (fname lname age zip))






(define C1 (make-customer "Joseph" "Mazzei" 18 "01605"))
(define C2 (make-customer "Kaz" "Erdos" 17 "01754"))

 






(check-expect (birthday C1) (make-customer "Joseph" "Mazzei" (+ 18 1) "01605"))
(check-expect (birthday C2) (make-customer "Kaz" "Erdos" (+ 17 1) "01754"))





(define (birthday acustomer)
  (make-customer (customer-fname acustomer)
                 (customer-lname acustomer)
                 (+ 1 (customer-age acustomer))
                 (customer-zip acustomer)))




(check-expect (name-change C1 "" "Dalbeck") (make-customer "" "Dalbeck" (customer-age C1) (customer-zip C1)))
(check-expect (name-change C2 "Bobby" "Jones") (make-customer "Bobby" "Jones" (customer-age C2) (customer-zip C2)))





(define (name-change acustomer new-fname new-lname)
  (make-customer new-fname
                 new-lname
                 (customer-age acustomer) 
                 (customer-zip acustomer)))






(define-struct simple-img (type color area))






(define SI (make-simple-img "triangle" "red" 250))
(define SI1 (make-simple-img  "circle" "red" 120))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI3 (make-simple-img "circle" "purple" 40))
(define SI4 (make-simple-img "square" "white" 0))
(define SI5 (make-simple-img "triangle" "white" 0))

 






(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI3 SI4) true)
(check-expect (bigger? SI2 SI) false)





(define (bigger? si1 si2)
  (>
   (simple-img-area si1)
   (simple-img-area si2)))




(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))
(check-expect (build-image SI3) (circle  (sqrt (/ 40 pi)) "solid" "purple"))
(check-expect (build-image SI4) (square  (sqrt 0) "solid" "white"))
(check-expect (build-image SI) (triangle  (sqrt (* 4 (/ 250 (sqrt 3)))) "solid" "red"))





(define (build-image asimple-img)
  (cond[(string=? (simple-img-type asimple-img) "square")
        (square (find-length asimple-img "square")
                "solid"
                (simple-img-color asimple-img))]
       [(string=? (simple-img-type asimple-img) "triangle")
        (triangle (find-length asimple-img "triangle")
                  "solid"
                  (simple-img-color asimple-img))]
       [else
        (circle (find-length asimple-img "circle")
                "solid"
                (simple-img-color asimple-img))]))




(check-within (find-length SI "triangle") (sqrt (* 4 (/ 250 (sqrt 3)))) 0.1)
(check-expect (find-length SI5 "triangle") 0)
(check-within (find-length SI3 "circle") (sqrt (/ 40 pi)) 0.1)
(check-within (find-length SI2 "square") (sqrt 200) 0.1)



(define (find-length asimple-img type)
  (cond
    [(= (simple-img-area asimple-img) 0)
     0]
    [(string=? type "square")
     (sqrt (simple-img-area asimple-img))]
    [(string=? type "triangle")
     (sqrt (* 4 (/ (simple-img-area asimple-img) (sqrt 3))))]
    [else
     (sqrt (/ (simple-img-area asimple-img) pi))]))






(define-struct diff (insert? pos to-insert num-chars))






(define D1 (make-diff true 1 "hey" 0))
(define D2 (make-diff false 5 "" 3))
(define D3 (make-diff false 0 "" 2))
(define D4 (make-diff true 7 " boy" 0))

 






(check-expect (update "hello" D1) (string-append (substring "hello" 0 1) (diff-to-insert D1) (substring "hello" 1)))
(check-expect (update "balloon" D4) (string-append "balloon"  (diff-to-insert D4)))
(check-expect (update "hi there my friend!" D2)
              (string-append (substring "hi there my friend!" 0 (diff-pos D2))
                             (substring "hi there my friend!" (+ (diff-num-chars D2) (diff-pos D2)))))
(check-expect (update "joe" D2) "joe")
(check-expect (update "a" D3) "")





(define (update str adiff)
  (cond
    [(diff-insert? adiff)
     (if (pos-valid? str (diff-pos adiff))
         (insert str adiff)
         (string-append str (diff-to-insert adiff)))
     ]
    [(out-of-bounds? str adiff)
     (substring str 0 (diff-pos adiff))]
    [(pos-valid? str (diff-pos adiff))
     (delete str adiff)]
    [else
     str]))




(check-expect (pos-valid? "" 0) false)
(check-expect (pos-valid? "cat" 1) true)
(check-expect (pos-valid? "dog" 3) false)



(define (pos-valid? str pos)
  (< pos (string-length str)))





(check-expect (update "ghetti" D1) (string-append (substring "ghetti" 0 1) (diff-to-insert D1) (substring "ghetti" 1)))
(check-expect (update "The Amazing Spider Man" D1) (string-append (substring "The Amazing Spider Man" 0 1) (diff-to-insert D1) (substring "The Amazing Spider Man" 1)))



(define (insert str adiff)
  (string-append (substring str 0 (diff-pos adiff)) (diff-to-insert adiff) (substring str (diff-pos adiff))))




(check-expect (delete "abcdefghijklmnop" D2)
              (string-append (substring "abcdefghijklmnop" 0 (diff-pos D2))
                             (substring "abcdefghijklmnop" (+ (diff-num-chars D2) (diff-pos D2)))))
(check-expect (delete "0123456789" D3) "23456789")



(define (delete str adiff)
  (string-append (substring str 0 (diff-pos adiff))
                    (substring str (+ (diff-num-chars adiff) (diff-pos adiff)))))




(check-expect (out-of-bounds? "Barney" D2) true)
(check-expect (out-of-bounds? "" D2) false)
(check-expect (out-of-bounds? "Lamp Post" D3) false)



(define (out-of-bounds? str adiff)
  (and (pos-valid? str (diff-pos adiff)) (> (+ (diff-pos adiff) (diff-num-chars adiff)) (string-length str))))









(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)







(define LOI (list I1 I2 I3 I4))
(define LOI1 empty)
(define LOI2 (list I2))
(define LOI3 (list I2 I4))


 






(check-expect (total-width LOI) (+ (image-width I1) (image-width I2) (image-width I3) (image-width I4))) 
(check-expect (total-width LOI1) 0)
(check-expect (total-width LOI2) (image-width I2))
(check-expect (total-width LOI3) (+ (image-width I2) (image-width I4)))





(define (total-width loi)
  (cond
    [(empty? loi) 0]
    [else
     (+ (image-width (first loi))
        (total-width (rest loi)))]))




(check-expect (taller-than LOI 30)
              (list . .))
(check-expect (taller-than LOI1 50) empty)
(check-expect (taller-than LOI2 10) (list I2))
(check-expect (taller-than LOI3 (- (image-width I4) 1)) (list I4))



(define (taller-than loi cutoff)
  (cond
    [(empty? loi) empty]
    [(> (image-height (first loi)) cutoff)
     (cons (first loi) (taller-than (rest loi) cutoff))]
    [else (taller-than (rest loi) cutoff)]))










(define LOSI1 empty)
(define LOSI2 (list SI2))
(define LOSI3 (list SI3 SI4))
(define LOSI4 (list SI1 SI2 SI3))

 






(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
(check-expect (find-shape LOSI1 "triangle") empty)
(check-expect (find-shape LOSI2 "square") (list (build-image SI2)))
(check-expect (find-shape LOSI3 "triangle") empty)
(check-expect (find-shape LOSI4 "circle") (list (build-image SI1) (build-image SI3)))
                                                  


(define (find-shape losi str)
  (cond
    [(empty? losi) empty]
    [else
     (if (string=? (simple-img-type (first losi)) str)
         (cons (build-image (first losi)) (find-shape (rest losi) str))
         (find-shape (rest losi) str))]))










(define LOD1 empty)
(define LOD2 (cons D2 empty))
(define LOD3 (cons D3 (cons D4 empty)))

 






(check-expect (updates LOD1 "hey") "hey")
(check-expect (updates LOD2 "cheerios") "cheer")
(check-expect (updates LOD3 "0123456789") "2345678 boy9")





(define (updates lod str)
  (cond
    [(empty? lod) str]
    [else
     (updates (rest lod) (update str (first lod)))]))