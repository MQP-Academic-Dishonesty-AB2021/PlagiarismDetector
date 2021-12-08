

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Comp Sci Assignment 1 |) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)








(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "") "")
(check-expect (backwards "racecar") "racecar")
(check-expect (backwards "TacoCat") "taCocaT")


(define (backwards string) "dcba")



















(define-struct customer (fn ln age zip)) 










(check-expect (birthday (make-customer "Dylan" "Roskind" 18 94065)) (make-customer "Dylan" "Roskind" 19 94065))
(check-expect (birthday (make-customer "Naomi" "Treto" 18 90631)) (make-customer "Naomi" "Treto" 19 90631))


(define (birthday customer)
  (make-customer (customer-fn customer) (customer-ln customer) (+ (customer-age customer) 1) (customer-zip customer))
  )









(check-expect (name-change (make-customer "Dylan" "Roskind" 18 94065) "Nice" "TA") (make-customer "Nice" "TA" 18 94065))
(check-expect (name-change (make-customer "Naomi" "Treto" 18 90631) "Cool" "TA") (make-customer "Cool" "TA" 18 90631))


 


(define (name-change customer newfn newln)
  (make-customer newfn newln (customer-age customer) (customer-zip customer))
  )





(define-struct simple-img (shape color area))








(define SI (make-simple-img "triangle" "red" 250))    

(define SI2 (make-simple-img "square" "purple" 200))

(define SI3 (make-simple-img "circle" "green" 300))


 











(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI SI) false)


 


(define (bigger? img1 img2)
  (if (> (simple-img-area img1) (simple-img-area img2))
      true
      false))
  






(check-expect (build-image SI2) (rectangle (sqrt 200) (sqrt 200) "solid" "purple"))
(check-expect (build-image SI) (triangle (sqrt (/ 250 (sqrt (/ 3 4)))) "solid" "red"))
(check-expect (build-image SI3) (circle (sqrt (/ 300 pi)) "solid" "green"))


 


(define (build-image img)
  (cond[(string=? (simple-img-shape img) "triangle")
        (triangle (sqrt (/ (simple-img-area img) (sqrt (/ 3 4))))  "solid" (simple-img-color img))]
       [(string=? (simple-img-shape img) "square")
        (rectangle (sqrt (simple-img-area img)) (sqrt (simple-img-area img)) "solid" (simple-img-color img))]
       [(string=? (simple-img-shape img) "circle")
        (circle (sqrt (/ (simple-img-area img) pi)) "solid" (simple-img-color img))]))


















(define-struct Function (Func Id Change))








(define-struct diff (LOF))











(check-expect (update "0123456789" (make-diff (list (make-Function 1 1 "xyzzy") empty))) "0xyzzy123456789")
(check-expect (update "0123456789" (make-diff (list (make-Function 0 3 2) empty))) "01256789")


(check-expect (update "How are you today?" (make-diff (list
                                                       (make-Function 0 4 3)            
                                                       (make-Function 1 4 "were")       
                                                       (make-Function 0 13 5)           
                                                       (make-Function 1 13 "yesterday") 
                                                       empty)
                                                      )) "How were you yesterday?")


 


(define (update String diff)
  (cond [(empty? (first (diff-LOF diff))) String]
        [(= 0 (Function-Func (first (diff-LOF diff)))) (update (delete String (first (diff-LOF diff))) (make-diff (rest (diff-LOF diff))))]
        [(= 1 (Function-Func (first (diff-LOF diff)))) (update (insert String (first (diff-LOF diff))) (make-diff (rest (diff-LOF diff))))]))










(define (delete String Function)
  (string-append (substring String 0 (Function-Id Function))
                 (substring String (+ (Function-Change Function) (Function-Id Function)) (string-length String))))

                        









(define (insert String Function)
  (string-append (substring String 0 (Function-Id Function))
                 (Function-Change Function)
                 (substring String (Function-Id Function) (string-length String))))
















(define-struct LOI (list-img))


(define IMG1 .)


(define IMG2 .)


(define IMG3 (rectangle 20 10 "solid" "blue"))










(check-expect (total-width (make-LOI (list IMG1 IMG2 IMG3))) (+ (+ 51 125) 20))
(check-expect (total-width (make-LOI (list IMG1 IMG2))) (+ 51 125))
(check-expect (total-width (make-LOI (list IMG2 IMG3))) (+ 125 20))








(define (total-width imgList)
  (cond [(empty? (LOI-list-img imgList)) 0]
        [true (+ (image-width (first (LOI-list-img imgList))) (total-width (make-LOI(rest (LOI-list-img imgList)))))]))







(check-expect (taller-than (make-LOI (list IMG1 IMG2 IMG3)) 0) (list IMG1 IMG2 IMG3))
(check-expect (taller-than (make-LOI (list IMG1 IMG2)) 1000) empty)
(check-expect (taller-than (make-LOI (list IMG2 IMG3)) 25) (list .))







(define (taller-than imgList num)
  (cond[(empty? (LOI-list-img imgList)) empty]
  [(> (image-height(first (LOI-list-img imgList))) num) (cons (first (LOI-list-img imgList)) (taller-than (make-LOI (rest (LOI-list-img imgList)))num))]
[true (taller-than (make-LOI (rest (LOI-list-img imgList)))num)]))











(define-struct LOSI (LOSI))








(check-expect (find-shape (make-LOSI (list SI SI2 SI3)) "triangle") (list SI))
(check-expect (find-shape (make-LOSI (list SI SI2)) "circle") empty)
(check-expect (find-shape (make-LOSI (list SI SI2 SI3)) "square") (list SI2))



(define (find-shape LOSI String)
  (cond [(empty? (LOSI-LOSI LOSI)) null]
        [(string=? (simple-img-shape(first (LOSI-LOSI LOSI))) String) (cons (first (LOSI-LOSI LOSI)) (find-shape (make-LOSI (rest (LOSI-LOSI LOSI))) String))]
        [true (find-shape (make-LOSI (rest (LOSI-LOSI LOSI))) String)]))
        





(check-expect (update "0123456789" (make-diff (list(make-Function 1 1 "xyzzy") (make-Function 0 8 2) empty))) "0xyzzy1256789")














(define-struct LOD (LOD))











 