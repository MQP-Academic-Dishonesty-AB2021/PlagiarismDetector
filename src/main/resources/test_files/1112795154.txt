

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 1102_assignment1_samuelW_audreyG) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))















(require 2htdp/image)







(define (backwards str) "") 


(check-expect (backwards "") "")
(check-expect (backwards "a") "a")
(check-expect (backwards "abc") "cba")








(define-struct customer (firstN lastN age zip))








 







(check-expect (birthday (make-customer "Audrey" "Gross" 18 93012))
              (make-customer "Audrey" "Gross" 19 93012))
(check-expect (birthday (make-customer "Samuel" "Wilensky" 1 06489))
              (make-customer "Samuel" "Wilensky" 2 06489))

(define (birthday theCustomer)
  (make-customer (customer-firstN theCustomer)
                 (customer-lastN theCustomer)
                 (+ 1 (customer-age theCustomer))
                 (customer-zip theCustomer)))







(check-expect (name-change (make-customer "Audrey" "Gross" 18 93012) "Natalie" "Jones")
              (make-customer "Natalie" "Jones" 18 93012))
(check-expect (name-change (make-customer "Samuel" "Wilensky" 1 06489) "Toby" "Wilensky")
              (make-customer "Toby" "Wilensky" 1 06489))
(check-expect (name-change (make-customer "John" "Doe" 44 00000) "John" "Doe")
              (make-customer "John" "Doe" 44 00000))
(check-expect (name-change (make-customer "Maria" "Ross" 23 01471) "" "")
              (make-customer "" "" 23 01471))

(define (name-change theCustomer newFN newLN)
  (make-customer newFN
                 newLN
                 (customer-age theCustomer)
                 (customer-zip theCustomer)))




(define-struct simple-img (type color area))









(define SI  (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))

 







(define (bigger? s-img1 s-img2)
  (> (simple-img-area s-img1) (simple-img-area s-img2)))

(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI SI) false)
(check-expect (bigger? SI2 SI) false)









(check-expect (build-image SI) .)
(check-expect (build-image SI2) (square (sqrt (simple-img-area SI2)) "solid" (simple-img-color SI2)))
(check-expect (build-image (make-simple-img "circle" "blue" 100)) (circle (sqrt (/ 100 pi)) "solid" "blue"))

(define (build-image s-img)
  (cond [(string=? (simple-img-type s-img) "circle") 
         (circle (sqrt (/ (simple-img-area s-img) pi)) "solid" (simple-img-color s-img))]
        [(string=? (simple-img-type s-img) "square") 
         (square (sqrt (simple-img-area s-img)) "solid" (simple-img-color s-img))]
        [else                                        
         (triangle (sqrt (* (simple-img-area s-img) (/ 4 (sqrt 3)))) "solid" (simple-img-color s-img))]))



(define-struct diff (insert? index del str))






(define (fn-for-diff adiff)
  (... (diff-insert? adiff)
       (diff-index adiff)
       (diff-del adiff)
       (diff-str adiff)))

(define D1 (make-diff true 1 0 "xyzzy"))
(define D2 (make-diff false 3 2 ""))












(check-expect (update D1 "0123456789") "0xyzzy123456789")
(check-expect (update D2 "0123456789") "01256789")
(check-expect (update D1 "") "xyzzy")
(check-expect (update D2 "") "")


(define (update adiff text)
  (if (diff-insert? adiff)
      (if (= 0 (string-length text))                                
          (diff-str adiff)                                          
          (insert text (diff-str adiff) (diff-index adiff)))
      (if (= 0 (string-length text))                                
          ""                                                        
          (delete text (diff-index adiff) (diff-del adiff)))))






(check-expect (insert "012345" "abc" 2) "01abc2345")
(check-expect (insert "012345" "" 2) "012345")
(check-expect (insert "" "abc" 0) "abc")

(define (insert base change index)
  (string-append (substring base 0 index)
                         change
                         (substring base index)))






(check-expect (delete "012345" 2 3) "015")
(check-expect (delete "012345" 2 0) "012345")

(define (delete base index num)
  (string-append (substring base 0 index)
                 (substring base (+ index num))))


















(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4)) 

 







(check-expect (total-width LOI) 265)
(check-expect (total-width (list (square 20 "solid" "magenta"))) 20)
(check-expect (total-width empty) 0)

(define (total-width loi)
  (cond [(empty? loi) 0]
        [else (+ (image-width (first loi))
                 (total-width (rest loi)))]))







(check-expect (taller-than empty 12) empty)
(check-expect (taller-than (list (square 20 "solid" "magenta")) 10) (list (square 20 "solid" "magenta")))
(check-expect (taller-than (list (square 20 "solid" "magenta")) 50) empty)
(check-expect (taller-than LOI 30)
              (list . .))

(define (taller-than loi height)
  (cond [(empty? loi) empty]
        [else (if (> (image-height (first loi)) height)            
              (cons (first loi) (taller-than (rest loi) height))   
              (taller-than (rest loi) height))]))                  





(define SI1 (make-simple-img "circle" "red" 120))
(define SI3  (make-simple-img "circle" "purple" 40))






 







(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
(check-expect (find-shape (list SI SI2) "circle") empty)
(check-expect (find-shape empty "circle") empty)

(define (find-shape losi type)
  (cond [(empty? losi) empty]
        [else (if (string=? (simple-img-type (first losi)) type)
                  (cons (build-image (first losi)) (find-shape (rest losi) type))
                  (find-shape (rest losi) type))]))







 








(check-expect (updates (list D2 D1) "0123456789") "0xyzzy1256789")
(check-expect (updates (list D1 D2) "0123456789") "0xyy123456789")
(check-expect (updates empty "0123456789") "0123456789")
(check-expect (updates (list D2 D1) "") "xyzzy")

(define (updates lod text)
  (cond [(empty? lod) text]
        [else (updates (rest lod) (update (first lod) text))]))

