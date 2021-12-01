

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname FINAL_assignment#1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)



























(define-struct customer (firstName lastName age zip-code))

(define Customer1 (make-customer "Daiwik" "Pal" 17 12345))
(define Customer2 (make-customer "Aman" "Hiregoudar" 18 54321))

(define (fn-for-customer CUSTOMER) 
  (... (customer-firstName CUSTOMER)
       (customer-lastName CUSTOMER)
       (customer-age CUSTOMER)
       (customer-zip-code CUSTOMER)))






(check-expect (birthday Customer1) (make-customer "Daiwik" "Pal" (+ 17 1) 12345))
(check-expect (birthday Customer2) (make-customer "Aman" "Hiregoudar" (+ 18 1) 54321))

(define (birthday CUSTOMER)
  (make-customer (customer-firstName CUSTOMER) (customer-lastName CUSTOMER) (+ (customer-age CUSTOMER) 1) (customer-zip-code CUSTOMER)))
  
    





(check-expect (name-change Customer1 "" "") (make-customer "" "" (customer-age Customer1) (customer-zip-code Customer1)))
(check-expect (name-change Customer2 "Joe" "Smith") (make-customer "Joe" "Smith" (customer-age Customer2) (customer-zip-code Customer2)))

(define (name-change CUSTOMER new-first new-last) 
  (make-customer new-first new-last (customer-age CUSTOMER) (customer-zip-code CUSTOMER)))











(define-struct simple-img (shape-type color area))

(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI3 (make-simple-img "circle" "blue" 150))

(define (fn-for-simple-img SI)
  (... (simple-img-shape-type SI)
       (simple-img-color SI)
       (simple-img-area SI)))







(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) (> (simple-img-area SI2) (simple-img-area SI)))

(define (bigger? SI1 SI2)
  (> (simple-img-area SI1) (simple-img-area SI2)))
  





(check-expect (build-image SI) .)
(check-expect (build-image SI2) (square (sqrt (simple-img-area SI2)) "solid" (simple-img-color SI2)))
(check-expect (build-image SI3) (circle (sqrt(/ (simple-img-area SI3) pi)) "solid" (simple-img-color SI3)))

(define (build-image SI)
  (cond[(equal? "triangle" (simple-img-shape-type SI))
        (triangle (sqrt(/ ( * 4 (simple-img-area SI)) (sqrt 3))) "solid" (simple-img-color SI))]
       [(equal? "square" (simple-img-shape-type SI))
        (square (sqrt (simple-img-area SI)) "solid" (simple-img-color SI)) ]
       [(equal? "circle" (simple-img-shape-type SI))
        (circle (sqrt(/ (simple-img-area SI) pi)) "solid" (simple-img-color SI))]))


 










(define-struct diff(type init-index input))

(define D1 (make-diff "insertion" 1 "xyzzy"))
(define D2 (make-diff "deletion" 3 "2"))
(define D3 (make-diff "insertion" 10 "hello"))
 
(define (fn-for-diff aDiff input)
  (... (diff-type aDiff)
       (diff-init-index aDiff)
       (diff-input aDiff)))







(check-expect (update D1 "0123456789") "0xyzzy123456789")
(check-expect (update D2 "0123456789") "01256789")

(define (update aDiff init-string) 
  (cond[(equal? (diff-type aDiff) "insertion") (insert aDiff init-string)]
       [(equal? (diff-type aDiff) "deletion" ) (delete aDiff init-string)]))







(check-expect (insert D1 "") "xyzzy") 
(check-expect (insert D3 "0123") "0123hello") 
(check-expect (insert D1 "0123456789") (string-append
                                        (substring "0123456789" 0 (diff-init-index D1))
                                        (diff-input D1)
                                        (substring "0123456789" (diff-init-index D1)))) 


(define (insert aDiff init-string)
  (cond [(> (diff-init-index aDiff) (string-length init-string))
         (string-append
          init-string
          (diff-input aDiff))]
        [else
         (string-append
          (substring init-string 0 (diff-init-index aDiff))
          (diff-input aDiff)
          (substring init-string (diff-init-index aDiff)))]))








(check-expect (delete D2 "0123456789")
              (string-append
               (substring  "0123456789" 0 (diff-init-index D2))
               (substring  "0123456789" (+ (diff-init-index D2) (string->number(diff-input D2))))))
(check-expect (delete D2 "34") "34") 
(check-expect (delete D2 "4567") "456") 
(check-expect (delete D2 "") "") 


(define (delete aDiff init-string)
  (cond [(< (string-length init-string)
            (diff-init-index aDiff)) init-string]
        [(< (string-length init-string) (+ (diff-init-index aDiff) (string->number(diff-input aDiff))))
         (substring init-string 0 (diff-init-index aDiff))]
        [else
         (string-append
          (substring  init-string 0 (diff-init-index aDiff))
          (substring  init-string (+ (diff-init-index aDiff) (string->number(diff-input aDiff)))))]))




(define STRING "How are you today?")

(define Diff1 (make-diff "deletion" 4 "3"))
(define Diff2 (make-diff "insertion" 4 "were"))
(define Diff3 (make-diff "deletion" 13 "5"))
(define Diff4 (make-diff "insertion" 13 "yesterday"))

STRING
(update Diff4 (update Diff3 (update Diff2 (update Diff1 STRING))))






(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)







(define LOI1 empty)
(define LOI2 (list I1 I2 I3 I4))

 







(check-expect (total-width LOI1) 0) 
(check-expect (total-width LOI2) (+ (image-width I4) (+ (image-width I3) (+ (image-width I1) (image-width I2)))))  

(define (total-width loi)
  (cond[(empty? loi) 0]
       [else
        (+ (image-width(first loi))
           (total-width(rest loi)))]))






(check-expect (taller-than LOI1 24) empty)
(check-expect (taller-than LOI2 50) (list I1 I4))

(define (taller-than loi cuttoff)
  (cond[(empty? loi) empty]
       [(> (image-height (first loi)) cuttoff)
        (cons (first loi) (taller-than(rest loi) cuttoff))]
       [else
        (taller-than(rest loi)cuttoff)]))














(define si1 (make-simple-img "circle" "red" 120)) 
(define si2 (make-simple-img "square" "lightblue" 90))
(define si3 (make-simple-img "circle" "purple" 40)) 
(define si4 (make-simple-img "triangle" "orange" 60))

(define LOSI1 empty)
(define LOSI2 (list si1 si2 si3 si4)) 

 








(check-expect (find-shape LOSI1 "square") empty)
(check-expect (find-shape LOSI2 "circle")
              (list . .))

(define (find-shape losi image-type)
  (cond[(empty? losi) empty]
       [(equal? image-type (simple-img-shape-type (first losi)))
        (cons (build-image (first losi)) (find-shape (rest losi) image-type))]
       [else
        (find-shape (rest losi) image-type)]))











(define LOD1 empty)
(define LOD2 (list Diff1 Diff2 Diff3 Diff4)) 
(define LOD3 (list D2 D1)) 

 







(check-expect (updates LOD1 "012345789") "012345789") 
(check-expect (updates LOD2 "How are you today?") "How were you yesterday?")
(check-expect (updates LOD3 "0123456789") "0xyzzy1256789")


(define (updates lod inital-string)
  (cond[(empty? lod) inital-string]
       [else
        (updates (rest lod) (update (first lod) inital-string))]))







