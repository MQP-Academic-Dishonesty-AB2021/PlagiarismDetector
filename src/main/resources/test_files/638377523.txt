

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Ethan Chen, Seth Frank - Assignment 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))














(require 2htdp/image)





























(define-struct customer (fname lname age zip-code))








(define C1 (make-customer "John" "Doe" 30 01632))
(define C2 (make-customer "Mike" "Smith" 40 24742))

 







(check-expect (birthday C1) (make-customer "John" "Doe" (+ 30 1) 01632))
(check-expect (birthday C2) (make-customer "Mike" "Smith" (+ 40 1) 24742))



(define (birthday acustomer)
  (make-customer
   (customer-fname acustomer)
   (customer-lname acustomer)
   (+ (customer-age acustomer) 1)
   (customer-zip-code acustomer)))





(check-expect (name-change C1 "Joe" "Dane") (make-customer "Joe" "Dane" 30 01632))
(check-expect (name-change C2 "Dan" "Rogers") (make-customer "Dan" "Rogers" 40 24742))



(define (name-change acustomer fname lname)
  (make-customer
   fname
   lname
   (customer-age acustomer)
   (customer-zip-code acustomer)))










(define-struct simple-img (shape color area))













(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))


 






(check-expect (bigger? SI SI) false)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI SI2) true)



(define (bigger? simple1 simple2) 
  (> (simple-img-area simple1) (simple-img-area simple2)))









(check-expect (build-image (make-simple-img "circle" "blue" 300)) (circle (sqrt (/ 300 pi)) "solid" "blue"))
(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))
(check-expect (build-image SI) .)




(define (build-image a-image)
  (cond [(string=? "triangle" (simple-img-shape a-image)) (triangle (sqrt (/(simple-img-area a-image) 0.25 (sqrt 3))) "solid" (simple-img-color a-image))]
        [(string=? "square" (simple-img-shape a-image)) (square (sqrt (simple-img-area a-image)) "solid" (simple-img-color a-image))]
        [(string=? "circle" (simple-img-shape a-image)) (circle (sqrt (/ (simple-img-area a-image) pi)) "solid" (simple-img-color a-image))]))











(define-struct insert-diff (pos text))






 


(define-struct delete-diff (pos character-count))






 







 















(check-expect (update "0123456789" (make-insert-diff 1 "xyzzy"))
              "0xyzzy123456789")
(check-expect (update "0123456789" (make-delete-diff 3 2))
              "01256789")




(define (update text a-diff)
  (cond [(insert-diff? a-diff) (apply-insert-diff text a-diff)]
    [(delete-diff? a-diff) (apply-delete-diff text a-diff)]))






(check-expect (apply-insert-diff "0123456789" (make-insert-diff 1 "xyzzy"))
              "0xyzzy123456789")




(define (apply-insert-diff text diff)
  (string-append
   (substring text 0 (insert-diff-pos diff))
    (insert-diff-text diff)
   (substring text (insert-diff-pos diff))))









(define (apply-delete-diff text diff)
  (string-append
   (substring text 0 (delete-diff-pos diff))
   (substring text (+(delete-diff-pos diff) (delete-diff-character-count diff)))))





(check-expect
 (update (update (update (update "How are you today?"
         (make-delete-diff 12 5))
         (make-delete-diff 4 3) )
         (make-insert-diff 9 "yesterday"))
         (make-insert-diff 4 "were"))
              "How were you yesterday?")







(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))











 






(check-expect (total-width empty) 0)
(check-expect (total-width LOI) 265)



(define (total-width loi) 
  (cond [(empty? loi) 0]
        [else (+ (image-width (first loi))
                   (total-width(rest loi)))]))







(check-expect (taller-than LOI 30)
              (list . .))


(define (taller-than loi height)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) height)
         (cons (first loi) (taller-than (rest loi) height))]
        [else (taller-than(rest loi) height)])) 














(define SI1 (make-simple-img "circle" "red" 120))
(define SI3 (make-simple-img "circle" "purple" 40))

 






(check-expect (find-shape empty "triangle") empty)
(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))




(define (find-shape losi shape)
  (cond [(empty? losi) empty]
        [(string=? (simple-img-shape (first losi)) shape)
         (cons (build-image(first losi)) (find-shape (rest losi) shape))]
        [else (find-shape (rest losi) shape)]))














 






(check-expect (updates
               (list 
                     (make-delete-diff 3 2)
                     (make-insert-diff 1 "xyzzy"))
               "0123456789")
"0xyzzy1256789")



(define (updates lod text)
  (cond [(empty? lod) text]
        [else (updates (rest lod) (update text (first lod)))]))







