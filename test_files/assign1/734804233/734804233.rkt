

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |CS 1102 Assignment 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)





(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "tacocat") "tacocat")
(check-expect (backwards "") "")
(check-expect (backwards "ihateracket") "tekcaretahi")

(define (backwards input) "") 





(define-struct customer (fname lname age zipcode))





(define customer1 (make-customer "ben" "heffer" 24 93849))
(define customer2 (make-customer "zack" "handel" 17 92739))













(check-expect (birthday customer1) (make-customer (customer-fname customer1)
                                                  (customer-lname customer1)
                                                  (+ (customer-age customer1) 1)
                                                  (customer-zipcode customer1)))
(check-expect (birthday customer2) (make-customer (customer-fname customer2)
                                                  (customer-lname customer2)
                                                  (+ (customer-age customer2) 1)
                                                  (customer-zipcode customer2)))                                            

(define (birthday customer-in)
  (make-customer (customer-fname customer-in)
                 (customer-lname customer-in)
                 (+ (customer-age customer-in) 1)
                 (customer-zipcode customer-in)))







(check-expect (name-change customer1 "" "") (make-customer
                                             ""
                                             ""
                                             (customer-age customer1)
                                             (customer-zipcode customer1)))
(check-expect (name-change customer1 "silly" "george") (make-customer
                                                        "silly"
                                                        "george"
                                                        (customer-age customer1)
                                                        (customer-zipcode customer1)))
(check-expect (name-change customer2 "Audrey" "Carter") (make-customer
                                                         "Audrey"
                                                         "Carter"
                                                         (customer-age customer2)
                                                         (customer-zipcode customer2)))

(define (name-change customer-in new-first new-last)
  (make-customer new-first
                 new-last
                 (customer-age customer-in)
                 (customer-zipcode customer-in)))




(define-struct simple-img (shape area color))



(define SI (make-simple-img "triangle" 250 "red"))
(define SI2 (make-simple-img "square" 200 "purple"))
(define SI-3 (make-simple-img "circle" 300 "blue"))












(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI SI2) (> (simple-img-area SI) (simple-img-area SI2)))
(check-expect (bigger? SI2 SI) (< (simple-img-area SI) (simple-img-area SI2)))
(check-expect (bigger? SI SI) (> (simple-img-area SI) (simple-img-area SI)))   

(define (bigger? simple-img1 simple-img2)
  (> (simple-img-area simple-img1) (simple-img-area simple-img2)))






(check-expect (build-image SI) .)
(check-expect (build-image SI)
              (triangle (sqrt (/ (* 4 (sqrt 3) (simple-img-area SI)) 3)) "solid" (simple-img-color SI)))
(check-expect (build-image SI2)
              (square (sqrt (simple-img-area SI2)) "solid" (simple-img-color SI2)))
(check-expect (build-image SI-3)
              (circle (sqrt (/ (simple-img-area SI-3) pi)) "solid" (simple-img-color SI-3)))

(define (build-image simple-img-in)
  (cond [(string=? (simple-img-shape simple-img-in) "triangle")
         (build-triangle simple-img-in)]
        [(string=? (simple-img-shape simple-img-in) "square")
         (build-square simple-img-in)]
        [(string=? (simple-img-shape simple-img-in) "circle")
         (build-circle simple-img-in)]))






(check-expect (build-triangle SI)
              (triangle (sqrt (/ (* 4 (sqrt 3) (simple-img-area SI)) 3))
                        "solid"
                        (simple-img-color SI)))
(check-expect (build-triangle SI2)
              (triangle (sqrt (/ (* 4 (sqrt 3) (simple-img-area SI2)) 3))
                        "solid"
                        (simple-img-color SI2)))
(check-expect (build-triangle SI-3)
              (triangle (sqrt (/ (* 4 (sqrt 3) (simple-img-area SI-3)) 3))
                        "solid"
                        (simple-img-color SI-3)))

(define (build-triangle simple-image-in)
  (triangle (sqrt (/ (* 4 (sqrt 3) (simple-img-area simple-image-in)) 3))
            "solid"
            (simple-img-color simple-image-in)))







(check-expect (build-square SI)
              (square (sqrt (simple-img-area SI))
                      "solid"
                      (simple-img-color SI)))
(check-expect (build-square SI2)
              (square (sqrt (simple-img-area SI2))
                      "solid"
                      (simple-img-color SI2)))
(check-expect (build-square SI-3)
              (square (sqrt (simple-img-area SI-3))
                      "solid"
                      (simple-img-color SI-3)))

(define (build-square simple-image-in)
  (square (sqrt (simple-img-area simple-image-in))
          "solid"
          (simple-img-color simple-image-in)))







(check-expect (build-circle SI)
              (circle (sqrt (/ (simple-img-area SI) pi))
                      "solid"
                      (simple-img-color SI)))
(check-expect (build-circle SI2)
              (circle (sqrt (/ (simple-img-area SI2) pi))
                      "solid"
                      (simple-img-color SI2)))
(check-expect (build-circle SI-3)
              (circle (sqrt (/ (simple-img-area SI-3) pi))
                      "solid"
                      (simple-img-color SI-3)))

(define (build-circle simple-image-in)
  (circle (sqrt (/ (simple-img-area simple-image-in) pi))
          "solid"
          (simple-img-color simple-image-in)))




(define-struct diff (index insert delete))










(define diff1 (make-diff 0 "hi" 2))
(define diff2 (make-diff 3 "bye" 0))
(define diff3 (make-diff 2 "" 3))

 







(define diff-1 (make-diff 2 "hate" 4))
(define diff-2 (make-diff 1 "xyzzy" 0))
(define diff-3 (make-diff 1 "" 2))

(check-expect (update diff-1 "I love doctor racket")
              (string-append (substring "I love doctor racket" 0 2)
                             "hate"
                             (substring "I love doctor racket" 6 (string-length  "I love doctor racket"))))
                                                                     
(check-expect (update diff-2 "0123456789")
              (string-append (substring "0123456789" 0 1)
                             "xyzzy"
                             (substring "0123456789" 1 (string-length "0123456789"))))

(check-expect (update diff-3 "0123456789")
              (string-append (substring "0123456789" 0 1)
                             ""
                             (substring "0123456789" 3 (string-length "0123456789"))))


(define (update diff-in string-in)
  (string-append
   (substring string-in 0 (diff-index diff-in))
   (diff-insert diff-in)
   (substring string-in (+ (diff-index diff-in) (diff-delete diff-in)) (string-length string-in))))



(define diff-4 (make-diff 4 "were" 3))
(define diff-5 (make-diff 13 "yesterday" 5))

(check-expect (update diff-5 (update diff-4 "How are you today?")) "How were you yesterday?")












(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))
(define LOI2 empty)

  







(check-expect (total-width LOI) 265)
(check-expect (total-width empty) 0)

(define (total-width loi)
  (cond [(empty? loi) 0]
        [else
         (+ (image-width (first loi))
            (total-width (rest loi)))]))







(check-expect (taller-than empty 20) empty)
(check-expect (taller-than LOI 30)
              (list . .))

(define (taller-than LOI min-height)
  (cond [(empty? LOI) empty]
        [(> (image-height (first LOI)) min-height)
         (cons (first LOI) (taller-than (rest LOI) min-height))]
        [else
         (taller-than (rest LOI) min-height)]))









(define SI1 (make-simple-img "circle" 120 "red"))
(define SI3 (make-simple-img "circle" 40 "purple"))
(define LSI1 empty)
(define LSI2 (list SI SI2))
(define LSI3 (list SI SI2 SI3))












(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
(check-expect (find-shape LSI2 "circle") empty)
(check-expect (find-shape LSI3 "square")
              (list (build-image (make-simple-img "square" 200 "purple"))))
(check-expect (find-shape LSI3 "triangle")
              (list (build-image (make-simple-img "triangle" 250 "red"))))

(define (find-shape losi shape-name)
  (cond [(empty? losi) empty]
        [(string=? (simple-img-shape (first losi)) shape-name)
         (cons (build-image (first losi)) (find-shape (rest losi) shape-name))]
        [else (find-shape (rest losi) shape-name)]))









 









(define lod1 (list diff-4 diff-5))

(check-expect (updates "hi" empty) "hi")
(check-expect (updates "How are you today?" lod1) "How were you yesterday?")

(define (updates string-in lod)
  (cond [(empty? lod) string-in]
        [else (updates (update (first lod) string-in) (rest lod))]))
              


