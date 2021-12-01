

#reader(lib "htdp-beginner-reader.ss" "lang")((modname V005) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)






(define (backwards s) "")

(check-expect (backwards "asdf") "fdsa")
(check-expect (backwards "") "")
(check-expect (backwards "hannah") "hannah")








(define-struct customer (first-name last-name age zip-code))

(define cust1 (make-customer "John" "Smith" 50 90210))
(define cust2 (make-customer "Caroline" "James" 29 12345))

(define (fn-for-cust cust)
  (... (customer-first-name cust)
       (customer-last-name cust)
       (customer-age cust)
       (customer-zip-code cust)))








(check-expect (birthday (make-customer "Jason" "Bourne" 30 11111))
              (make-customer "Jason" "Bourne" (+ 30 1) 11111))

(check-expect (birthday (make-customer "Mary" "Smith" 0 33221))
              (make-customer "Mary" "Smith" (+ 0 1) 33221))

(define (birthday cust)
  (make-customer (customer-first-name cust)
                 (customer-last-name cust)
                 (+ (customer-age cust) 1)
                 (customer-zip-code cust)))









(check-expect (name-change (make-customer "Caroline" "James" 29 12345)
                           "Caroline" "Smith")
              (make-customer "Caroline" "Smith" 29 12345))


(check-expect (name-change (make-customer "Jake" "Gyllenhaal" 40 55443)
                           "Your" "Mother")
              (make-customer "Your" "Mother" 40 55443))


(define (name-change cust new-first new-last)
  (make-customer new-first
                 new-last
                 (customer-age cust)
                 (customer-zip-code cust)))










(define-struct simple-img (shape color area))




(define (build-image simple)
  (cond [(string=? (simple-img-shape simple) "circle")
         (circle (sqrt (/ (simple-img-area simple) pi))
                 "solid"
                 (simple-img-color simple))]
        [(string=? (simple-img-shape simple) "square")
         (square (sqrt (simple-img-area simple))
                 "solid"
                 (simple-img-color simple))]
        [(string=? (simple-img-shape simple) "triangle")
         (triangle (sqrt (/ (* (simple-img-area simple) 4) (sqrt 3)))
                   "solid"
                   (simple-img-color simple))]
        [else
         (square 1 "solid" "white")]))

(check-expect (build-image (make-simple-img "square" "red" 100))
              (square 10 "solid" "red"))









(define (bigger? img1 img2)
  (> (simple-img-area img1) (simple-img-area img2)))



(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))

(check-expect (bigger? SI SI2) true)
(check-expect (build-image SI) .)











(define-struct diff (position edit))










(check-expect (update "My friend" (make-diff 3 "best ")) "My best friend")
(check-expect (update "Joe is my favorite student" (make-diff 10 9)) "Joe is my student")
(check-expect (update "CS is great" (make-diff 7 0)) "CS is great")
(check-expect (update "I like WPI" (make-diff 5 "")) "I like WPI")

(define (update string change)
  (if (string? (diff-edit change))
      (string-append (substring string 0 (diff-position change)) (diff-edit change) (substring string (diff-position change)))
      (string-append (substring string 0 (diff-position change)) (substring string (+ (diff-position change) (diff-edit change))))))


(define string-to-diff "How are you today?")
(define string-diffed "How were you yesterday?")
(define d1 (make-diff 4 3)) 
(define d2 (make-diff 4 "were")) 
(define d3 (make-diff 13 5)) 
(define d4 (make-diff 13 "yesterday")) 
(check-expect (update (update (update (update string-to-diff d1) d2) d3) d4) string-diffed)





















(define (total-width loi)
  (cond [(empty? loi) 0]
        [else
         (+ (image-width (first loi))
            (total-width (rest loi)))]))




(define (taller-than loi height)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) height)
         (cons (first loi) (taller-than (rest loi) height))]
        [else
         (taller-than (rest loi) height)]))















(define (find-shape losi type)
  (cond [(empty? losi) empty]
        [(string=? (simple-img-shape (first losi))
                   type)
         (cons (build-image (first losi))
               (find-shape (rest losi)
                           type))]
        [else
         (find-shape (rest losi) type)]))






(define-struct list-of-diffs (first rest))








(check-expect (updates string-to-diff (make-list-of-diffs d1 (make-list-of-diffs d2 (make-list-of-diffs d3 (make-list-of-diffs d4 empty))))) string-diffed)

(define (updates String diffs)
  (if (empty? diffs) String
      (updates (update String (list-of-diffs-first diffs)) (list-of-diffs-rest diffs))))





(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))


(check-expect (total-width LOI) 265) 

(check-expect (taller-than LOI 30)
              (list . .))



(define SI1 (make-simple-img "circle" "red" 120))
(define SI3 (make-simple-img "circle" "purple" 40))

(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))