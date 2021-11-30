

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Assignment1-AdamKalayjian-KeatonMangone) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






(require 2htdp/image)
















 


















(define-struct customer (fn ln age zip))

(define (fn-for-customer customer)
  (... (customer-fn customer)
       (customer-ln customer)
       (customer-age customer)
       (customer-zip customer)))





(check-expect (birthday
               (make-customer "bob" "jones" 15 80003))
              (make-customer "bob" "jones" 16 80003))
(check-expect (birthday
               (make-customer "james" "mar" 78 82303))
              (make-customer "james" "mar" 79 82303))
               


 

(define (birthday customer)
  (make-customer
   (customer-fn customer)
   (customer-ln customer)
   (+ (customer-age customer) 1)
   (customer-zip customer)))





(check-expect (name-change
               (make-customer "bob" "jones" 15 80003) "nick" "paige")
              (make-customer "nick" "paige" 15 80003))
(check-expect (name-change 
               (make-customer "james" "mar" 78 82303) "john" "williams")
              (make-customer "john" "williams" 78 82303))
               



 

(define (name-change customer newfn newln)
  (make-customer
   newfn
   newln
   (customer-age customer)
   (customer-zip customer)))













(define-struct simple-img (shape color area))

(define (fn-for-simple-img img)
  (... (simple-img-shape img)
       (simple-img-color img)
       (simple-img-area img)))




(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI5 (make-simple-img "circle" "green" 150))





(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI SI5) true)
(check-expect (bigger? SI5 SI2) false)



 

(define (bigger? img1 img2)
  (> (simple-img-area img1)
       (simple-img-area img2)))






(check-expect (build-image SI) .)
(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))
(check-expect (build-image SI5) (circle (sqrt (/ 150 pi)) "solid" "green"))



 

(define (build-image img)
  (cond [(string=? (simple-img-shape img) "triangle")
         (triangle (* 2/3 (expt 3 3/4) (sqrt (simple-img-area img))) "solid" (simple-img-color img))]
        [(string=? (simple-img-shape img) "square")
         (square (sqrt (simple-img-area img)) "solid" (simple-img-color img))]
        [(string=? (simple-img-shape img) "circle")
         (circle (sqrt (/ (simple-img-area img) pi)) "solid" (simple-img-color img))]
        [else 0]))


  















(define-struct insertion (str))

(define (fn-for-insertion insertion)
  (... (insertion-str insertion)))




(define-struct deletion (num-chr))

(define (fn-for-deletion deletion)
  (... (deletion-num-chr deletion)))





(define (fn-for-operation op)
  (cond [(insertion? op)
         (fn-for-insertion op)]
        [(deletion? op)
         (fn-for-deletion op)]))





(define-struct diff (operation location))

(define (fn-for-diff diff)
  (... (diff-location diff)
       (fn-for-operation (diff-operation diff))))





(check-expect (update "Cool Guy" (make-diff (make-insertion "hello") 5)) "Cool helloGuy")
(check-expect (update "Nice Friend" (make-diff (make-deletion 3) 4)) "Niceiend")
(check-expect (update "0123456789" (make-diff (make-insertion "xyzzy") 1)) "0xyzzy123456789")
(check-expect (update "0123456789" (make-diff (make-deletion 2) 3)) "01256789")



 

(define (update str diff)
  (cond [(insertion? (diff-operation diff))
         (apply-insertion str (insertion-str (diff-operation diff)) (diff-location diff))]
        [(deletion? (diff-operation diff))
         (apply-deletion str (deletion-num-chr (diff-operation diff)) (diff-location diff))]))





(check-expect (apply-insertion "this is funny" "not " 8) "this is not funny")
(check-expect (apply-insertion "I hate cake." "cheese" 7) "I hate cheesecake.")
(check-expect (apply-insertion "testing out of bounds, " "it works!" 100) "testing out of bounds, it works!")



 

(define (apply-insertion base-str new-str location)
  (if (> (string-length base-str) location)
      (string-append
       (substring base-str 0 location)
       new-str
       (substring base-str location))
      (string-append base-str new-str)))





(check-expect (apply-deletion "this is not funny" 4 8) "this is funny")
(check-expect (apply-deletion "welcome to jungle" 2 14) "welcome to june")
(check-expect (apply-deletion "are you there?" 4 0) "you there?")
(check-expect (apply-deletion "out of bounds test!" 10 50) "out of bounds test!")




 

(define (apply-deletion base-str num-chr location)
  (if (> (string-length base-str) location)
      (string-append
       (substring base-str 0 location)
       (substring base-str (+ location num-chr)))
      base-str))




(define D1 (make-diff (make-deletion 2) 4))
(define D2 (make-diff (make-insertion "wer") 4))


(define D3 (make-diff (make-deletion 2) 13))
(define D4 (make-diff (make-insertion "yester") 13))

(check-expect
 (update
  (update
   (update
    (update "How are you today?" D1) D2) D3) D4)
 "How were you yesterday?")










(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))









(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))





(check-expect (total-width LOI) 265)
(check-expect (total-width (list . . . .)) 145)
(check-expect (total-width empty) 0)



 

(define (total-width loi)
  (cond [(empty? loi) 0]
        [else
         (+ (image-width (first loi)) 
              (total-width (rest loi)))]))





(check-expect (taller-than LOI 30)
              (list . .))
(check-expect (taller-than
               (list . . . .) 30)
              (list . .))
(check-expect (taller-than
               (list . . . . . .) 10)
              empty)
 



 

(define (taller-than loi height)
  (cond [(empty? loi) empty]
        [else
         (if (> (image-height (first loi)) height)
             (cons (first loi) (taller-than (rest loi) height))
             (taller-than (rest loi) height))]))








(define SI1 (make-simple-img "circle" "red" 120))
(define SI3 (make-simple-img "circle" "purple" 40))








(define (fn-for-losi losi)
  (cond [(empty? losi) (...)]
        [else
         (... (first losi)
              (fn-for-losi (rest losi)))]))





(check-expect (find-shape (list SI SI1 SI2 SI3 SI5) "circle")
              (list . . .))
(check-expect (find-shape (list SI SI1 SI2 SI3 SI5) "triangle")
              (list .))
(check-expect (find-shape (list SI SI1 SI2 SI3 SI5) "square")
              (list .))



 

(define (find-shape losi shape)
  (cond [(empty? losi) empty]
        [else
         (if (string=? (simple-img-shape (first losi)) shape)
             (cons (build-image (first losi)) (find-shape (rest losi) shape))
             (find-shape (rest losi) shape))]))












(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (first lod)
              (fn-for-lod (rest lod)))]))







(check-expect (updates (list D1 D2 D3 D4) "How are you today?") "How were you yesterday?") 
(check-expect (updates empty "This shouldn't be changed.") "This shouldn't be changed.")
(check-expect (updates (list
                        (make-diff (make-deletion 4) 0)
                        (make-diff (make-insertion "are ") 6)
                        (make-diff (make-deletion 1) 20)
                        (make-diff (make-insertion ".") 20))
                       "are these some words?")
              "these are some words.")



 

(define (updates lod str)
  (cond [(empty? lod) str]
        [else
         (updates
          (rest lod)
          (update str (first lod)))]))
