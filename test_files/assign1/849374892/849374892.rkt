

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Assignment1_kFlerlage_iPoulsen) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)









(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "dcba") "abcd")
(check-expect (backwards "") "")
(define (backwards str) "") 






(define-struct customer (fname lname age zipcode))





(define C1 (make-customer "Ian" "Poulsen" 18 11111))
(define C2 (make-customer "Kylie" "Flerlage" 18 21468))

(define (fn-for-customer c)
  (...(customer-fname c)
      (customer-lname c)
      (customer-age c)
      (customer-zipcode c)))












(check-expect (birthday C1) (make-customer "Ian" "Poulsen" 19 11111))
(check-expect (birthday (make-customer "John" "Smith" 0 12345))
              (make-customer "John" "Smith" 1 12345))

(define (birthday c)
  (make-customer
   (customer-fname c)
   (customer-lname c)
   (+ 1 (customer-age c))
   (customer-zipcode c)))







(check-expect (name-change C1 "Brian" "Lewis")
              (make-customer "Brian" "Lewis" 18 11111))
(check-expect (name-change C2 "" "") (make-customer "" "" 18 21468))
(check-expect (name-change (make-customer "" "" 12 12345) "Lu" "Bo")
              (make-customer "Lu" "Bo" 12 12345))
 
(define (name-change c fname lname)
  (make-customer fname lname (customer-age c) (customer-zipcode c)))











(define-struct simple-img (type color area))

(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img  "square" "purple" 200))
(define BLANK (make-simple-img "square" "white" 0))

(define (fn-for-simple-img img)
  (... (img-type img)
       (img-color img)
       (img-area img)))












(define (bigger? img1 img2)
  (> (simple-img-area img1) (simple-img-area img2)))

(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI (make-simple-img "square" "purple" 250)) false)







(define (build-image img)
  (cond
    [(string=? (simple-img-type img) "triangle")
     (triangle (sqrt( / (* 4 (simple-img-area img)) (sqrt 3))) "solid"
               (simple-img-color img))]
    [(string=? (simple-img-type img) "square")
     (square (sqrt(simple-img-area img)) "solid"
             (simple-img-color img))]
    [(string=? (simple-img-type img) "circle")
     (circle (sqrt(/ (simple-img-area img) pi)) "solid"
             (simple-img-color img))]
    ))
(check-expect (build-image SI) .)
(check-expect (build-image SI2)
              (square (sqrt(simple-img-area SI2)) "solid"
                      (simple-img-color SI2)))






(define-struct diffi (pos1-end str))

(define (fn-for-diffi diff)
  (... (diff-pos1-end)
       (diff-str)))








(define-struct diffd  (pos1-end pos2-start))

(define (fn-for-diffd diff)
  (... (diff-pos1-start)
       (diff-pos1-end)
       (diff-pos2-start)))















(check-expect (update (make-diffi 1 "xyzzy") "0123456789") "0xyzzy123456789")
(check-expect (update (make-diffd 3 5) "0123456789") "01256789")
(check-expect (update (make-diffi 0 "") "") "")
(check-expect (update (make-diffd 0 0) "") "")



(define (update diff str)
  (cond
    [(boolean=? (diffi? diff) true) (insert diff str)]
    [(boolean=? (diffd? diff) true) (delete diff str)]
    [else str]))


(define (delete diff string)
  (string-append (substring string 0 (diffd-pos1-end diff))
                 (substring string (diffd-pos2-start diff))))

(define (insert diff string)
  (string-append (substring string 0 (diffi-pos1-end diff))
                 (diffi-str diff)
                 (substring string (diffi-pos1-end diff))))




(update (make-diffi 13 "yesterday")
        (update (make-diffd 13 18)
                (update (make-diffi 4 "were")
                        (update (make-diffd 4 7) "How are you today?"))))













 





(check-expect (total-width (list I1 I2 I3 I4) )
              (+ (image-width I1) (image-width I2)
                 (image-width I3) (image-width I4)))
(check-expect (total-width empty) 0)



(define (total-width loi) 
 (cond [(empty? loi) 0]
        [else
         (+ (image-width (first loi))
            (total-width (rest loi)))]))





(check-expect (taller-than empty 0) empty)
(check-expect (taller-than LOI 50) (list I1 I4))
(check-expect (taller-than LOI 150) empty)
(check-expect (taller-than LOI 0) (list I1 I2 I3 I4))



(define (taller-than loi cut)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) cut)
         (cons (first loi)
               (taller-than (rest loi) cut))]
        [else (taller-than (rest loi) cut)]))
         
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
(check-expect (find-shape empty 0) empty)
(check-expect (find-shape (list SI1 SI3) "square") empty)
(check-expect (find-shape (list SI1 SI3) "circle")
              (list (build-image SI1) (build-image SI3)))



(define (find-shape losm type)
  (cond [(empty? losm) empty]
        [(string=? (simple-img-type (first losm)) type)
         (cons (build-image(first losm)) (find-shape (rest losm) type))]
        [else (find-shape (rest losm) type)]))









 







(define DEL (make-diffd 3 5))
(define INS (make-diffi 1 "xyzzy"))

(check-expect (updates (list DEL INS) "0123456789") "0xyzzy1256789")
(check-expect (updates (list DEL DEL INS) "0123456789") "0xyzzy12789")
(check-expect (updates  (list "") "0123456789") "0123456789")

(define (updates lod str)
  (cond [(empty? lod) str]
        [else
         (update (first (reverse lod)) (updates (rest (reverse lod)) str))]))
