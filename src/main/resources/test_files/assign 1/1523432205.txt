

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment 1 Nathan D Ethan S|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))








(define (backwards input-string) "") 


(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "dog") "god")
(check-expect (backwards "") "")
(check-expect (backwards 8) "")











(require 2htdp/image)













(define-struct customer (fname lname age zip))

(define C1 (make-customer "George" "Orwell" 45 12345))
(define C2 (make-customer "Sarah" "Maxwell" 21 67890))

  







(check-expect (birthday C1) (make-customer (customer-fname C1) (customer-lname C1) (+ 1 (customer-age C1)) (customer-zip C1)))
(check-expect (birthday C2) (make-customer (customer-fname C2) (customer-lname C2) (+ 1 (customer-age C2)) (customer-zip C2))) 

(define (birthday acustomer)
  (make-customer
   (customer-fname acustomer) 
   (customer-lname acustomer) 
   (+ 1 (customer-age acustomer)) 
   (customer-zip acustomer))) 







(check-expect (name-change C1 " " " ") (make-customer " " " " (customer-age C1) (customer-zip C1)))
(check-expect (name-change C1 "Barry" "Benson") (make-customer "Barry" "Benson" (customer-age C1) (customer-zip C1)))
(check-expect (name-change C2 "Johnny" "Boy") (make-customer "Johnny" "Boy" (customer-age C2) (customer-zip C2)))

(define (name-change acustomer newfname newlname)
  (make-customer
   newfname 
   newlname 
   (customer-age acustomer) 
   (customer-zip acustomer))) 
















(define-struct simple-img (shape color area))

(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI4 (make-simple-img "circle" "green" 250)) 

  







(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI SI4) false) 

(define (bigger? s1 s2)
  (if (> (simple-img-area s1) (simple-img-area s2))
      true
      false))










(check-expect (build-image SI) .)
(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))
(check-expect (build-image SI4) (circle (sqrt (/ 250 pi)) "solid" "green"))
(define (build-image si)
  (cond
   [(string=? (simple-img-shape si) "triangle")
    (triangle (sqrt (/ (* 4 (simple-img-area si)) (sqrt 3))) "solid" (simple-img-color si))]
   [(string=? (simple-img-shape si) "square")
    (square (sqrt (simple-img-area si)) "solid" (simple-img-color si))]
   [(string=? (simple-img-shape si) "circle")
    (circle (sqrt (/ (simple-img-area si) pi)) "solid" (simple-img-color si))]))














(define-struct diff-ins (pos insert))

 



(define (insert diff string) 
  (string-append (substring string 0 (diff-ins-pos diff)) (diff-ins-insert diff) (substring string (diff-ins-pos diff))))








(define-struct diff-del (pos1-start pos1-end pos2))

 

(define (delete diff string) 
  (string-append (substring string (diff-del-pos1-start diff) (diff-del-pos1-end diff)) (substring string (diff-del-pos2 diff))))
  







(check-expect (update (make-diff-ins 1 "xyzzy") "0123456789") "0xyzzy123456789")
(check-expect (update (make-diff-del 0 3 5) "0123456789") "01256789")
(check-expect (update (make-diff-ins 0 "") "") "")
(check-expect (update (make-diff-del 0 0 0) "") "")

(define (update diff string)
  (cond
    [(boolean=? (diff-ins? diff) true)
     (insert diff string)]
    [(boolean=? (diff-del? diff) true)
     (delete diff string)]
    [else " "]))





(update (make-diff-ins 13 "yesterday")
        (update (make-diff-del 0 13 18)
                (update (make-diff-ins 4 "were")
                        (update (make-diff-del 0 4 7) "How are you today?"))))






(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))
(define LOI2 (list I4 I2))
(define LOI3 empty)









 







(check-expect (total-width LOI) 265)
(check-expect (total-width LOI2) 85)
(check-expect (total-width LOI3) 0)

(define (total-width loi)
  (cond
    [(empty? loi) 0]
    [else
     (+ (image-width (first loi))
      (total-width (rest loi)))]))







(check-expect (taller-than LOI 30)
              (list . .))

(define (taller-than loi height)
  (cond
    [(empty? loi) empty]
    [else
     (if (> (image-height (first loi)) height)
         (cons (first loi) (taller-than (rest loi) height))
         (taller-than (rest loi) height))]))







(define SI1 (make-simple-img "circle" "red" 120))
(define SI3 (make-simple-img "circle" "purple" 40))






 








(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
(check-expect (find-shape empty " ") empty)
(check-expect (find-shape (list SI SI1 SI2 SI3 SI4) "square")
              (list .))

(define (find-shape losi shape)
  (cond
    [(empty? losi) empty]
    [(string=? shape (simple-img-shape (first losi)))
     (cons (build-image(first losi)) (find-shape (rest losi) shape))]
    [else
      (find-shape (rest losi) shape)]))








 







(define DEL (make-diff-del 0 3 5))
(define INS (make-diff-ins 1 "xyzzy"))

(check-expect (updates (list DEL INS) "0123456789") "0xyzzy1256789")
(check-expect (updates (list INS DEL) "0123456789") "0xyy123456789")
(check-expect (updates empty "0123456789") "0123456789")

(define (updates lod string)
  (cond
    [(empty? lod) string]
    [else
     (update
      (first (reverse lod))
      (updates (rest (reverse lod)) string))]))


