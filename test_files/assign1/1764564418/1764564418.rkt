

#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)









(check-expect (backwards "abcdef") "fedcba")
(check-expect (backwards "aaaa") "aaaa")
(check-expect (backwards " aaa") "aaa ")
(check-expect (backwards "c") "c")

(define (backwards str1) "")







(define-struct customer (first last age zip))



(define C1 (make-customer "John" "Smith" 50 12309))
(define C2 (make-customer "Jake" "Hand" 18 01609))
(define C3 (make-customer "Sam" "Jones" 25 12345))

    







(check-expect (birthday (make-customer "John" "Smith" 50 12309))
              (make-customer "John" "Smith" 51 12309))
(check-expect (birthday (make-customer "John" "" 0 12309))
              (make-customer "John" "" 1 12309))



(define (birthday C)     
  (make-customer (customer-first C) (customer-last C) (+ 1 (customer-age C)) (customer-zip C)))





(check-expect (name-change "Jake" "Hand" (make-customer "John" "Smith" 50 12309))
              (make-customer "Jake" "Hand" 50 12309))
(check-expect (name-change "John" "" (make-customer "" "" 0 12309))
              (make-customer "John" "" 0 12309))



(define (name-change first last C)     
  (make-customer first last (customer-age C) (customer-zip C)))






(define-struct simple-img (shape color area))




    






(check-expect (bigger? (make-simple-img "tri" "blue" 50)
                       (make-simple-img "cir" "green" 70)) "simple-img 2 has a larger area")
(check-expect (bigger? (make-simple-img "squ" "blue" 900)
                       (make-simple-img "tri" "green" 20)) "simple-img 1 has a larger area")
(check-expect (bigger? (make-simple-img "cir" "blue" 70)
                       (make-simple-img "cir" "green" 70)) "They have equal areas")




(define (bigger? smp1 smp2)     
(cond[(> (simple-img-area smp1) (simple-img-area smp2))
   "simple-img 1 has a larger area"]
   [(< (simple-img-area smp1) (simple-img-area smp2))
    "simple-img 2 has a larger area"]
   [else "They have equal areas"]))






(check-expect (build-image (make-simple-img "tri" "blue" 50))
              (triangle (expt (/ (* 8 50) 3) (/ 1 3)) 'solid "blue"))
(check-expect (build-image (make-simple-img "squ" "green" 100))
              (square (sqrt 100) 'solid "green"))
(check-expect (build-image (make-simple-img "cir" "red" 10))
              (circle (sqrt (/ 10 3.14)) 'solid "red"))




(define (build-image smp1)     
(cond
  [(equal? "tri" (simple-img-shape smp1))
   (triangle (expt (/ (* 8 (simple-img-area smp1)) 3) (/ 1 3)) 'solid (simple-img-color smp1))]
  [(equal? "squ" (simple-img-shape smp1))
   (square (sqrt (simple-img-area smp1)) 'solid (simple-img-color smp1))]
  [else
   (circle (sqrt (/ (simple-img-area smp1) 3.14)) 'solid (simple-img-color smp1))]))






(define-struct diff (Ad/Sub charnum pos text))





    







(check-expect (update (make-diff "add" 4 4 "567") "123489") "123456789")
(check-expect (update (make-diff "del" 4 0 "") "123489") "89")
(check-expect (update (make-diff "del" 0 2 "") "123489") "123489")






(define (update dif str1)
(cond [
       (eqv? (diff-Ad/Sub dif) "add")
       (insert (diff-text dif) (diff-pos dif) str1)]
      [
       (eqv? (diff-Ad/Sub dif) "del")
       (remover (diff-charnum dif) (diff-pos dif) str1)]))






(check-expect (insert "89" 7 "1234567") "123456789")
(check-expect (insert "45" 0 "1234567") "451234567")
(check-expect (insert "38" 3 "1234567") "123384567")



(define (insert dstr pos ostr)
 (string-append (string-append (substring ostr 0 pos) dstr)
                (substring ostr pos)))

  




(check-expect (remover 3 4 "0123456789") "0123789")
(check-expect (remover 2 0 "0123456789") "23456789")
(check-expect (remover 2 8 "0123456789") "01234567")
(check-expect (remover 0 4 "0123456789") "0123456789")



(define (remover char pos str)
  (string-append (substring str 0 pos) (substring str (+ pos char))))





(define orig "How are you today?")

(define one
 (update (make-diff "del" 3 4 "") orig))

(define two
 (update (make-diff "add" 4 4 "were") one))

(define three
 (update (make-diff "del" 5 13 "") two))

(define four
 (update (make-diff "add" 9 13 "yesterday") three))












 



(define L1 (cons (rectangle 10 10 "solid" "red")
                                 (cons (rectangle 10 20 "solid" "red") empty)))




(check-expect (total-width L1) 20)

(check-expect (total-width empty) 0)

(check-expect (total-width (cons (circle 100 "solid" "blue")
                                 (cons (rectangle 10 20 "solid" "red") (cons (triangle 10 "outline" "green") empty)))) 220)




(define (total-width loi)
  (cond [(empty? loi) 0]
        [else
  (+ (image-width (first loi)) (total-width (rest loi)))]))
  






(check-expect (taller-than L1 9) (cons (rectangle 10 10 "solid" "red")
                                 (cons (rectangle 10 20 "solid" "red") empty)))

(check-expect (taller-than empty 0) empty)

(check-expect (taller-than (cons (circle 100 "solid" "blue")
                                 (cons (rectangle 10 20 "solid" "red")
                                       (cons (triangle 10 "outline" "green") empty))) 20)
              (cons (circle 100 "solid" "blue") empty))




(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) cutoff)
         (cons (first loi) (taller-than (rest loi) cutoff) )]
        [else
         (taller-than (rest loi) cutoff)]))













 



(check-expect (find-shape (cons (make-simple-img "tri" "blue" 30) (cons (make-simple-img "squ" "red" 70) empty)) "tri")
              (cons (build-image (make-simple-img "tri" "blue" 30)) empty))

(check-expect (find-shape (cons (make-simple-img "squ" "blue" 30) (cons (make-simple-img "squ" "red" 70) empty)) "squ")
              (cons (build-image (make-simple-img "squ" "blue" 30))(cons (build-image (make-simple-img "squ" "red" 70)) empty)))

(check-expect (find-shape (cons (make-simple-img "tri" "blue" 30) (cons (make-simple-img "squ" "red" 70) empty)) "cir")
              empty)


(define (find-shape losi str)
  (cond [(empty? losi)
         empty]
        [(eqv? str (simple-img-shape (first losi)))
         (cons (build-image (first losi)) (find-shape (rest losi) str))]
        [else
         (find-shape (rest losi) str)]))













 






(check-expect (updates (cons (make-diff "del" 3 4 "") (cons (make-diff "add" 4 4 "were") empty)) "How are you today?") "How were you today?")
(check-expect (updates (cons (make-diff "del" 3 4 "") (cons (make-diff "del" 4 4 "") empty)) "How are you today?") "How  today?")
(check-expect (updates (cons (make-diff "add" 3 0 "012") (cons (make-diff "add" 4 3 "3456") empty)) "") "0123456")
(check-expect (updates (cons (make-diff "del" 3 1 "") (cons (make-diff "del" 1 1 "") empty)) "abcdef") "af")

(define (updates lod str)
  (cond [(= (length lod) 1) (update (first lod) str)]
        [else
  (updates (rest lod) (update (first lod) str))]))