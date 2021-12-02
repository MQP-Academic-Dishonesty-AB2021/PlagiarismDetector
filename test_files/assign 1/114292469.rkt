

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |CS1102 Assignment 1 Woodward Antupit-1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






(require 2htdp/image)









(check-expect (backwards "") "")
(check-expect (backwards "ABCD") "DCBA")
(check-expect (backwards "hello world") "dlrow olleh")

(define (backwards text)
 (if (> (string-length text) 0)
     (string-append (backwards (substring text 1)) (substring text 0 1))
     ""))










(define-struct customer (fname lname age zip))
 




(define (birthday customer)
  (make-customer (customer-fname customer)
                 (customer-lname customer)
                 (+ 1 (customer-age customer))
                 (customer-zip customer)))

(check-expect (birthday (make-customer "Joe" "" 25 01609))
              (make-customer "Joe" "" 26 01609))
(check-expect (birthday (birthday (make-customer "Bob" "" 24 01609)))
              (make-customer "Bob" "" 26 01609))




(define (name-change customer fname lname)
  (make-customer fname
                 lname
                 (customer-age customer)
                 (customer-zip customer)))

(check-expect (name-change (make-customer "Joe" "" 25 01609) "Bob" "Builder")
              (make-customer "Bob" "Builder" 25 01609))
(check-expect (name-change (make-customer "" "" 25 01609) "" "")
              (make-customer "" "" 25 01609))









(define-struct simple-img (shape color area))
 


(define SI (make-simple-img "triangle" "red" 250))

(define SI2 (make-simple-img "square" "purple" 200))




(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI SI) false)



(define (bigger? simple-img-1 simple-img-2)
  (> (simple-img-area simple-img-1)
     (simple-img-area simple-img-2)))




(check-expect (build-image (make-simple-img "circle" "blue" 100))
              (circle (sqrt (/ 100 pi)) "solid" "blue"))
(check-expect (build-image SI) .)
(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))



(define (build-image simple-img)
  (cond
    [(string=? "square" (simple-img-shape simple-img))
     (square (sqrt (simple-img-area simple-img)) "solid" (simple-img-color simple-img))]
    
    [(string=? "circle" (simple-img-shape simple-img))
     (circle (sqrt (/ (simple-img-area simple-img) pi)) "solid" (simple-img-color simple-img))]

    [(string=? "triangle" (simple-img-shape simple-img))
     (triangle (* (expt 3 3/4) 2/3 (sqrt (simple-img-area simple-img)))
               "solid" (simple-img-color simple-img))]
    
    [else (...)]))











(define-struct diff (index init result))
 










(check-expect (update "there is this thing" (make-diff 9 "this thing" "")) "there is ")
(check-expect (update "there is this thing" (make-diff 19 "" ".")) "there is this thing.")
(check-expect (update "there is that" (make-diff 9 "that" "this thing")) "there is this thing")
(check-expect (update "0123456789" (make-diff 1 "" "xyzzy")) "0xyzzy123456789")
(check-expect (update "0123456789" (make-diff 2 "23" "")) "01456789")



(check-expect (update (update "How are you today?"
                        (make-diff 4 "a" "we"))
                      (make-diff 13 "to" "yester"))
              "How were you yesterday?")

(define (update text diff)
  (if (> (string-length (diff-init diff)) 0) 
      (string-append (substring text 0 (diff-index diff))
                     (diff-result diff)
                     (substring text (+ (diff-index diff) (string-length (diff-init diff)))))
      
      (string-append (substring text 0 (diff-index diff))
                     (diff-result diff)
                     (substring text (diff-index diff)))))






 


(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))

(define LOI2 (list (circle 20 "solid" "blue")
                   (rectangle 40 60 "solid" "green")
                   (ellipse 80 100 "solid" "red")))






(define (total-width loi)
  (if (empty? loi) 
      0
      (+ (image-width (first loi)) (total-width (rest loi)))))

(check-expect (total-width LOI2) 160)
(check-expect (total-width empty) 0)
(check-expect (total-width LOI) 265) 






(check-expect (taller-than empty 0) empty)
(check-expect (taller-than LOI2 50) 
              (list (rectangle 40 60 "solid" "green")
                    (ellipse 80 100 "solid" "red")))
(check-expect (taller-than LOI2 120) 
              empty)

(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) cutoff)
         (cons (first loi) (taller-than (rest loi) cutoff))]
        [else (taller-than (rest loi) cutoff)]))

(check-expect (taller-than LOI 30)
              (list . .))







 

(define LOSI (list SI SI2))




(define (find-shape losi shape)
  (cond [(empty? losi) empty]
        [(string=? (simple-img-shape (first losi)) shape) 
         (cons (build-image (first losi)) (find-shape (rest losi) shape))]
        [else (find-shape (rest losi) shape)]))


(check-expect (find-shape LOSI "triangle") 
              (list (build-image (make-simple-img "triangle" "red" 250))))
(check-expect (find-shape LOSI "square") 
              (list (build-image (make-simple-img "square" "purple" 200))))
(check-expect (find-shape empty "square") empty)

(define SI1 (make-simple-img "circle" "red" 120))
(define SI3 (make-simple-img "circle" "purple" 40))

(check-expect (build-image SI1) .)
(check-expect (build-image SI3) .)

(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))





 






(define (updates lod text)
  (if (empty? lod) 
      text
      (updates (rest lod) (update text (first lod)))))

(define DEL (make-diff 3 "34" ""))
(define INS (make-diff 1 "" "xyzzy"))
(define nochange (make-diff 0 "" "" ))

(check-expect (updates (list nochange) "0123456789") "0123456789")
(check-expect (updates (list DEL) "0123456789") "01256789")
(check-expect (updates (list INS) "0123456789") "0xyzzy123456789")
(check-expect (updates (list DEL INS nochange) "0123456789") "0xyzzy1256789")
(check-expect (updates (list DEL INS) "0123456789") "0xyzzy1256789")