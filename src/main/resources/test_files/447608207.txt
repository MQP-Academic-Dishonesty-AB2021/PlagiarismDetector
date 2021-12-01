

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |assigment 1 alex ramirez|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))










(require 2htdp/image)










(check-expect (backwards "") "") 
(check-expect (backwards "amogus") "sugoma")
(check-expect (backwards "racecar") "racecar") 
(check-expect (backwards "reimu") "umier")





(define (backwards input_string)
  (list->string (reverse (string->list input_string)))) 














(define-struct customer(fname lname age zip))


(define CUS1 (make-customer "Richard" "Head" 69 69420))
(define CUS2 (make-customer "Reimu" "Hakurei" 17 77777))
(define CUS3 (make-customer "Nicholas" "Ishigami" 18 07006))



(check-expect (customer-age(birthday CUS1)) 70)
(check-expect (customer-age(birthday CUS2)) 18)
(check-expect (customer-age(birthday CUS3)) 19)
(check-expect (customer-fname(birthday CUS2)) (customer-fname CUS2)) 

(check-expect (customer-fname(name-change CUS1 "amo" "amogus")) "amo")
(check-expect (customer-lname(name-change CUS1 "amo" "amogus" )) "amogus")
(check-expect (customer-lname(name-change CUS2 "HAHHAHA" "" )) "")
(check-expect (customer-age(name-change CUS2 "HAHHAHA" "" )) (customer-age CUS2)) 
(check-expect (customer-zip(name-change CUS2 "HAHHAHA" "" )) (customer-zip CUS2)) 


 





 


(define (birthday customer)
  (make-customer (customer-fname customer)
                 (customer-lname customer)
                 (+ (customer-age customer) 1)
                 (customer-zip customer)))





 


(define (name-change customer new-fname new-lname)
  (make-customer new-fname
                 new-lname
                 (customer-age customer)
                 (customer-zip customer)))












(define-struct simple-img(shape colour area))







(define WITHIN_DELTA .00000000001)
(define PI 3.14159265358979323846) 
(define SI (make-simple-img "triangle" "red" 250))
(define SI1 (make-simple-img "circle" "red" 120))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI3 (make-simple-img "circle" "purple" 40))
(define SI4 (make-simple-img "circle" "blue" PI)) 
(define SI5 (make-simple-img "circle" "blue" (* 64 PI))) 
(define SI6 (make-simple-img "triangle" "white" 0)) 
(define SI7 (make-simple-img "triangle" "white" (/ (* (sqrt 3) 81) 4))) 
(define SI8 (make-simple-img "triangle" "white" (/ (* (sqrt 3) 36) 4))) 



(check-expect (bigger? SI SI2) true) 
(check-expect (bigger? SI SI) false)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI2 SI2) false)


(check-expect (build-image SI) .) 


(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))
(check-expect (build-image SI6) (triangle 0 "solid" "white"))


(check-expect (triangle-side-length (simple-img-area SI6)) 0) 
(check-within (triangle-side-length (simple-img-area SI7)) 9 WITHIN_DELTA) 
(check-within (triangle-side-length (simple-img-area SI8)) 6 WITHIN_DELTA) 


(check-expect (circle-radius (simple-img-area SI4)) 1) 
(check-expect (circle-radius (simple-img-area SI5)) 8) 


 






 


(define (bigger? simg1 simg2)
  (> (simple-img-area simg1) (simple-img-area simg2)))





 


(define (build-image simg)
  (cond [(equal? (simple-img-shape simg) "triangle") (triangle (triangle-side-length (simple-img-area simg))
                                                               "solid"
                                                               (simple-img-colour simg))]
        
        [(equal? (simple-img-shape simg) "circle") (circle (circle-radius (simple-img-area simg))
                                                           "solid"
                                                           (simple-img-colour simg))]
        
        [else (square (sqrt (simple-img-area simg)) "solid" (simple-img-colour simg))]))        





 


(define (triangle-side-length area)
  (sqrt (* 4/3 (sqrt 3) area)))





 


(define (circle-radius area)
  (sqrt (/ area PI)))




















(define-struct diff(type start wild))


(define EXAMPLE_STRING "0123456789")
(define EXAMPLE_DIFF1 (make-diff true 1 "xyzzy"))
(define EXAMPLE_DIFF2 (make-diff false 3 2))


(define Q3_STRING "How are you today?")
(define Q3_DIFF1 (make-diff false 4 1)) 
(define Q3_DIFF2 (make-diff true 4 "we")) 
(define Q3_DIFF3 (make-diff false 13 2)) 
(define Q3_DIFF4 (make-diff true 13 "yester")) 



(check-expect (update Q3_STRING Q3_DIFF1) "How re you today?")
(check-expect (update "How re you today?" Q3_DIFF2) "How were you today?")
(check-expect (update "How were you today?" Q3_DIFF3) "How were you day?")
(check-expect (update "How were you day?" Q3_DIFF4) "How were you yesterday?")


(check-expect (diff-add EXAMPLE_STRING EXAMPLE_DIFF1) "0xyzzy123456789")
(check-expect (diff-add EXAMPLE_STRING (make-diff true 1 "")) "0123456789") 


(check-expect (diff-del EXAMPLE_STRING EXAMPLE_DIFF2) "01256789")
(check-expect (diff-del EXAMPLE_STRING (make-diff false 1 0)) "0123456789") 


 





 


(define (update string diff)
  (cond [(diff-type diff) (diff-add string diff)]
        [(not (diff-type diff)) (diff-del string diff)]
        [else "how did you mess up this badly?"])) 





 


(define (diff-add string pdiff)
  (string-append 
   (substring string 0 (diff-start pdiff)) 
   (diff-wild pdiff) 
   (substring string (diff-start pdiff)))) 





 


(define (diff-del string ndiff)
  (string-append 
   (substring string 0 (diff-start ndiff)) 
   (substring string (+ (diff-start ndiff) (diff-wild ndiff))))) 












(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)
(define LOI (list I1 I2 I3 I4))
(define LOI2 empty)


 



(check-expect (total-width LOI) 265) 
(check-expect (total-width (taller-than LOI 20)) 185) 
(check-expect (total-width empty) 0) 

(check-expect (taller-than LOI 30) 
              (list . .))
(check-expect (taller-than LOI -1) LOI) 
(check-expect (taller-than LOI 999.9) empty) 





 


(define (total-width loi)
  (cond [(empty? loi) 0] 
        [else
         (+ (image-width (first loi)) (total-width (rest loi)))])) 





 


(define (taller-than loi cutoff)
  (cond [(empty? loi) empty] 
        [(> (image-height (first loi)) cutoff) 
         (cons (first loi) (taller-than (rest loi) cutoff))] 
        [else 
         (taller-than (rest loi) cutoff)])) 












 



(check-expect (find-shape (list SI SI1 SI2 SI3) "circle") 
              (list . .))
(check-expect (find-shape (list SI SI1 SI2 SI3) "square") 
              (list .)) 
(check-expect (find-shape (list SI SI1 SI2 SI3) "triangle") 
              (list .)) 
(check-expect (find-shape (list SI SI1 SI2 SI3 SI4 SI5 SI6 SI7 SI8) "triangle") 
              (list . . . .)) 
(check-expect (find-shape (list SI SI1 SI2 SI3 SI4 SI5 SI6 SI7 SI8) "pentagon") 
              empty) 






 


(define (find-shape losi req-shape)
  (cond [(empty? losi) empty] 
        [(equal? req-shape (simple-img-shape (first losi))) 
         (cons (build-image (first losi)) (find-shape (rest losi) req-shape))] 
        [else 
         (find-shape (rest losi) req-shape)])) 












(define INS EXAMPLE_DIFF1)
(define DEL EXAMPLE_DIFF2)
(define Q3_DIFFS (list Q3_DIFF1 Q3_DIFF2 Q3_DIFF3 Q3_DIFF4)) 


 



(check-expect (updates (list DEL INS) "0123456789") "0xyzzy1256789") 
(check-expect (updates Q3_DIFFS Q3_STRING) "How were you yesterday?") 
(check-expect (updates empty "AMONG US") "AMONG US") 





 


(define (updates lod input)
  (cond [(empty? lod) input] 
        [else (updates (rest lod) (update input (first lod)))])) 


