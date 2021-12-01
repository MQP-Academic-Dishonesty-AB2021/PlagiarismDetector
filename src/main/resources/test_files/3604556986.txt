

#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require racket/math)








(check-expect (backwards "") "")
(check-expect (backwards "l") "l")
(check-expect ("hello") "olleh")

(define (backwards Input)...)











(define-struct customer (fname lname age zip))

(define Riley (make-customer "Riley" "Smith" 7 11111))
(define Steve (make-customer "" "" 0 11111))



(check-expect (birthday Riley) (make-customer "Riley" "Smith" 8 11111))
(check-expect (birthday Steve) (make-customer "" "" 1 11111))

(define (birthday acustomer)
  (make-customer (customer-fname acustomer) (customer-lname acustomer)(+ (customer-age acustomer)1)(customer-zip acustomer)))



(check-expect (name-change Riley "Dan" "Shman") (make-customer "Dan" "Shman" 7 11111))
(check-expect (name-change Steve "Dan" "Shman") (make-customer "Dan" "Shman" 0 11111))

(define (name-change acustomer nfirst nlast)
  (make-customer nfirst nlast (customer-age acustomer)(customer-zip acustomer)))







(define-struct simple-img (shape color area))
 

(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))
(define a-circle (make-simple-img "circle" "green" 100))
(define fake-shape (make-simple-img "circle" "yellow" 0))



(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? a-circle a-circle) false)

(define (bigger? img1 img2)
  (> (simple-img-area img1) (simple-img-area img2)))



(check-expect (build-image SI2) (square (square-side 200) "solid" "purple"))
(check-expect (build-image SI) (triangle (tri-side 250) "solid" "red"))
(check-expect (build-image SI) .)
(check-expect (build-image a-circle) (circle (circle-rad 100) "solid" "green"))
(check-expect (build-image fake-shape) (circle (circle-rad 0) "solid" "yellow"))


(define (build-image img)
  (cond [(string=? (simple-img-shape img) "square")
         (square (square-side (simple-img-area img)) "solid" (simple-img-color img))]
        [(string=? (simple-img-shape img) "triangle")
         (triangle (tri-side (simple-img-area img)) "solid" (simple-img-color img))]
        [(string=? (simple-img-shape img) "circle")
         (circle (circle-rad (simple-img-area img)) "solid" (simple-img-color img))]
        [else
         "invalid shape"]))



(check-expect (square-side 25) (sqrt 25))
(check-expect (square-side 0) (sqrt 0))

(define (square-side area)
  (sqrt area))



(check-within (tri-side 17) (sqrt (/ (* 17 4) (sqrt 3))) 0.0001)
(check-within (tri-side 0) (sqrt (/ (* 0 4) (sqrt 3))) 0.0001)

(define (tri-side area)
  (sqrt (/ (* area 4) (sqrt 3))))



(check-within (circle-rad 11) (sqrt (/ 11 pi)) 0.0001)
(check-within (circle-rad 0) (sqrt (/ 0 pi)) 0.0001)

(define (circle-rad area)
  (sqrt (/ area pi)))













(define-struct diff (state pos delete insert))

(define ExIn (make-diff "insert" 3 empty " hi "))
(define ExDel (make-diff "delete" 3 2 empty))

(define ExEmpIn (make-diff "insert" 3 empty ""))
(define ExEmpDel (make-diff "delete" 3 0 empty))



(check-expect (update ExIn "aaaaa") "aaa hi aa")
(check-expect (update ExDel "aaaaa") "aaa")

(define (update adiff instring)
  (cond
    [(string=? (diff-state adiff) "insert")(Insertion adiff instring)]
    [(string=? (diff-state adiff) "delete")(Deletion adiff instring)]
    [else "invalid diff"]))




(check-expect (Insertion ExIn "aaaaa") "aaa hi aa")
(check-expect (Insertion ExEmpIn "aaaaa") "aaaaa")

(define (Insertion adiff instring)
  (string-append (substring instring 0 (diff-pos adiff))
                 (diff-insert adiff)
                 (substring instring(diff-pos adiff)(string-length instring))))




(check-expect (Deletion ExDel "aaaaa") "aaa")
(check-expect (Deletion ExEmpDel "aaaaa") "aaaaa")

(define (Deletion adiff instring)
  (string-append (substring instring 0 (diff-pos adiff))
                 (substring instring(+(diff-pos adiff)(diff-delete adiff))(string-length instring))))







(define Step1 (make-diff "delete" 4 1 empty))
(define Step2 (make-diff "insert" 4 empty "we"))
(define Step3 (make-diff "delete" 13 2 empty))
(define Step4 (make-diff "insert" 13 empty "yester"))

(update Step4 (update Step3 (update Step2 (update Step1 "How are you today?"))))














(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))
(define LOI2 (cons . empty))
(define LOI3 (cons (square 10 "solid" "red") (cons (triangle 5 "solid" "red") empty)))

 



(check-expect (total-width LOI) 265)
(check-expect (total-width LOI2) (image-width .))
(check-expect (total-width LOI3) (+ (image-width (square 10 "solid" "red")) (image-width (triangle 5 "solid" "red"))))


(define (total-width loi)
  (cond [(empty? loi) 0]
        [else
         (+ (image-width (first loi)) (total-width (rest loi)))]))




(check-expect (taller-than LOI 30)
              (list . .))
(check-expect (taller-than LOI2 5) LOI2)
(check-expect (taller-than LOI2 1000) empty)
(check-expect (taller-than LOI3 3) LOI3)
(check-expect (taller-than LOI3 6) (cons (first LOI3) empty))
(check-expect (taller-than LOI3 1000) empty)

(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) cutoff) (cons (first loi) (taller-than (rest loi) cutoff))]
        [else
         (taller-than (rest loi) cutoff)]))







(define losi (cons SI2 (cons SI (cons a-circle empty))))
(define SI1 (make-simple-img "circle" "red" 120))
(define SI3 (make-simple-img "circle" "purple" 40))

 

(simple-img-shape (first losi))
(simple-img-color (first losi))
(simple-img-area (first losi))





(check-expect (find-shape losi "square") (cons (build-image SI2) empty))
(check-expect (find-shape losi "triangle") (cons (build-image SI) empty))
(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))

(define (find-shape losi Type)
  (cond [(empty? losi) empty]
        [(string=? (simple-img-shape (first losi)) Type) (cons(build-image(first losi)) (find-shape(rest losi) Type))]
        [else (find-shape (rest losi) Type)]))








 

(define LOD1 (cons ExIn empty))
(define LOD2 (cons ExIn (cons ExDel empty)))
(define LOD3 (cons Step1 (cons Step2 (cons Step3 (cons Step4 empty))))) 



(check-expect (updates LOD1 "aaaa") (update (first LOD1) "aaaa"))
(check-expect (updates LOD3 "How are you today?") "How were you yesterday?")

(define (updates lod instring)
  (cond [(empty? lod) instring]
        [else
         (updates (rest lod) (update (first lod) instring))]))