

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assignment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)





(check-expect (backwards "") "")
(check-expect (backwards "swag") "gaws")
(check-expect (backwards "loved") "devol")
(check-expect (backwards "hannah") "hannah")
(check-expect (backwards "i love cs") "sc elov i")






(define-struct customer (firstname lastname age zipcode))







(define (fn-for-customer customer)
  (... (customer-firstname customer)
       (customer-lastname customer)
       (customer-age customer)
       (customer-zipcode customer)))



(define customer1 (make-customer "Bob" "Jones" 12 01702))
(define customer2 (make-customer "Jane" "Doe" 172 01532))
(define customer3 (make-customer "Tim" "Daggett" 59 01020))
(define customer4 (make-customer "" "" 0 0))

(check-expect (birthday customer1) (make-customer "Bob" "Jones" 13 01702))
(check-expect (birthday customer2) (make-customer "Jane" "Doe" 173 01532))
(check-expect (birthday customer3) (make-customer "Tim" "Daggett" 60 01020))
(check-expect (birthday customer4) (make-customer "" "" 1 0))






(define (birthday customer)
  (make-customer (customer-firstname customer)
                 (customer-lastname customer)
                 (+ (customer-age customer) 1)
                 (customer-zipcode customer)))



(define customer5 (make-customer "Bob" "Jones" 12 01702))
(define customer6 (make-customer "Jane" "Doe" 172 01532))
(define customer7 (make-customer "Tim" "Daggett" 59 01020))
(define customer8 (make-customer "" "" 0 0))

(check-expect (name-change "George" "Adams" customer5) (make-customer "George" "Adams" 12 01702))
(check-expect (name-change "Shelby" "Jones" customer6) (make-customer "Shelby" "Jones" 172 01532))
(check-expect (name-change "" "" customer7) (make-customer "" "" 59 01020))
(check-expect (name-change "Joe" "Stevens" customer8) (make-customer "Joe" "Stevens" 0 0))





(define (name-change fname lname customer)
  (make-customer fname
                 lname
                 (customer-age customer)
                 (customer-zipcode customer)))



(define-struct simple-img (shape color area))









(define (fn-for-simple-img simple-img)
  (... (simple-img-shape simple-img)
       (simple-img-color simple-img)
       (simple-img-area simple-img)))

(define SI (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI4 (make-simple-img "circle" "blue" 0))
(define SI5 (make-simple-img "circle" "orange" 0))
(define SI6 (make-simple-img "square" "green" 300))
(define SI7 (make-simple-img "circle" "magenta" 500))
(define SI8 (make-simple-img "triangle" "green" 450))



(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI4 SI5) false)
(check-expect (bigger? SI2 SI5) true)
(check-expect (bigger? SI5 SI) false)




(define (bigger? img1 img2)
  (> (simple-img-area img1) (simple-img-area img2)))



(check-expect (build-image SI) .)
(check-expect (build-image SI2) (square (square-side (simple-img-area SI2)) "solid" (simple-img-color SI2)))
(check-expect (build-image SI4) (circle (circle-radius (simple-img-area SI4)) "solid" (simple-img-color SI4)))
(check-expect (build-image SI5) (circle (circle-radius (simple-img-area SI5)) "solid" (simple-img-color SI5)))
(check-expect (build-image SI6) (square (square-side (simple-img-area SI6)) "solid" (simple-img-color SI6)))
(check-expect (build-image SI7) (circle (circle-radius (simple-img-area SI7)) "solid" (simple-img-color SI7)))
(check-expect (build-image SI8) (triangle (triangle-side (simple-img-area SI8)) "solid" (simple-img-color SI8)))




(define (build-image img)
  (cond [(string=? (simple-img-shape img) "triangle") (triangle (triangle-side (simple-img-area img)) "solid" (simple-img-color img))]
        [(string=? (simple-img-shape img) "square") (square (square-side (simple-img-area img)) "solid" (simple-img-color img))]
        [(string=? (simple-img-shape img) "circle") (circle (circle-radius (simple-img-area img)) "solid" (simple-img-color img))]))



(define (triangle-side area)
  (sqrt (/ (* area 4) (sqrt 3))))



(define (square-side area)
  (sqrt area))



(define (circle-radius area)
  (sqrt (/ area pi)))



(define-struct diff (diff? type))









(define (fn-for-diff diff)
  (... (diff-diff? diff)
       (diff-type diff)))




(define-struct insert-diff (position text))





(define (fn-for-insert-diff insert-diff)
  (... (insert-diff-position insert-diff)
       (insert-diff-text insert-diff)))

(define-struct delete-diff (position characters))





(define (fn-for-delete-diff delete-diff)
  (... (delete-diff-position delete-diff)
       (delete-diff-characters delete-diff)))




(define diff1 (make-diff (make-insert-diff 1 "xyzzy") 0))
(define diff2 (make-diff (make-delete-diff 3 2) 1))
(define diff3 (make-diff (make-insert-diff 5 "xyzzy") 0))
(define diff4 (make-diff (make-delete-diff 0 5) 1))

(check-expect (update "0123456789" diff1) "0xyzzy123456789")
(check-expect (update "0123456789" diff2) "01256789")
(check-expect (update "xyzzy" diff3) "xyzzyxyzzy")
(check-expect (update "xyzzy" diff4) "")




(define (update text diff)
  (cond [(= (diff-type diff) 0)
         (insert-string (insert-diff-position (diff-diff? diff)) text (insert-diff-text (diff-diff? diff)))]
        [(= (diff-type diff) 1)
         (delete-string (delete-diff-position (diff-diff? diff)) text (delete-diff-characters (diff-diff? diff)))]))



(define (insert-string position text inserted-text)
  (string-append (substring text 0 position) inserted-text (substring text position)))



(define (delete-string position text characters)
  (string-append (substring text 0 position) (substring text (+ position characters))))


(define diff5 (make-diff (make-insert-diff 4 "were you yesterday") 0))
(define diff6 (make-diff (make-delete-diff 4 13) 1))
(check-expect (update (update "How are you today?" diff6) diff5) "How were you yesterday?")









(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))




(define loi1 empty)
(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)
(define LOI (list I1 I2 I3 I4))

(check-expect (total-width loi1) 0)
(check-expect (total-width LOI) 265)




(define (total-width loi)
  (cond [(empty? loi) 0]
        [else
         (+ (image-width (first loi)) (total-width (rest loi)))]))




(check-expect (taller-than loi1 5) empty)
(check-expect (taller-than LOI 30)
              (list . .))




(define (taller-than loi cutoff)
  (cond [(empty? loi) empty]
        [else
         (if (> (image-height (first loi)) cutoff)
             (cons (first loi) (taller-than (rest loi) cutoff))
             (taller-than (rest loi) cutoff))]))








(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))




(define SI1 (make-simple-img "circle" "red" 120))
(define SI3 (make-simple-img "circle" "purple" 40))

(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))




(define (find-shape los shape)
  (cond [(empty? los) empty]
        [else
         (if (string=? (simple-img-shape (first los)) shape)
             (cons (build-image (first los)) (find-shape (rest los) shape))
             (find-shape (rest los) shape))]))








(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (first lod)
              (fn-for-lod (rest lod)))]))




(check-expect (updates (list diff2 diff1) "0123456789") "0xyzzy1256789")




(define (updates lod text)
  (cond
    [(empty? lod) text]
    [else
     (updates (rest lod) (update text (first lod)))]))