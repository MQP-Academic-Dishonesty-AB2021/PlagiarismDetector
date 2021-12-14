

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Assignment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)










(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "") "")
(check-expect (backwards "d") "d")



















(define-struct customer (first last age zip))



(define mike (make-customer "mike" "Wilkinson" 19 12345))
(define alex (make-customer "alex" "mintz" 0 07087))

(define (fn-for-customer customer1)
  (...
    (customer-first customer1) 
    (customer-last customer1)  
    (customer-age customer1)   
    (customer-zip customer1))) 






(check-expect (birthday mike) (make-customer "mike" "Wilkinson" (+ 19 1) 12345))
(check-expect (birthday alex) (make-customer "alex" "mintz" (+ 0 1) 07087))








(define (birthday customer1)
  (make-customer (customer-first customer1) (customer-last customer1)
                 (+ (customer-age customer1) 1) (customer-zip customer1)))






(check-expect (name-change mike "steve" "smith") (make-customer "steve" "smith" 19 12345))
(check-expect (name-change alex "lisa" "sil") (make-customer "lisa" "sil" 0 07087))








(define (name-change customer1 first last)
  (make-customer first last (customer-age customer1) (customer-zip customer1)))













(define-struct simple-img (shape color area))


(define SI1 (make-simple-img "triangle" "red" 250))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI3 (make-simple-img "circle" "green" 300))


(define (fn-for-simple-img simple-img1)
  (...
   (simple-img-shape simple-img1)  
   (simple-img-color simple-img1)  
   (simple-img-area simple-img1))) 










(check-expect (bigger? SI1 SI2) true)
(check-expect (bigger? SI2 SI3) false)
(check-expect (bigger? SI3 SI3) false)







(define (bigger? simple-img1 simple-img2)
  (> (simple-img-area simple-img1) (simple-img-area simple-img2)))












(check-expect (build-image SI1) (triangle (sqrt (/ (* 4 250) (sqrt 3))) "solid" "red"))
(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))
(check-expect (build-image SI3) (circle (sqrt (/ 300 pi)) "solid" "green"))







(define (build-image simple-img1)
  (cond
    [(equal? (simple-img-shape simple-img1) "square")
     (square (sqrt (simple-img-area simple-img1)) "solid" (simple-img-color simple-img1))]
    [(equal? (simple-img-shape simple-img1) "circle")
     (circle (sqrt (/ (simple-img-area simple-img1) pi)) "solid" (simple-img-color simple-img1))]
    [else
     (triangle (sqrt (/ (* 4 (simple-img-area simple-img1)) (sqrt 3)))
               "solid" (simple-img-color simple-img1))]))







(define-struct diff (start ins del))




(define (fn-for-diff diff)
  (...
   (diff-start diff)
   (diff-ins diff)
   (diff-del diff)))

(define diff1 (make-diff 1 "xyzzy" 0))
(define diff2 (make-diff 3 "" 2))











(check-expect (update diff1 "0123456789") "0xyzzy123456789")
(check-expect (update diff2 "0123456789") "01256789")








(define (update diff str)
  (updateL diff (string->list str)))






(define (updateL diff loc)
  (cond
    [(> (diff-start diff) 0)
     (string-append (string (first loc))
                    (updateL (make-diff (- (diff-start diff) 1)
                                        (diff-ins diff) (diff-del diff))
                             (rest loc)))]
    [(> (diff-del diff) 0)
     (delete (diff-del diff) (diff-ins diff) loc)]
    [else
     (insert (diff-ins diff) loc)]))





(define (delete del str loc)
  (if (> del 0)
      (delete (- del 1) str (rest loc))
      (insert str loc)))








(define (insert str loc)
  (string-append str (list->string loc)))









(define diff3 (make-diff 4 "were" 3))
(define diff4 (make-diff 13 "yesterday" 5))
"Diffs question 3:"
(update diff4 (update diff3 "How are you today?"))










(define (fn-for-loi loi)
  (cond
    [(empty? loi) (...)]
    [else
     (... (first loi)
          (fn-for-loi (rest loi)))]))






 
(define image1 (rectangle 50 100 "solid" "green"))
(define image2 (rectangle 75 50 "solid" "blue"))
(define image3 (rectangle 200 20 "solid" "red"))
(define list1 (list image1 image2 image3))
(define list2 (list image3 image1))



(check-expect (total-width empty) 0)
(check-expect (total-width list1) 325)
(check-expect (total-width list2) 250)








(define (total-width loi)
  (cond
    [(empty? loi) 0]
    [else
     (+ (image-width (first loi)) (total-width (rest loi)))]))











(check-expect (taller-than 5 empty) empty-image)
(check-expect (taller-than 100 list1) empty-image)
(check-expect (taller-than 30 list1) (beside image1 image2))








(define (taller-than height loi)
  (cond
    [(empty? loi) empty-image]
    [(> (image-height (first loi)) height)
     (beside (first loi) (taller-than height (rest loi)))]
    [else
     (taller-than height (rest loi))]))












(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (first los)
          (fn-for-los (rest los)))]))











(define SI4 (make-simple-img "circle" "blue" 500))

(define los1 (list SI1 SI2 SI3 SI4))
(define los2 (list SI2 SI1))



(check-expect (find-shape "circle" los2) empty-image)
(check-expect (find-shape "circle" los1) (beside (build-image SI3) (build-image SI4)))
(check-expect (find-shape "square" los2) (build-image SI2))








(define (find-shape shape los)
  (cond
    [(empty? los) empty-image]
    [(equal? (simple-img-shape (first los)) shape)
     (beside (build-image (first los)) (find-shape shape (rest los)))]
    [else
     (find-shape shape (rest los))]))













(define (fn-for-lod lod)
  (cond
    [(empty? lod) (...)]
    [else
     (... (first lod)
          (fn-for-lod (rest lod)))]))




















(define lod1 (list diff2 diff1))
(define lod2 (list diff3 diff4))

(check-expect (updates "0123456789" lod1) "0xyzzy1256789")
(check-expect (updates "How are you today?" lod2) "How were you yesterday?")








(define (updates str lod)
  (cond
    [(empty? lod) str]
    [else
     (updates (update (first lod) str) (rest lod))])) 
              






