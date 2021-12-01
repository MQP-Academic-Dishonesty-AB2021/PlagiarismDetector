

#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment_1_Daniel_Jonathan) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)



(define (backwards string) "")







(define-struct customer (fname lname age zipcode))






(define (fn-for-customer customer)
  (... (customer-fname customer)
  (customer-lname customer)
  (customer-age customer)
  (customer-zipcode customer))
)




(check-expect (birthday (make-customer "a" "b" 1 111111)) (make-customer "a" "b" (+ 1 1) 111111))
(check-expect (birthday (make-customer "b" "a" -1 111111)) (make-customer "b" "a" (+ 1 -1) 111111))
(check-expect (birthday (make-customer "" "" 0 000000)) (make-customer "" "" (+ 1 0) 000000))

(define (birthday customer)
  (make-customer (customer-fname customer)
  (customer-lname customer)
  (+ 1 (customer-age customer))
  (customer-zipcode customer))
)





(check-expect (name-change (make-customer "a" "b" 0 000000) "g" "h") (make-customer "g" "h" 0 000000))
(check-expect (name-change (make-customer "a" "b" 0 000000) "" "") (make-customer "" "" 0 000000))
(check-expect (name-change (make-customer "a" "b" 0 000000) "a" "b") (make-customer "a" "b" 0 000000))
(check-expect (name-change (make-customer "" "" 12 000100) "a" "b") (make-customer "a" "b" 12 000100))


(define (name-change customer newfn newln)
  (make-customer newfn newln
  (customer-age customer)
  (customer-zipcode customer))
)










(require 2htdp/image)










(define-struct simple-image (shape color area))
 






(define SI (make-simple-image "triangle" "red" 250))
(define SI1 (make-simple-image "circle" "red" 120))
(define SI2 (make-simple-image "square" "purple" 200))

(define SI3 (make-simple-image "circle" "purple" 40))
(define SI4 (make-simple-image "trapazoid" "purple" 4000))


(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false) 







(define (bigger? img1 img2)
  (> (simple-image-area img1) (simple-image-area img2))
)






(check-expect (build-image SI) .)
(check-expect (build-image SI2) .)
(check-expect (build-image SI3) .)









(define (build-image img)
  (cond
    [(string=? (simple-image-shape img) "square") (square (sqrt (simple-image-area img)) "solid" (simple-image-color img))]
    [(string=? (simple-image-shape img) "circle") (circle (sqrt (/ (simple-image-area img) pi)) "solid" (simple-image-color img))]
    [(string=? (simple-image-shape img) "triangle") (triangle (* (* (/ 2 3) (sqrt (sqrt(expt 3 3)))) (sqrt (simple-image-area img))) "solid" (simple-image-color img))]
  )
)










(define-struct diff (change index))

(define (fn-for-diff diff)
  (... (diff-change diff)
       (diff-index diff)
  )
)






(check-expect (update "0123456789" (make-diff "xyzzy" 1)) "0xyzzy123456789")
(check-expect (update "0123456789" (make-diff 2 3)) "01256789")


(define (update string diff)
  (if (string? (diff-change diff)) 
       (string-append (substring string 0 (diff-index diff)) (diff-change diff) (substring string (diff-index diff)))      
       (string-append (substring string 0 (diff-index diff)) (substring string (+ (diff-index diff) (diff-change diff))))  

  )
)



(define str "How are you today?")

(define diff-1 (make-diff 1 4)) 


(define diff-2 (make-diff "we" 4)) 


(define diff-3 (make-diff 1 14)) 


(define diff-4 (make-diff "yes" 13)) 


(define diff-5 (make-diff "er" 17)) 




(update (update (update (update (update str diff-1) diff-2) diff-3) diff-4) diff-5)












(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))




(check-expect (total-width LOI) 265) 

(check-expect (taller-than LOI 30)
              (list . .))













(define (fn-for-LoI Loi)
  (cond [(empty? Loi) ...]
        [else (... (first Loi) (fn-for-Loi (rest Loi)))]
   )
)






(check-expect (total-width empty) 0)
(check-expect (total-width (cons (square 10 "solid" "red") (cons (triangle 14 "solid" "blue") empty))) (+ (image-width (square 10 "solid" "red"))
                                                                                                          (image-width (triangle 14 "solid" "blue"))
                                                                                                       ))
(check-expect (total-width (cons (square 11.4 "solid" "red") empty)) (image-width (square 11.4 "solid" "red")))
(check-expect (total-width (cons (circle 20 "solid" "red") empty)) (image-width (circle 20 "solid" "red")))


(define (total-width Loi)
  (cond [(empty? Loi) 0]
        [else (+ (image-width (first Loi)) (total-width (rest Loi)))]
   )
)






(check-expect (taller-than empty 0) empty)

(check-expect (taller-than (cons (square 10 "solid" "red") (cons (triangle 14 "solid" "blue") empty)) 0) (cons (square 10 "solid" "red") (cons (triangle 14 "solid" "blue") empty)))

(check-expect (taller-than (cons (square 10 "solid" "red") (cons (triangle 14 "solid" "blue") empty)) 10) (cons (triangle 14 "solid" "blue") empty))


(define (taller-than Loi cutoff)
  (cond [(empty? Loi) empty]
        [else (if (> (image-height (first Loi)) cutoff) (cons (first Loi) (taller-than (rest Loi) cutoff))
                   (taller-than (rest Loi) cutoff))]
   )
)





(define (fn-for-losi losi)
  (cond [(empty? losi) ...]
        [else (... (first losi) (fn-for-losi (rest losi)))]
  )
)






(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))

(check-expect (build-image SI1) .)

(check-expect (find-shape (list SI SI1 SI2 SI3) "square")
              (list .))

(check-expect (find-shape empty "square")
             empty)
(check-expect (find-shape (list SI SI1 SI2 SI3) "squire")
              empty)


(define (find-shape losi shape)
  (cond [(empty? losi) empty]
        [else (if (string=? (simple-image-shape (first losi)) shape)
                  (cons (build-image (first losi)) (find-shape (rest losi) shape))
                  (find-shape (rest losi) shape))]
  )
)





(define (fn-for-Lod Lod)
        (cond [(empty? Lod) ...]
              [else (... (first Lod) (fn-for-Lod (rest Lod)))]
        )
)





(check-expect (updates empty "") "")
(check-expect (updates (list (make-diff 2 3) (make-diff "xyzzy" 1)) "0123456789") "0xyzzy1256789")
(check-expect (updates (list diff-1 diff-2 diff-3 diff-4 diff-5) "How are you today?") "How were you yesterday?")

(define (updates Lod string)
        (cond [(empty? Lod) string]
              [else (updates (rest Lod) (update string (first Lod)))]
        )
)

