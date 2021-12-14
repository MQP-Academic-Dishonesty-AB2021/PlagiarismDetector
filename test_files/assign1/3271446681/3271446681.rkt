

#reader(lib "htdp-beginner-reader.ss" "lang")((modname |assigment 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))









(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "a") "a")
(check-expect (backwards "") "")

(define (backwards text) "")





(define-struct customer (fristName lastName age zipCode))








 

(define jim (make-customer "jim" "johnson" 45 54367))
(define kimmy (make-customer "kimmy" "danils" 23 98703))






(check-expect (brithday jim)
              (make-customer
               (customer-fristName jim)
               (customer-lastName jim)
               (+ (customer-age jim) 1)
               (customer-zipCode jim)
               ))
(check-expect (brithday kimmy)
              (make-customer
               (customer-fristName kimmy)
               (customer-lastName kimmy)
               (+ (customer-age kimmy) 1)
               (customer-zipCode kimmy)
               ))



(define (brithday aCustomer)
  (make-customer
   (customer-fristName aCustomer)
   (customer-lastName aCustomer)
   (+ (customer-age aCustomer) 1)
   (customer-zipCode aCustomer)
   )
  )






(check-expect (name-change jim "steve" "berks")
              (make-customer
               "steve"
               "berks"
               (customer-age jim)
               (customer-zipCode jim)
               )
              )
(check-expect (name-change jim "jamie" "kennity")
              (make-customer
               "jamie"
               "kennity"
               (customer-age jim)
               (customer-zipCode jim)
               )
              )


 

(define (name-change aCustomer newFN newLN)
  (make-customer
   newFN
   newLN
   (customer-age aCustomer)
   (customer-zipCode aCustomer)
   )
  )



(require 2htdp/image)


(define-struct simple-img (shape area color))







 





(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)



(define (bigger? SI SI2)
  (> (simple-img-area SI) (simple-img-area SI2)))


(define SI (make-simple-img "triangle" 250 "red"))
(define SI2 (make-simple-img "square" 200 "purple"))
(define simple-img3 (make-simple-img "circle" 100 "purple"))
(define nonsense (make-simple-img "rectangle" -2 "apple"))

(check-expect (build-image SI)
              (triangle (sqrt (/(* (simple-img-area SI) 4) (sqrt 3))) "solid" (simple-img-color SI))
              )
(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))
(check-expect (build-image simple-img3)
              (circle (sqrt (/ (simple-img-area simple-img3) pi)) "solid" (simple-img-color simple-img3))
              )
(check-expect (build-image nonsense) "huh?")



(define (build-image simp)
  (cond [(equal? (simple-img-shape simp) "circle") (circ simp)]
        [(equal? (simple-img-shape simp) "square") (box simp)]
        [(equal? (simple-img-shape simp) "triangle") (tri simp)]
        [else "huh?"]))




(define (circ si)
  (circle (sqrt (/ (simple-img-area si) pi)) "solid" (simple-img-color si)))




(define (box si)
  (square (sqrt (simple-img-area si)) "solid" (simple-img-color si)))




(define (tri si)
  (triangle (sqrt (/(* (simple-img-area si) 4) (sqrt 3))) "solid" (simple-img-color si)))




(require racket/string) 

(define-struct diff (change pos))
















 

(define dealte-2-3 (make-diff 3 2))
(define addER (make-diff "er " 4))





(check-expect (update "cooljack" dealte-2-3)
              (string-replace
               "cooljack"
               (substring "cooljack" 2 5)
               ""
               )
              )
(check-expect (update "cooljack" addER)
              (string-append
               (substring "cooljack" 0 (diff-pos addER))
               (diff-change addER)
               (substring "cooljack" (diff-pos addER ) (string-length "cooljack"))
               )
              )
(check-expect (update "0123456789" (make-diff "xyzzy" 1)) "0xyzzy123456789" )
(check-expect (update "0123456789" (make-diff 2 3)) "01256789" )
              




(define (update text diff)
  (if (string? (diff-change diff))
      
      (string-append
       (substring text 0 (diff-pos diff))
       (diff-change diff)
       (substring text (diff-pos diff) (string-length text))
       )
      
      (string-replace
       text
       (substring text (diff-pos diff) (last-index diff))
       ""
       )
      )
  
  )





(define (last-index diff)
  (+ (diff-pos diff) (diff-change diff))
  )









 




(check-expect (total-width empty) 0)

(check-expect (total-width
               (list (square 5 "solid" "blue") (square 10 "solid" "blue"))
               )
              (+ 5 10)
              )
(check-expect (total-width
               (list (circle 5 "solid" "blue") (circle 10 "solid" "blue"))
               )
              (+ (* 5 2) (* 10 2))
              )



(define (total-width loi)
  (cond [(empty? loi) 0]
        [else
         (+ (image-width (first loi))
            (total-width (rest loi)))
         ]
        )
  )





(check-expect (taller-than empty 0) empty)

(check-expect (taller-than(list (square 5 "solid" "blue") (square 10 "solid" "blue")) 7)
              (list (square 10 "solid" "blue")))
(check-expect (taller-than (list (circle 20 "solid" "blue") (circle 10 "solid" "blue")) 22)
              (list (circle 20 "solid" "blue")))
(check-expect (taller-than (list (circle 20 "solid" "blue") (circle 10 "solid" "blue")) 5)
              (list (circle 20 "solid" "blue") (circle 10 "solid" "blue")))



(define (taller-than loi cutOff)
  (cond [(empty? loi) empty]
        [(> (image-width (first loi)) cutOff)
         (cons (first loi) (taller-than (rest loi) cutOff))
         ]
        [else (taller-than (rest loi) cutOff)]
        )
  )













 





(define SI3 (make-simple-img "square" 200 "purple"))
(define SI4 (make-simple-img "circle" 50 "red"))
(define SI5 (make-simple-img "triangle" 100 "blue"))

(define ex1 (make-simple-img "circle" 120 "red"))
(define ex2 (make-simple-img "circle" 40 "purple"))

(check-expect (find-shape (list SI3 SI4 SI5) "square") (list .))
(check-expect (find-shape (list SI3 SI4 SI5 SI4) "circle") (list . .))
(check-expect (find-shape (list SI3 SI4 SI5) "triangle") (list .))
(check-expect (find-shape (list SI3 SI4 SI3) "triangle") empty)
(check-expect (find-shape (list SI3 ex1 SI5 ex2) "circle")(list . .))



(define (find-shape losi shapeType)
  (cond [(empty? losi) empty]
        [(string=? (simple-img-shape (first losi)) shapeType )
         (cons (build-image (first losi)) (find-shape (rest losi) shapeType))
         ]
        [else
         (find-shape (rest losi) shapeType)
         ]
        )
  )















 







(define DEL (make-diff 2 3))
(define INS (make-diff "xyzzy" 1))

(check-expect (updates (list DEL INS) "0123456789") "0xyzzy1256789")
(check-expect (updates (list DEL INS DEL DEL) "How were you yesterday?")
              "Hxywere u yesterday?")
(check-expect (updates empty "How were you yesterday?") "How were you yesterday?")





(define (updates lod text)
  (cond
    [(empty? lod) text]
    [else (updates (rest lod) (update text (first lod)) )]
    )
  )
