

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Kai Nakamura Assignment 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))










(check-expect (backwards "") "")
(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "hello") "olleh")

(define (backwards string) "") 













(define-struct customer (first-name last-name age zip-code))

(define EMPTY-CUSTOMER (make-customer "" "" 0 0))
(define ISAAC (make-customer "Isaac" "Lau" 18 02035))
(define KAI (make-customer "Kai" "Nakamura" 18 98042))

 









(check-expect (birthday ISAAC)
              (make-customer "Isaac" "Lau" (+ 1 18) 02035))
(check-expect (birthday KAI)
              (make-customer "Kai" "Nakamura" (+ 1 18) 98042))

 



(define (birthday customer)
  (make-customer (customer-first-name customer)
                 (customer-last-name customer)
                 (+ 1 (customer-age customer))
                 (customer-zip-code customer)))







(check-expect (name-change ISAAC "John" "Doe")
              (make-customer "John" "Doe" 18 02035))
(check-expect (name-change KAI "Nakamura" "Kai")
              (make-customer "Nakamura" "Kai" 18 98042))

 



(define (name-change customer first-name last-name)
  (make-customer first-name
                 last-name
                 (customer-age customer)
                 (customer-zip-code customer)))


(require 2htdp/image)







 












(define-struct simple-img (shape color area))






(define BLANK-SIMPLE-IMAGE (make-simple-img "square" "white" 0))
(define BLUE-TRIANGLE-100 (make-simple-img "triangle" "blue" 100))
(define BLUE-TRIANGLE-200 (make-simple-img "triangle" "blue" 200))
(define GREEN-SQUARE-100 (make-simple-img "square" "green" 100))
(define GREEN-SQUARE-200 (make-simple-img "square" "green" 200))
(define YELLOW-CIRCLE-100 (make-simple-img "circle" "yellow" 100))
(define YELLOW-CIRCLE-200 (make-simple-img "circle" "yellow" 200))
(define SI (make-simple-img "triangle" "red" 250))
(define SI1 (make-simple-img "circle" "red" 120))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI3 (make-simple-img "circle" "purple" 40))

 











(check-expect (bigger? BLANK-SIMPLE-IMAGE SI) false)
(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI SI) false)

 




(define (bigger? simple-img-1 simple-img-2)
  (> (simple-img-area simple-img-1)
     (simple-img-area simple-img-2)))










(check-expect (build-image BLANK-SIMPLE-IMAGE) (square 0 "solid" "white"))
(check-expect (build-image SI) .)
(check-expect (build-image SI2) (square (sqrt 200) "solid" "purple"))

 




(define (build-image simple-img)
  (cond [(string=? "triangle" (simple-img-shape simple-img))
         (build-triangle-image simple-img)]
        [(string=? "square" (simple-img-shape simple-img))
         (build-square-image simple-img)]
        [(string=? "circle" (simple-img-shape simple-img))
         (build-circle-image simple-img)]))








(check-expect (build-triangle-image BLANK-SIMPLE-IMAGE)
              (triangle 0 "solid" "white"))
(check-expect (build-triangle-image SI)
              (triangle (sqrt (* (/ 4 (sqrt 3)) 250))
                        "solid"
                        "red"))
(check-expect (build-triangle-image BLUE-TRIANGLE-100)
              (triangle (sqrt (* (/ 4 (sqrt 3)) 100))
                        "solid"
                        "blue"))
(check-expect (build-triangle-image BLUE-TRIANGLE-200)
              (triangle (sqrt (* (/ 4 (sqrt 3)) 200))
                        "solid"
                        "blue"))

 




(define (build-triangle-image simple-img)
  (triangle (sqrt (* (/ 4 (sqrt 3))
                     (simple-img-area simple-img)))
            "solid"
            (simple-img-color simple-img)))








(check-expect (build-square-image BLANK-SIMPLE-IMAGE)
              (square 0 "solid" "white"))
(check-expect (build-square-image GREEN-SQUARE-100)
              (square (sqrt 100) "solid" "green"))
(check-expect (build-square-image GREEN-SQUARE-200)
              (square (sqrt 200) "solid" "green"))

 




(define (build-square-image simple-img)
  (square (sqrt (simple-img-area simple-img))
          "solid"
          (simple-img-color simple-img)))









(check-expect (build-circle-image BLANK-SIMPLE-IMAGE)
              (circle 0 "solid" "white"))
(check-expect (build-circle-image YELLOW-CIRCLE-100)
              (circle (sqrt (/ 100 pi)) "solid" "yellow"))
(check-expect (build-circle-image YELLOW-CIRCLE-200)
              (circle (sqrt (/ 200 pi)) "solid" "yellow"))

 




(define (build-circle-image simple-img)
  (circle (sqrt (/ (simple-img-area simple-img) pi))
          "solid"
          (simple-img-color simple-img)))








 










(define-struct diff (position data))

(define EMPTY-INSERTION (make-diff 0 ""))
(define EMPTY-DELETION (make-diff 0 0))
(define INSERTION-1 (make-diff 1 "xyzzy"))
(define INSERTION-2 (make-diff 2 "abcde"))
(define DELETION-1 (make-diff 3 2))
(define DELETION-2 (make-diff 2 3))

 
























(check-expect (insert EMPTY-INSERTION "") "")
(check-expect (insert INSERTION-1 "0123456789") "0xyzzy123456789")
(check-expect (insert INSERTION-2 "0123456789") "01abcde23456789")

 




(define (insert diff string)
  (string-append (substring string 0 (diff-position diff))
                 (diff-data diff)
                 (substring string (diff-position diff)
                            (string-length string))))








(check-expect (delete EMPTY-DELETION "") "")
(check-expect (delete DELETION-1 "0123456789") "01256789")
(check-expect (delete DELETION-2 "0123456789") "0156789")

 




(define (delete diff string)
  (string-append (substring string 0 (diff-position diff))
                 (substring string (+ (diff-data diff)
                                      (diff-position diff))
                            (string-length string))))







(check-expect (update EMPTY-INSERTION "") "")
(check-expect (update EMPTY-DELETION "") "")
(check-expect (update INSERTION-1 "0123456789") "0xyzzy123456789")
(check-expect (update INSERTION-2 "0123456789") "01abcde23456789")
(check-expect (update DELETION-1 "0123456789") "01256789")
(check-expect (update DELETION-2 "0123456789") "0156789")
(check-expect (update (make-diff 13 "yesterday")
                      (update (make-diff 13 5)
                              (update (make-diff 4 "were")
                                      (update (make-diff 4 3)
                                              "How are you today?"))))
              "How were you yesterday?")

 




(define (update diff string)
  (cond [(string? (diff-data diff)) (insert diff string)]
        [(integer? (diff-data diff)) (delete diff string)]))





(update (make-diff 13 "yesterday")
        (update (make-diff 13 5)
                (update (make-diff 4 "were")
                        (update (make-diff 4 3) "How are you today?"))))











(define BLANK-IMAGE (square 0 "solid" "white"))
(define LOI-0 (list BLANK-IMAGE))
(define LOI-1 (list (rectangle 10 20 "solid" "red")))
(define LOI-2 (list (rectangle 10 20 "solid" "red")
                    (circle 10 "solid" "blue")))
(define LOI-3 (list (rectangle 10 30 "solid" "red")
                    (circle 10 "solid" "blue")
                    (triangle 15 "solid" "green")))

(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))

 












(check-expect (total-width empty) 0)
(check-expect (total-width LOI-0) 0)
(check-expect (total-width LOI-1) 10)
(check-expect (total-width LOI-2) (+ 10 20))
(check-expect (total-width LOI-3) (+ 10 20 15))
(check-expect (total-width LOI) 265)
(check-expect (taller-than LOI 30)
              (list . .))

 






(define (total-width loi)
  (cond [(empty? loi) 0]
        [else
         (+ (image-width (first loi))
            (total-width (rest loi)))]))







(check-expect (taller-than empty -1) empty)
(check-expect (taller-than LOI-0 0) empty)
(check-expect (taller-than LOI-3 0) LOI-3)
(check-expect (taller-than LOI-3 100) empty)
(check-expect (taller-than LOI-3 20) (list (rectangle 10 30 "solid" "red")))
(check-expect (taller-than LOI-3 15) (list (rectangle 10 30 "solid" "red")
                                           (circle 10 "solid" "blue")))

 






(define (taller-than loi height)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi))
            height)
         (cons (first loi)
               (taller-than (rest loi) height))]
        [else (taller-than (rest loi) height)]))







(define LOSI-1 (list BLUE-TRIANGLE-100))
(define LOSI-2 (list BLUE-TRIANGLE-100
                     GREEN-SQUARE-100))
(define LOSI-3 (list BLUE-TRIANGLE-100
                     GREEN-SQUARE-100
                     YELLOW-CIRCLE-100))
(define LOSI-4 (list BLUE-TRIANGLE-100
                     GREEN-SQUARE-100
                     YELLOW-CIRCLE-100
                     BLUE-TRIANGLE-200))
(define LOSI-5 (list BLUE-TRIANGLE-100
                     GREEN-SQUARE-100
                     YELLOW-CIRCLE-100
                     BLUE-TRIANGLE-200
                     GREEN-SQUARE-200))
(define LOSI-6 (list BLUE-TRIANGLE-100
                     GREEN-SQUARE-100
                     YELLOW-CIRCLE-100
                     BLUE-TRIANGLE-200
                     GREEN-SQUARE-200
                     YELLOW-CIRCLE-200))

 













(check-expect (find-shape empty "circle") empty)
(check-expect (find-shape LOSI-1 "triangle")
              (list (build-image BLUE-TRIANGLE-100)))
(check-expect (find-shape LOSI-1 "square") empty)
(check-expect (find-shape LOSI-6 "triangle")
              (list (build-image BLUE-TRIANGLE-100)
                    (build-image BLUE-TRIANGLE-200)))
(check-expect (find-shape LOSI-6 "square")
              (list (build-image GREEN-SQUARE-100)
                    (build-image GREEN-SQUARE-200)))
(check-expect (find-shape LOSI-6 "circle")
              (list (build-image YELLOW-CIRCLE-100)
                    (build-image YELLOW-CIRCLE-200)))
(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))

 







(define (find-shape losi shape)
  (cond [(empty? losi) empty]
        [(string=? (simple-img-shape (first losi))
                   shape)
         (cons (build-image (first losi))
               (find-shape (rest losi) shape))]
        [else (find-shape (rest losi) shape)]))







(define LOD-0 (list EMPTY-INSERTION
                    EMPTY-DELETION))
(define LOD-1 (list INSERTION-1))
(define LOD-2 (list INSERTION-1
                    DELETION-1))
(define LOD-3 (list INSERTION-1
                    DELETION-1
                    INSERTION-2))
(define LOD-4 (list INSERTION-1
                    DELETION-1
                    INSERTION-2
                    DELETION-2))

 













(check-expect (updates LOD-0 "") "")
(check-expect (updates LOD-0 "0123456789") "0123456789")
(check-expect (updates LOD-1 "0123456789")
              (update INSERTION-1 "0123456789"))
(check-expect (updates LOD-2 "0123456789")
              (update DELETION-1
                      (update INSERTION-1 "0123456789")))
(check-expect (updates LOD-3 "0123456789")
              (update INSERTION-2
                      (update DELETION-1
                              (update INSERTION-1 "0123456789"))))
(check-expect (updates LOD-4 "0123456789")
              (update DELETION-2
                      (update INSERTION-2
                              (update DELETION-1
                                      (update INSERTION-1 "0123456789")))))
(check-expect (updates (list (make-diff 4 3)
                             (make-diff 4 "were")
                             (make-diff 13 5)
                             (make-diff 13 "yesterday"))
                       "How are you today?")
              (update (make-diff 13 "yesterday")
                      (update (make-diff 13 5)
                              (update (make-diff 4 "were")
                                      (update (make-diff 4 3)
                                              "How are you today?")))))

 







(define (updates lod string)
  (cond [(empty? lod) string]
        [else
         (updates (rest lod)
                  (update (first lod) string))]))