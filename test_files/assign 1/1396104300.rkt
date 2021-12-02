

#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))









(check-expect (backwards "") "")
(check-expect (backwards "racecar") "racecar")
(check-expect (backwards "cat") "tac")
(check-expect (backwards "corn") "nroc")

(define (backwards string) string)










(define-struct customer (first last age zipcode))

(define TIM (make-customer "Tim" "Allen" 27 90210))

   







(check-expect (birthday (make-customer "Jane" "Doe" 0 65474))
              (make-customer "Jane" "Doe" (+  1 0) 65474))

(check-expect (birthday TIM)
              (make-customer (customer-first TIM)
                             (customer-last TIM)
                             (+ (customer-age TIM) 1)
                             (customer-zipcode TIM)))

 

(define (birthday customer)
  (make-customer (customer-first customer)
                 (customer-last customer)
                 (+ 1 (customer-age customer))
                 (customer-zipcode customer)))









(define CHARLES (make-customer "Charles" "McMaster" 91 12345))
(define ELIZABETH (make-customer "Queen" "Elizabeth" 92 00000))

(check-expect (name-change (make-customer "Fran" "Doe" 54 65784) "Jane" "Doe")
              (make-customer "Jane" "Doe" 54 65784))
(check-expect (name-change CHARLES "Frank" "Herbert")
              (make-customer "Frank" "Herbert" 91 12345))
(check-expect (name-change ELIZABETH "Emperor" "Elizabeth")
              (make-customer "Emperor" "Elizabeth"
                             (customer-age ELIZABETH)
                             (customer-zipcode ELIZABETH)))
(check-expect (name-change TIM "Tim" "Allen") TIM)
(check-expect (name-change CHARLES "" "")
              (make-customer "" ""
                             (customer-age CHARLES)
                             (customer-zipcode CHARLES)))

 

(define (name-change customer first last)
  (make-customer first
                 last
                 (customer-age customer)
                 (customer-zipcode customer)))












(require 2htdp/image)




(define-struct simple-img (color shape area))







(define SI (make-simple-img "red" "triangle" 250))
(define SI2 (make-simple-img "purple" "square" 200))

   










(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI SI) false)

 

(define (bigger? lhs rhs)
  (> (simple-img-area lhs) (simple-img-area rhs)))









(define MODE "solid")
(define SICirc (make-simple-img "white" "circle" 140))
(define SITri (make-simple-img "red" "triangle" 250))
(define SIempty (make-simple-img "pink" "square" 0))




(check-expect (build-image SI) .)

(check-expect (build-image SI2)
              (square (sqrt (simple-img-area SI2)) MODE (simple-img-color SI2)))
(check-expect (build-image SICirc)
              (circle (sqrt (/ (simple-img-area SICirc) pi))
                      MODE
                      (simple-img-color SICirc)))
(check-expect (build-image SITri)
              (triangle (sqrt (/ (simple-img-area SITri) (/ (sqrt 3) 4)))
                        MODE
                        (simple-img-color SITri)))
(check-expect (build-image SIempty) (square 0 MODE (simple-img-color SIempty)))
 

(define (build-image img)
  (cond
    [(string=? (simple-img-shape img) "triangle")
     (triangle
      (triarea->side (simple-img-area img))
      MODE
      (simple-img-color img))]
    [(string=? (simple-img-shape img) "circle")
     (circle
      (area->rad (simple-img-area img))
      MODE
      (simple-img-color img))]
    [(string=? (simple-img-shape img) "square")
     (square
      (sqarea->side (simple-img-area img))
      MODE
      (simple-img-color img))]))





(check-expect (triarea->side 0) 0)
(check-within (triarea->side 80) (sqrt (/ 80 (/ (sqrt 3) 4))) 0.0000001)

 

(define (triarea->side area)
  (sqrt (/ area (/ (sqrt 3) 4))))





(check-expect (area->rad 0) 0)
(check-within (area->rad 80) (sqrt (/ 80 pi)) 0.0000001)

 


(define (area->rad area)
  (sqrt (/ area pi)))





(check-expect (sqarea->side 0) 0)
(check-within (sqarea->side 80) (sqrt 80) 0.0000001)

 


(define (sqarea->side area)
  (sqrt area))















(define-struct diff (position delta))




(define-struct insert (data))


(define-struct delete (num))



 
 
 





(check-expect (update "0123456789" (make-diff 1 (make-insert "xyzzy")))
              (string-append "0" "xyzzy" "123456789"))
(check-expect (update "0123456789" (make-diff 3 (make-delete 2)))
              (string-append (substring "0123456789" 0 3)
                             (substring "0123456789" (+ 3 2)
                                        (string-length "0123456789"))))
(check-expect (update "" (make-diff 0 (make-insert "this is a long string")))
              "this is a long string")
(check-expect (update "crayon" (make-diff 0 (make-delete 6))) "")
(check-expect (update "cran" (make-diff 4 (make-insert "berry"))) "cranberry")
(check-expect (update "berry" (make-diff 0 (make-insert "blue"))) "blueberry")
(check-expect (update "racecar" (make-diff 0 (make-delete 4))) "car")

 

(define (update string diff)
  (cond [(insert? (diff-delta diff))
         (diff-insert diff string)]
        [(delete? (diff-delta diff))
         (diff-delete diff string)]))






(check-expect (diff-insert (make-diff 1 (make-insert "bcd")) "aef")
              (string-append "a" "bcd" "ef"))
(check-expect (diff-insert (make-diff 0 (make-insert "cat")) " in the hat")
              (string-append "cat" " in the hat"))
(check-expect (diff-insert (make-diff 7 (make-insert "append")) "string-" )
              (string-append "string-" "append"))

 

(define (diff-insert diff string)
  ( string-append (substring string 0 (diff-position diff))
                  (insert-data (diff-delta diff))
                  (substring string
                             (diff-position diff)
                             (string-length string))))







(check-expect (diff-delete (make-diff 0 (make-delete 9)) "chocolate")
              "")
(check-expect (diff-delete (make-diff 4 (make-delete 2)) "Europa")
              (substring "Europa" 0 4))
(check-expect (diff-delete (make-diff 0 (make-delete 0)) "racecar")
              "racecar")

 

(define (diff-delete diff string)
  (string-append (substring string 0 (diff-position diff))
                 (substring string
                            (+ (diff-position diff)
                               (delete-num (diff-delta diff)))
                            (string-length string))))


(define START-STRING "How are you today?")
(define END-STRING "How were you yesterday?")

(define DEL1 (make-diff 13 (make-delete 1)))
(define DEL2 (make-diff 4 (make-delete 1)))

(define INS1 (make-diff 4 (make-insert "we")))
(define INS2 (make-diff 13 (make-insert "yes")))
(define INS3 (make-diff 17 (make-insert "er")))


(check-expect (update
               (update
                (update
                 (update
                  (update START-STRING DEL1)
                  DEL2)
                 INS1)
                INS2)
               INS3)
              END-STRING)


(check-expect (apply (list DEL1 DEL2 INS1 INS2 INS3) START-STRING) END-STRING)





(define EXDIFF1 (make-diff 0 (make-delete 4)))
(define EXDIFF2 (make-diff 0 (make-insert "cran")))
(define EXDIFF3 (make-diff 0 (make-insert "Irish ")))
(define EXDIFF4 (make-diff (+
                            (string-length "Irish ")
                            (string-length "potato"))
                           (make-insert " famine")))
(define EXDIFF5 (make-diff 0 (make-delete (string-length "Welcome "))))
(define EXDIFF6 (make-diff (string-length "new")
                           (make-delete (string-length " students!"))))
  
(check-expect (apply (list EXDIFF1 EXDIFF2) "blueberry")
              (update (update "blueberry" EXDIFF1) EXDIFF2))
(check-expect (apply empty "printer") "printer")
(check-expect (apply (list EXDIFF3 EXDIFF4) "potato")
              (update (update "potato" EXDIFF3) EXDIFF4))
(check-expect (apply (list EXDIFF5 EXDIFF6) "Welcome new students!")
              (update (update "Welcome new students!" EXDIFF5) EXDIFF6))

 

(define (apply lod string)
  (if (empty? lod)
      string
      (apply (rest lod) (update string (first lod)))))











(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))









(define EXLOI (cons (circle 50 "solid" "white")
                    (cons (square 25 "solid" "blue") empty)))

(define EXLOI2 empty)

 







(check-expect (total-width EXLOI)
              (+ (image-width (first LOI))
                 (image-width (first (rest LOI)))))
(check-expect (total-width EXLOI2) 0)

(check-expect (total-width LOI) 265) 
 

(define (total-width loi)
  (cond [(empty? loi) 0]
        [else (+ (image-width (first loi)) (total-width (rest loi)))]))







(define EXSQ1 (square 10 "solid" "white"))

(define EXSQ3 (square 30 "solid" "white"))
(define EXSQ4 (square 40 "solid" "white"))
(define EXSQ5 (square 50 "solid" "white"))
(define EXSQ6 (square 60 "solid" "white"))

(define EXSQ8 (square 80 "solid" "white"))

(check-expect (taller-than (list EXSQ1 EXSQ8 EXSQ3) 50) (list EXSQ8))
(check-expect (taller-than empty 12) empty)
(check-expect (taller-than (list EXSQ4 EXSQ5 EXSQ6) 90) empty)
(check-expect (taller-than (list EXSQ4 EXSQ5 EXSQ6) 10)
              (list EXSQ4 EXSQ5 EXSQ6))



(check-expect (taller-than LOI 30)
              (list . .))

 

(define (taller-than loi limit)
  (cond [(empty? loi) empty]
        [(> (image-height (first loi)) limit)
         (cons (first loi) (taller-than (rest loi) limit))]
        [else (taller-than (rest loi) limit)]))





























(define LOSI1 empty)
(define LOSI2 (cons SI (cons SI2 empty)))

(define (fn-for-losi losi)
  (cond
    [(empty? losi) (...)]
    [else (... (fn-for-shape (first losi)) (fn-for-losi (rest losi)))]))









(define SI1 (make-simple-img "red" "circle" 120))
(define SI3 (make-simple-img "purple" "circle" 40))

(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
(check-expect (find-shape empty "triangle") empty)
(check-expect (find-shape (list SI1 SI3) "square") empty)
(check-expect (find-shape (list SICirc SI1 SI3) "circle")
              (list (build-image SICirc) (build-image SI1) (build-image SI3)))

 

(define (find-shape losi shape)
  (cond [(empty? losi) empty]
        [(string=? (simple-img-shape (first losi)) shape)
         (cons (build-image (first losi)) (find-shape (rest losi) shape))]
        [else (find-shape (rest losi) shape)]))




































































 





(define DEL (make-diff 3 (make-delete 2)))
(define INS (make-diff 1 (make-insert "xyzzy")))

(define EXSTART-STRING "How are you today?")
(define EXEND-STRING "How were you yesterday?")
(define EXDEL1 (make-diff 13 (make-delete 1)))
(define EXDEL2 (make-diff 4 (make-delete 1)))
(define EXINS1 (make-diff 4 (make-insert "we")))
(define EXINS2 (make-diff 13 (make-insert "yes")))
(define EXINS3 (make-diff 17 (make-insert "er")))

(check-expect (updates (list DEL INS) "0123456789") "0xyzzy1256789")

(check-expect (updates (list EXDEL1 EXDEL2 EXINS1 EXINS2 EXINS3)
                       EXSTART-STRING)
              EXEND-STRING)
(check-expect (updates empty "HELP!") "HELP!")

(check-expect (updates (list EXDIFF1 EXDIFF2) "blueberry")
              (update (update "blueberry" EXDIFF1) EXDIFF2))
(check-expect (updates empty "printer") "printer")
(check-expect (updates (list EXDIFF3 EXDIFF4) "potato")
              (update (update "potato" EXDIFF3) EXDIFF4))
(check-expect (updates (list EXDIFF5 EXDIFF6) "Welcome new students!")
              (update (update "Welcome new students!" EXDIFF5) EXDIFF6))

 







(define (updates lod string) (apply lod string))
