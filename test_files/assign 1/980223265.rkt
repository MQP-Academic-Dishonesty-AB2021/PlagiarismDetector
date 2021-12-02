

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment 1, Kevin & Lada|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))











(require 2htdp/image)
















(define-struct customer (fname lname age zip))










(define (birthday customer)
  (make-customer (customer-fname customer) (customer-lname customer)
                 (+ 1 (customer-age customer)) (customer-zip customer)))




(define (name-change customer nfname nlname)
  (make-customer nfname nlname (customer-age customer) (customer-zip customer)))


(define c1 (make-customer "Justin" "Shen" 18 "01609"))
(define c2 (make-customer "Kevin" "Siegall" 18 "11746"))


(check-expect (birthday c1) (make-customer "Justin" "Shen" 19 "01609"))
(check-expect (birthday (make-customer "" "" -1 "")) (make-customer "" "" 0 ""))
(check-expect (name-change c1 "Kevin" "Siegall") (make-customer "Kevin" "Siegall" 18 "01609"))
(check-expect (name-change c2 "Lada" "DiMascolo") (make-customer "Lada" "DiMascolo" 18 "11746"))






(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))






(check-expect (total-width LOI) (+ (image-width I1)
                                   (image-width I2)
                                   (image-width I3)
                                   (image-width I4)))
(check-expect (total-width empty) 0)


(check-expect (make-beside (list I1 I4)) (beside I1 I4))
(check-expect (make-beside (cons I2 empty)) I2)




(check-expect (taller-than LOI 30)
              (list . .))
(check-expect (taller-than LOI 0) LOI)
(check-expect (taller-than empty 5) empty)
(check-expect (taller-than LOI 1000000) empty)




(define (total-width loi)
  (if (empty? loi) 0
      (image-width (make-beside loi))))




(define (make-beside loi)
     (if (empty? (rest loi)) (first loi)
    (beside (first loi) (make-beside (rest loi)))))



(define (taller-than loi cutoff)
  (if (empty? loi) empty 
      (if (> (image-height (first loi)) cutoff) (cons (first loi) (taller-than (rest loi) cutoff))
          (taller-than (rest loi) cutoff))))







(define-struct simple-img (shape color area))

(define SI1 (make-simple-img "circle" "red" 200))
(define SI2 (make-simple-img "square" "blue" 100))
(define SI3 (make-simple-img "circle" "green" 365))
(define SI4 (make-simple-img "circle" "purple" 300))

(define LOSI (list SI1 SI2 SI3 SI4))









(define (bigger img1 img2)
  [cond ((> (simple-img-area img1) (simple-img-area img2)) true)
        (else false)])

(check-expect (bigger SI1 SI2) true)
(check-expect (bigger SI1 SI3) false)









(define (build-image img)
  (if (string=? (simple-img-shape img) "triangle")
             (triangle (sqrt (/ (* 2 (simple-img-area img)) (sqrt 3))) "solid" (simple-img-color img))
             (if (string=? (simple-img-shape img) "square")
                 (square (sqrt (simple-img-area img)) "solid" (simple-img-color img))
                      (if (string=? (simple-img-shape img) "circle")
                          (circle (sqrt (/ (simple-img-area img) pi)) "solid" (simple-img-color img))
                          (square 0 "solid" "black")))))


(check-expect (build-image SI3) (circle (sqrt (/ 365 pi)) "solid" "green"))
(check-expect (build-image SI2) (square (sqrt 100) "solid" "blue"))
(check-expect (build-image (make-simple-img "triangle" "olive" (/ (* (expt 15 2) (sqrt 3)) 2))) (triangle 15 "solid" "olive"))
(check-expect (build-image (make-simple-img "decahedron" "lavender" 5)) (square 0 "solid" "black"))
















(define (find-shape losi shape)
    (if (empty? losi) empty
       (if (string=? (simple-img-shape (first losi)) shape)
        (cons (first losi) (find-shape (rest losi) shape))
          (find-shape (rest losi) shape))))

(check-expect (find-shape LOSI "circle")
              (list SI1 SI3 SI4))
(check-expect (find-shape empty "square") empty)







(define-struct diff (type pos change))









(define (update string diff)
  (cond [(string=? (diff-type diff) "insert")
           (insert string diff)]
        [(string=? (diff-type diff) "delete")
           (delete string diff)]
        [else string]))



(define (insert string diff)
  (string-append (string-append (substring string 0 (diff-pos diff))(diff-change diff))
                        (substring string (diff-pos diff) (string-length string))))



(define (delete string diff)
  (string-append (substring string 0 (diff-pos diff))
                        (substring string (+ (diff-pos diff) (diff-change diff))
                                   (string-length string))))


(define D1 (make-diff "insert" 3 "def"))
(define D2 (make-diff "delete" 2 6))
(define D3 (make-diff "shen" 1 "him"))


(check-expect (update "abcghi" D1) "abcdefghi")
(check-expect (update "1234567890" D2) "1290")
(check-expect (update "Hello there" D3) "Hello there")

(check-expect (update (update (update (update "How are you today?" (make-diff "delete" 4 1))
                                (make-diff "insert" 4 "we"))
                                (make-diff "delete" 13 5))
                                (make-diff "insert" 13 "yesterday"))
              "How were you yesterday?")


(check-expect (insert "abcghi" D1) "abcdefghi")
(check-expect (insert "123456" (make-diff "insert" 6 "789")) "123456789")


(check-expect (delete "1234567890" D2) "1290")
(check-expect (delete "123456789" (make-diff "delete" 6 3)) "123456")















(define (updates lod string)
     (if (empty? lod) string
    (updates (rest lod) (update string (first lod)))))


(define LOD1 (list (make-diff "delete" 4 1) (make-diff "insert" 4 "we")
      (make-diff "delete" 13 5) (make-diff "insert" 13 "yesterday")))


(check-expect (updates LOD1 "How are you today?") "How were you yesterday?")
(check-expect (updates empty "How are you today?") "How are you today?")