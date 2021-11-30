

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))










(define (backwards text) (list->string (reverse (string->list text))))

(check-expect (backwards "") "")
(check-expect (backwards "a") "a")
(check-expect (backwards "abcd") "dcba")









(define-struct Customer (first-name last-name age zip-code))



(define (birthday customer) (make-Customer (Customer-first-name customer) (Customer-last-name customer) (+ (Customer-age customer)1) (Customer-zip-code customer)))

(check-expect (birthday (make-Customer "bob" "ross" 0 24874)) (make-Customer "bob" "ross" 1 24874))
(check-expect (birthday (make-Customer "nate" "westfall" 34 02748)) (make-Customer "nate" "westfall" 35 02748))



(define (name-change customer new-first-name new-last-name) (make-Customer new-first-name  new-last-name (Customer-age customer) (Customer-zip-code customer)))

(check-expect (name-change (make-Customer "bob" "ross" 0 02478) "Nate" "Westfall") (make-Customer "Nate" "Westfall" 0 02478))
(check-expect (name-change (make-Customer "bill" "gates" 102 67123) "jeff" "bezos") (make-Customer "jeff" "bezos" 102 67123))




(require 2htdp/image)




(define-struct simple-img (type area color))

(define SI (make-simple-img "triangle" 250 "red"))
(define SI2 (make-simple-img "square" 200 "blue"))
(define SI4 (make-simple-img "circle" 300 "green"))



(define (bigger? SI SI2) (if (> (simple-img-area SI) (simple-img-area SI2)) SI SI2))

(check-expect (bigger? SI SI2) SI)
(check-expect (bigger? SI2 SI) SI)
(check-expect (bigger? SI2 SI4) SI4)




(define TRAINGLE-COEFFICIENT (/ 2 (expt 3 0.25)))
(define BLANK (square 0 "outline" "white"))
(define (build-image img) (cond
                            [(string=? (simple-img-type img) "triangle") (triangle (* TRAINGLE-COEFFICIENT (sqrt (simple-img-area img))) "solid" (simple-img-color img))]
                            [(string=? (simple-img-type img) "square") (square (sqrt (simple-img-area img)) "solid" (simple-img-color img))]
                            [(string=? (simple-img-type img) "circle") (circle (sqrt (/ (simple-img-area img) pi)) "solid" (simple-img-color img))]
                            [else BLANK]))
(check-expect (build-image SI) .)
(check-expect (build-image SI2) (square 14.1421356237 "solid" "blue"))
(check-expect (build-image SI4) (circle 9.77205024 "solid" "green"))
(check-expect (build-image (make-simple-img "a" 0 "f")) BLANK)







(define-struct Diff (pos delete text))






(define (update text diff) (string-append (substring text 0 (Diff-pos diff)) (Diff-text diff) (substring text (+ (Diff-pos diff) (Diff-delete diff)))))

(check-expect (update "0123456789" (make-Diff 1 0 "xyzzy")) "0xyzzy123456789")
(check-expect (update "0123456789" (make-Diff 3 2 "")) "01256789")
(check-expect (update "0123456789" (make-Diff 2 2 "tree")) "01tree456789")






(check-expect (update (update (update "How are you today?" (make-Diff 13 1 "er")) (make-Diff 12 0 "yes")) (make-Diff 4 1 "we")) "How were you yesterday?")




(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)
(define LOI (list I1 I2 I3 I4))



(define (total-width loi) (if (empty? loi) 0 (+ (image-width (first loi)) (total-width (rest loi)))))

(check-expect (total-width empty) 0)
(check-expect (total-width (list I1)) 100)
(check-expect (total-width (list I1 I2)) 125)
(check-expect (total-width LOI) 265)



(define (taller-than loi num) (cond
                                [(empty? loi) empty]
                                [(> (image-height (first loi)) num) (cons (first loi) (taller-than (rest loi) num))]
                                [else (taller-than (rest loi) num)]))

(check-expect (taller-than empty 30) empty)
(check-expect (taller-than LOI 30) (list . .))




(define SI1 (make-simple-img "circle" 120 "red"))
(define SI3 (make-simple-img "circle" 40 "purple"))



(define (find-shape los type) (cond
                                [(empty? los) empty]
                                [(string=? (simple-img-type (first los)) type) (cons (build-image (first los)) (find-shape (rest los) type))]
                                [else (find-shape (rest los) type)]))


(check-expect (find-shape empty "") empty)
(check-expect (find-shape (list SI SI1 SI2 SI3) "triangle") (list .))
(check-expect (find-shape (list SI SI1 SI2 SI3) "circle") (list . .))






(define (updates text lod) (if (empty? lod) text (updates (update text (first lod)) (rest lod)))) 

(check-expect (updates "Nate" empty) "Nate")
(check-expect (updates "0123456789" (list (make-Diff 3 2 "") (make-Diff 1 0 "xyzzy"))) "0xyzzy1256789")






(check-expect (updates "How are you today?" (list (make-Diff 13 1 "er") (make-Diff 12 0 "yes")  (make-Diff 4 1 "we"))) "How were you yesterday?")