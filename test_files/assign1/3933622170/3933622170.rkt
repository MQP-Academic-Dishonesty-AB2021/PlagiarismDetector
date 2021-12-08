

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment #1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))











(define (backwards string) (list->string (reverse (string->list string))))



(check-expect (backwards "abcd") "dcba")
(check-expect (backwards "") "")
(check-expect (backwards "pad") "dap")










(define-struct customer (fname lname age zip))


(define C1 (make-customer "John" "Smith" 13 "02478"))
(define C2 (make-customer "Ian" "Wright" 18 "01605"))
(define C3 (make-customer "Billy" "Eyelash" 21 "E4B 0H5"))


(define (fn-for-customer customer)
  (...
   (customer-fname customer)
   (customer-lname customer)
   (customer-age customer)
   (customer-zip customer)))






(define (birthday customer)
  (make-customer
   (customer-fname customer)
   (customer-lname customer)
   (+ (customer-age customer) 1)
   (customer-zip customer)))

(check-expect (birthday C1) (make-customer
                             (customer-fname C1)
                             (customer-lname C1)
                             (+ (customer-age C1) 1)
                             (customer-zip C1)))
(check-expect (birthday C2) (make-customer
                             (customer-fname C2)
                             (customer-lname C2)
                             (+ (customer-age C2) 1)
                             (customer-zip C2)))
(check-expect (birthday C3) (make-customer
                             (customer-fname C3)
                             (customer-lname C3)
                             (+ (customer-age C3) 1)
                             (customer-zip C3)))








(define (name-change customer fname lname)
  (make-customer
   fname
   lname
   (customer-age customer)
   (customer-zip customer)))

(check-expect (name-change C1 "Jimmy" "John") (make-customer "Jimmy" "John"
                                                             (customer-age C1)
                                                             (customer-zip C1)))
(check-expect (name-change C2 "Jim" "Jim") (make-customer "Jim" "Jim"
                                                          (customer-age C2)
                                                          (customer-zip C2)))
(check-expect (name-change C3 "" "") (make-customer "" ""
                                                    (customer-age C3)
                                                    (customer-zip C3)))



(require 2htdp/image)




(define-struct simple-img (type color area))
(define SI (make-simple-img "triangle" "red" 250))
(define SI1 (make-simple-img "circle" "red" 120))
(define SI2 (make-simple-img "square" "purple" 200))
(define SI3 (make-simple-img "circle" "purple" 40))

 






(define (bigger? image1 image2)
  (> (simple-img-area image1)
     (simple-img-area image2)))

(check-expect (bigger? SI SI2) true)
(check-expect (bigger? SI2 SI) false)
(check-expect (bigger? SI3 SI2) false)
(check-expect (bigger? SI SI) false)






(define (build-image image)
  (cond [(string=? (simple-img-type image) "square")
         (square (sqrt (simple-img-area image))
                 "solid"
                 (simple-img-color image))]
        [(string=? (simple-img-type image) "circle")
         (circle (sqrt (/ (simple-img-area image) pi))
                 "solid"
                 (simple-img-color image))]
        [(string=? (simple-img-type image) "triangle")
         (triangle (* 2/3 (expt 3 3/4) (sqrt (simple-img-area image)))
                   "solid"
                   (simple-img-color image))]))
(check-expect (build-image SI) .)
(check-expect (build-image (make-simple-img "triangle" "purple" 200))
              (triangle (* 2/3 (expt 3 3/4) (sqrt 200))
                        "solid"
                        "purple"))
(check-expect (build-image SI2) (square
                                 (sqrt (simple-img-area SI2))
                                 "solid"
                                 (simple-img-color SI2)))
(check-expect (build-image (make-simple-img "square" "red" 600))
              (square (sqrt 600) "solid" "red"))
(check-expect (build-image SI3) (circle
                                 (sqrt (/ (simple-img-area SI3) pi))
                                 "solid"
                                 (simple-img-color SI3)))
(check-expect (build-image (make-simple-img "circle" "blue" 900))
              (circle (sqrt (/ 900 pi))
                      "solid"
                      "blue"))




(define (type=? image type)
  (string=? (simple-img-type image) type))

(check-expect (type=? SI "triangle") true)
(check-expect (type=? SI1 "square") false)
(check-expect (type=? SI3 "circle") true)







(define-struct add-diff (position content))

(define AD1 (make-add-diff 0 "Hi!"))
(define AD2 (make-add-diff 10 "")) 
(define AD3 (make-add-diff 15 "Bye!"))

(define HD3 (make-add-diff 4 "we"))
(define HD4 (make-add-diff 13 "yester"))

 




(define-struct remove-diff (position amount))

(define RD1 (make-remove-diff 0 2))
(define RD2 (make-remove-diff 10 0)) 
(define RD3 (make-remove-diff 15 1))
(define RD4 (make-remove-diff 20 3))

(define HD1 (make-remove-diff 4 1))
(define HD2 (make-remove-diff 11 2))

(define (fn-for-remove-diff diff)
  (... (remove-diff-position diff)
       (remove-diff-amount diff)))











(define (update diff string)
  (cond [(add-diff? diff)
         (update--add-diff diff string)]
        [(remove-diff? diff)
         (update--remove-diff diff string)]))




(check-expect (update (make-add-diff 0 "asdf") "") "asdf")
(check-expect (update (make-add-diff 1 "fghj") "ba") "bfghja")
(check-expect (update (make-remove-diff 0 2) "asdf") "df")
(check-expect (update (make-remove-diff 1 2) "hello") "hlo")




(define (update--add-diff diff string)
  (string-append
   (substring string 0 (add-diff-position diff))
   (add-diff-content diff)
   (substring string (add-diff-position diff) (string-length string))))

(check-expect (update--add-diff (make-add-diff 0 "asdf") "") "asdf")
(check-expect (update--add-diff (make-add-diff 0 "abcd") "efgh") "abcdefgh")
(check-expect (update--add-diff (make-add-diff 10 "xyz") "0123456789") "0123456789xyz")
(check-expect (update--add-diff (make-add-diff 5 "you") "abcdefghij") "abcdeyoufghij")
(check-expect (update--add-diff (make-add-diff 6 "") "asdfghjkl") "asdfghjkl")




(define (update--remove-diff diff string)
  (string-append
   (substring string 0 (remove-diff-position diff))
   (substring string (+ (remove-diff-position diff)
                        (remove-diff-amount diff))
              (string-length string))))

(check-expect (update--remove-diff (make-remove-diff 0 5) "12345") "")
(check-expect (update--remove-diff (make-remove-diff 0 3) "12345") "45")
(check-expect (update--remove-diff (make-remove-diff 5 2) "0123456789") "01234789")
(check-expect (update--remove-diff (make-remove-diff 2 2) "abcd") "ab")
(check-expect (update--remove-diff (make-remove-diff 2 0) "asdfghjkl") "asdfghjkl")











































(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))
(define LOI2 (list I4 I3 I2 I1))
(define LOI3 (list I3 I1))
(define LOI4 (list I4 I2))
(define LOI5 (list I1))

 






(define (total-width images)
  (cond [(empty? images) 0]
        [else
         (+ (image-width (first images))
            (total-width (rest images)))]))

(check-expect (total-width empty) 0)
(check-expect (total-width LOI) 265)
(check-expect (total-width LOI2) 265) 
(check-expect (total-width LOI3) (+ (image-width I3) (image-width I1)))
(check-expect (total-width LOI4) (+ (image-width I4) (image-width I2)))
(check-expect (total-width LOI5) (image-width I1))






(define (taller-than images height)
  (cond [(empty? images) empty]
        [else
         (if (taller? (first images) height)
             (cons (first images) (taller-than (rest images) height))
             (taller-than (rest images) height))]))

(check-expect (taller-than empty 13123) empty)
(check-expect (taller-than LOI 0) LOI)
(check-expect (taller-than LOI5 (image-height I1)) empty)
(check-expect (taller-than LOI5 (- (image-height I1) 1)) (list I1))
(check-expect (taller-than LOI5 (+ (image-height I1) 1)) empty)
(check-expect (taller-than LOI2 30) 
              (list . .))
(check-expect (taller-than LOI 30)
              (list . .))

(check-expect (taller-than LOI3 99999999999999999) empty)




(define (taller? image height)
  (> (image-height image) height))

(check-expect (taller? I1 (image-height I1)) false)
(check-expect (taller? I2 (- (image-height I2) 1)) true)
(check-expect (taller? I3 (+ (image-height I3) 1)) false)











(define LOSI (list SI SI2 SI3))
(define LOISI2 empty)
(define LOISI3 (list SI))
(define LOISI4 (list SI2 SI))

 






(define (find-shape images type)
  (cond [(empty? images) empty]
        [else
         (if (type=? (first images) type)
             (cons (build-image (first images))
                   (find-shape (rest images) type))
             (find-shape (rest images) type))]))

(check-expect (find-shape empty "circle") empty)
(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
(check-expect (find-shape (list SI3 SI2 SI1 SI) "circle") 
              (list . .))
(check-expect (find-shape (list SI SI1 SI2 SI3) "triangle")
              (list (build-image SI)))
(check-expect (find-shape (list SI) "square") empty)
(check-expect (find-shape (list SI) "triangle")
              (list (build-image SI)))
(check-expect (find-shape (list SI2 SI3) "square")
              (list (build-image SI2)))










(define LOD (list (make-add-diff 0 "hello") (make-remove-diff 0 5))) 
(define HDS (list HD1 HD2 HD3 HD4)) 

 




(define (updates diffs string)
  (cond [(empty? diffs) string]
        [else
         (updates (rest diffs)
                  (update (first diffs) string))]))

(check-expect (updates empty "qwerty") "qwerty")
(check-expect (updates LOD "asdfghjkl") "asdfghjkl")
(check-expect (updates HDS "How are you today?")
              "How were you yesterday?")
(check-expect (updates (list (make-remove-diff 3 2)
                             (make-add-diff 1 "xyzzy")) "0123456789")
              "0xyzzy1256789")