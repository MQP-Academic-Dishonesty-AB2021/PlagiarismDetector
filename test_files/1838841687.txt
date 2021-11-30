

#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
















[define-struct customer [fname lname age zip]]





(define (fn-for-customer customer)
  (...(customer-fname customer)
      (customer-lname customer)
      (customer-age customer)
      (customer-zip customer)))






[check-expect [birthday [make-customer "Bob" "Allen" 32 96744]]
              [make-customer "Bob" "Allen" 33 96744]]
[check-expect [birthday [make-customer "Joe" "Mamma" 0 01634]]
              [make-customer "Joe" "Mamma" 1 01634]]




[define [birthday customer]
  [make-customer [customer-fname customer] [customer-lname customer]
                 [+ 1 [customer-age customer]] [customer-zip customer]]]






[check-expect [name-change [make-customer "Bob" "Allen" 33 40582] "Bob" "Allen"]
              [make-customer "Bob" "Allen" 33 40582]] 
[check-expect [name-change [make-customer "Jack" "Weinstein" 18 96744] "Joe" "Mamma"]
              [make-customer "Joe" "Mamma" 18 96744]] 
[check-expect [name-change [make-customer "Angie" "Baird" 18 27943] "Lily" "Baird"]
              [make-customer "Lily" "Baird" 18 27943]] 
[check-expect [name-change [make-customer "John" "Hall" 18 39842] "John" "Stark"]
              [make-customer "John" "Stark" 18 39842]] 

  

[define [name-change customer fname lname]
  [make-customer fname lname [customer-age customer] [customer-zip customer]]]
      







[define-struct simple-img [shape color area]]





[define SI [make-simple-img "triangle" "red" 250]]
[define SI2 [make-simple-img "triangle" "purple" 200]]
[define SI4 [make-simple-img "circle" "orange" 300]]
[define SI5 [make-simple-img "square" "green" 150]]


[define [fn-for-simple-img img]
  [... [simple-img-shape img]
       [simple-img-shape color]
       [simple-img-shape area]]]






(check-expect (bigger? SI SI2) true)
[check-expect [bigger? SI2 SI] false]
[check-expect [bigger? SI SI] false]
[check-expect [bigger? SI4 SI5] true]



[define [bigger? img1 img2]
  [cond [[> [simple-img-area img1] [simple-img-area img2]]
         true]
        [else false]]]









(check-expect (build-image SI) .)
[check-expect [build-image SI2] [triangle
                                 [sqrt [* [simple-img-area SI2]
                                          [/ 4 [sqrt 3]]]]
                                 "solid" [simple-img-color SI2]]]

[check-expect [build-image SI5] [square
                                 [sqrt [simple-img-area SI5]]
                                 "solid" [simple-img-color SI5]]]

[check-expect [build-image SI4] [circle [sqrt [/ [simple-img-area SI4] pi]]
                                        "solid" [simple-img-color SI4]]]


[define [build-image simg]
  [cond [[string=? "triangle" [simple-img-shape simg]]
         [triangle [sqrt [* [simple-img-area simg]
                            [/ 4 [sqrt 3]]]]
                   "solid" [simple-img-color simg]]]
        [[string=? "square" [simple-img-shape simg]]
         [square [sqrt [simple-img-area simg]]
                 "solid" [simple-img-color simg]]]
        [[string=? "circle" [simple-img-shape simg]]
         [circle [sqrt [/ [simple-img-area simg] pi]]
                 "solid" [simple-img-color simg]]]]]




[define-struct diff [pos text numChar]]






[define D1 [make-diff 3 "abc" 0]]
[define D2 [make-diff 5 "" 3]]
[define D3 [make-diff 2 "pineapple" 4]]


[define [fn-for-diff diff]
  [... [diff-type diff]
       [diff-pos diff]
       [diff-text diff]
       [diff-numChar diff]]]

















[check-expect [update D1 "0123456789"] "012abc3456789"]
[check-expect [update D2 "0123456789"] "0123489"]
[check-expect [update D3 "0123456789"] "01pineapple6789"]




[define [update diff str]
  (string-append (substring str 0 (diff-pos diff))
                 (diff-text diff)
                 [substring str [+ [diff-pos diff] [diff-numChar diff]]
                            [string-length str]])]



[define D4 [make-diff 4 "were" 3]]
[define D5 [make-diff 13 "yesterday" 5]]

[check-expect [update D5 [update D4 "How are you today?"]] "How were you yesterday?"]







(define I1 .)
(define I2 .)
(define I3 .)
(define I4 .)

(define LOI (list I1 I2 I3 I4))
[define LOI2 [list I2 I3]]
[define LOI3  empty]
[define LOI4 [list I4]]










 






[check-expect [total-width LOI3] 0]
(check-expect (total-width LOI) 265)
(check-expect (total-width LOI2) [+ [image-width I2] [image-width I3]])
(check-expect (total-width LOI4) [image-width I4])



[define [total-width loi]
  [cond [[empty? loi] 0]
        [else [+ [image-width [first loi]]
                 [total-width[rest loi]]]]]]




(check-expect (taller-than LOI 30)
              (list . .))

[check-expect [taller-than LOI3 30] empty]
[check-expect [taller-than LOI 0] LOI]
[check-expect [taller-than LOI 100] empty]



[define [taller-than loi cutOFF]
  [cond [[empty? loi] empty]
        [[> [image-height [first loi]] cutOFF]
         [cons [first loi] [taller-than [rest loi] cutOFF]]]
        [else [taller-than [rest loi] cutOFF]]]]






[define SI1 [make-simple-img "circle" "red" 120]]

[define SI3 [make-simple-img "circle" "purple" 40]]






[define LOSI [list SI1 SI2 SI3 SI5]]
[define LOSI2 empty]
[define LOSI3 [list SI2 SI5]]







 


 





(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
[check-expect [find-shape LOSI2 "triangle"] empty]
[check-expect [find-shape LOSI "square"] [list [build-image SI5]]]
[check-expect [find-shape LOSI3 "circle"] empty]



[define [find-shape losi str]
  [cond [[empty? losi] empty]
        [[string=? str [simple-img-shape [first losi]]]
         [cons [build-image [first losi]] [find-shape [rest losi] str]]]
        [else [find-shape [rest losi] str]]]]








[define DEL [make-diff 3 "" 2]]
[define INS [make-diff 1 "xyzzy" 0]]

[define LOD1 [list D1 D2 D3]]
[define LOD2 [list DEL INS]]







[define [fn-for-LOD lod]
    [cond [[empty? lod] ...]
          [else [... [first lod]
                     [fn-for-LOD[rest lod]]]]]]






[check-expect [updates empty "abc"] "abc"]
[check-expect [updates LOD2 "0123456789"] "0xyzzy1256789"]
[check-expect [updates [list DEL] "0123456789"] "01256789"]
[check-expect [updates [list INS] "0123456789"] "0xyzzy123456789"]
[check-expect [updates [list DEL D2] "0123456789"] "01256"]
[check-expect [updates [list DEL D1] "0123456789"] "012abc56789"]
[check-expect [updates [list D1 DEL] "0123456789"] "012c3456789"]
[check-expect [updates [list D3 DEL D1] "0123456789"] "01pabceapple6789"]


[define [updates lod str]
    [cond [[empty? lod] str]
          [else [updates [rest lod] [update [first lod] str]]]]]
