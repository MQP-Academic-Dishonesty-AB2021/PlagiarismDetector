

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Lab1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)






(define (backwards reverse-this) "") 


(check-expect (backwards "") "")
(check-expect (backwards "a") "a")
(check-expect (backwards 
"The quick brown fox jumps over the lazy dog.") 
".god yzal eht revo spmuj xof nworb kciuq ehT")
(check-expect (backwards 	"Symbols!? & numbers, 2. 12345")
				"54321 .2 ,srebmun & ?!slobmyS")















(define-struct customer (fname lname age zip))






(define EX-C0 (make-customer "" "" 0 0))
(define EX-C1 (make-customer "Joe" "Freitas" 63 01629))
(define EX-C2 (make-customer "Amanda" "Vasile" 24 73102))

(define (fn-for-customer cust)
	(... (customer-fname cust) 
	(customer-lname cust) 
	(customer-age cust) 
	(customer-zip cust))) 







(check-expect (birthday EX-C0) (make-customer (customer-fname EX-C0) (customer-lname EX-C0) (+ (customer-age EX-C0) 1)  (customer-zip EX-C0)))
(check-expect (birthday EX-C1) (make-customer (customer-fname EX-C1) (customer-lname EX-C1) (+ (customer-age EX-C1) 1)  (customer-zip EX-C1)))
(check-expect (birthday EX-C2) (make-customer (customer-fname EX-C2) (customer-lname EX-C2) (+ (customer-age EX-C2) 1)  (customer-zip EX-C2)))

(define (birthday cust) (make-customer (customer-fname cust) (customer-lname cust) (+ (customer-age cust) 1) (customer-zip cust)))







(check-expect (name-change EX-C0 "" "") (make-customer 
	"" "" (customer-age EX-C0) (customer-zip EX-C0)))
(check-expect (name-change EX-C1 "Karen" "Freitas") (make-customer 
"Karen" "Freitas" (customer-age EX-C1) (customer-zip EX-C1)))
(check-expect (name-change EX-C2 "Amanda" "Quinn") (make-customer 
"Amanda" "Quinn" (customer-age EX-C2) (customer-zip EX-C2)))

(define (name-change cust new-fname new-lname)
(make-customer new-fname new-lname (customer-age cust) (customer-zip cust)))











(define-struct simple-img (shape area color))





(define SI0 (make-simple-img "circle" 0 "white"))
(define SI (make-simple-img "triangle" 250 "red"))
(define SI1 (make-simple-img "circle" 120 "red"))
(define SI2 (make-simple-img "square" 200 "purple"))
(define SI3 (make-simple-img "circle" 40 "purple"))
(define SI5 (make-simple-img "circle" 25.891 "green"))






(define (fn-for-simple-img si)
	(... 	(simple-img-shape si) 
		(simple-img-area si) 
		(simple-img-color si))) 




(check-expect (bigger? SI SI2) true)








(define bigSquare (make-simple-img "square" 500 "blue"))
(define smallCircle (make-simple-img "circle" 100 "green"))
(define sameSize (make-simple-img "triangle" 100 "yellow"))

(check-expect (bigger? smallCircle bigSquare) (> (simple-img-area smallCircle) (simple-img-area bigSquare)))
(check-expect (bigger? bigSquare smallCircle) (> (simple-img-area bigSquare) (simple-img-area smallCircle)))
(check-expect (bigger? sameSize smallCircle) (> (simple-img-area sameSize) (simple-img-area smallCircle)))


(define (bigger? image1 image2) (> (simple-img-area image1) (simple-img-area image2)))




(check-expect (build-image SI) (triangle (simple-size SI) "solid" "red"))





(check-expect (build-image SI0) 
(circle (sqrt (/ (simple-img-area SI0) pi)) "solid" (simple-img-color SI0)))
(check-expect (build-image SI) 
(triangle (sqrt (/ (* (simple-img-area SI) 4)(sqrt 3))) "solid" (simple-img-color SI)))
(check-expect (build-image SI2) 
(square (sqrt (simple-img-area SI2)) "solid" (simple-img-color SI2)))





(check-within (simple-size SI0) (sqrt (/ (simple-img-area SI0) pi)) 0.00001)
(check-within (simple-size SI) (sqrt (/ (* (simple-img-area SI) 4)(sqrt 3))) 0.00001)
(check-within (simple-size SI2) (sqrt (simple-img-area SI2)) 0.00001)
(check-within (simple-size SI5) (sqrt (/ (simple-img-area SI5) pi)) 0.00001)


(define (simple-size img) 
  (cond [(string=? (simple-img-shape img) "circle") 
         (sqrt (/ (simple-img-area img) pi))]
        [(string=? (simple-img-shape img) "triangle") 
         (sqrt (/ (* (simple-img-area img) 4)(sqrt 3)))]
        [(string=? (simple-img-shape img) "square") 
         (sqrt (simple-img-area img))]))

(define anotherCircle (make-simple-img "circle" (* pi (sqr 30)) "yellow"))
(define anotherTriangle (make-simple-img "triangle" (/ (* (sqrt 3) (sqr 5)) 4) "red"))

(check-expect (build-image bigSquare) (square (sqrt 500) "solid" "blue"))
(check-expect (build-image anotherCircle) (circle 30 "solid" "yellow"))
(check-expect (build-image anotherTriangle) (triangle 5 "solid" "red"))

(define (build-image img)
  (cond [(string=? (simple-img-shape img) "circle") 
         (circle (simple-size img) "solid" (simple-img-color img))]
        [(string=? (simple-img-shape img) "triangle") 
         (triangle (simple-size img) "solid" (simple-img-color img))]
        [(string=? (simple-img-shape img) "square") 
         (square (simple-size img) "solid" (simple-img-color img))]))


















 

(define-struct diff (is-insertion pos difference))







(define D0-1 (make-diff true 1 ""))
(define D0-2 (make-diff false 1 ""))
(define D1 (make-diff true 1 "xyzzy"))
(define D2 (make-diff false 3 "34"))
(define D3 (make-diff false 9 "9and10"))

(define (fn-for-diff diff) 
(if 	(diff-is-insertion diff) 
(...  (diff-pos diff)  (diff-difference diff))
(...  (diff-pos diff)  (diff-delete-length diff))))




(check-expect (diff-delete-length D0-1) 0)
(check-expect (diff-delete-length D0-2) 0) 
(check-expect (diff-delete-length D1) 0)
(check-expect (diff-delete-length D2) 2) 

(define (diff-delete-length diff)
(if 	(not (diff-is-insertion diff))
            (string-length (diff-difference diff))
 0))






(check-expect (update "" D0-1) "")
(check-expect (update "something" D0-1) "something")
(check-expect (update "" D1) "xyzzy")
(check-expect (update "something" D1) "sxyzzyomething")
(check-expect (update "" D0-2) "")
(check-expect (update "something" D0-2) "something")
(check-expect (update "" D2) "")
(check-expect (update "something" D2) "somhing")
(check-expect (delete "something" D3) "something")

(define (update pre diff)
	(if	(diff-is-insertion diff)
		(insert pre diff)
                        (delete pre diff)))




(check-expect (insert "" D0-1) "")
(check-expect (insert "something" D0-1) "something")
(check-expect (insert "" D1) "xyzzy")
(check-expect (insert "something" D1) "sxyzzyomething")

(define (insert pre diff)
 (if 	(< (string-length pre) (diff-pos diff))
(string-append pre (diff-difference diff))
(string-append 
         (substring pre 0 (diff-pos diff)) 
         (diff-difference diff) 
         (substring pre (diff-pos diff) (string-length pre)))))




(check-expect (delete "" D0-2) "")
(check-expect (delete "something" D0-2) "something")
(check-expect (delete "" D2) "")
(check-expect (delete "something" D2) "somhing")
(check-expect (delete "something" D3) "something")
(check-expect (delete "something9012" D3) "something")

(define (delete pre diff)
  (cond 	[(< (string-length pre) (diff-pos diff)) pre]
		[(< (string-length pre) (+ (diff-delete-length diff) (diff-pos diff)))
                 (substring pre 0 (diff-pos diff))]
		[else 
                 (string-append 
                  (substring pre 0 (diff-pos diff)) 
                  (substring pre (+ (diff-delete-length diff) (diff-pos diff)) (string-length pre)))]))



(define DQ-0 (make-diff false 4 "a"))
(define DQ-1 (make-diff false 4 "r"))
(define DQ-2 (make-diff false 4 "e"))
(define DQ-3 (make-diff true 4 "w"))
(define DQ-4 (make-diff true 5 "e"))
(define DQ-5 (make-diff true 6 "r"))
(define DQ-6 (make-diff true 7 "e"))

(check-expect (update "How are you today?" DQ-0) "How re you today?")
(check-expect (update "How re you today?" DQ-1) "How e you today?")
(check-expect (update "How e you today?" DQ-2) "How  you today?")
(check-expect (update "How  you today?" DQ-3) "How w you today?")
(check-expect (update "How w you today?" DQ-4) "How we you today?")
(check-expect (update "How we you today?" DQ-5) "How wer you today?")
(check-expect (update "How wer you today?" DQ-6) "How were you today?")






(define I0 (square 0 "outline" "white"))
(define I1 (square 75 "solid" "blue"))
(define I2 (rectangle 60 30 "solid" "yellow"))
(define I3 (triangle 50 "outline" "black"))
(define I4 (circle 40 "solid" "purple"))










(define LOI (list I1 I2 I3 I4))
(define LOI2 (cons I0 (cons I2 (cons I4 empty))))

(define (fn-for-LOI loi)
	(cond  	[(empty? loi) (...)]
		[else (... (first loi) (fn-for-LOI (rest loi)))]))







(check-expect (total-width empty) 0)
(check-expect (total-width LOI2) 140)
(check-expect (total-width LOI) 265) 

(define (total-width loi)
	(cond   [(empty? loi) 0]
		[else (+ (image-width (first loi)) (total-width (rest loi)))]))







(check-expect (taller-than empty 0) empty)
(check-expect (taller-than LOI 50) (list I1 I4))
(check-expect (taller-than LOI2 40) (list I4))

(define (taller-than loi this-tall)
	(cond 	[(empty? loi) empty]
	[(> (image-height (first loi)) this-tall) 
                 (cons (first loi) (taller-than (rest loi) this-tall))]
                [else (taller-than (rest loi) this-tall)]))








(check-expect (find-shape empty "") empty) 
(check-expect (find-shape (list SI0 SI5 smallCircle) "triangle") empty) 
(check-expect (find-shape (list SI0) "circle") (list (build-image SI0))) 
(check-expect (find-shape (list SI0 SI SI1 SI2 SI3 SI5) "circle") (list (build-image SI0) (build-image SI1) (build-image SI3) (build-image SI5))) 
(check-expect (find-shape (list SI0 SI SI1 SI2 SI3 SI5) "") empty) 

(define (find-shape loi type) 
  (cond 	[(empty? loi) empty]
                	[(string=? type (simple-img-shape (first loi))) 
            	 (cons (build-image (first loi)) (find-shape (rest loi) type))]
            [else (find-shape (rest loi) type)])) 

(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
 (list (build-image SI1) (build-image SI3)))














(define (fn-for-lod lod)
(cond
[(empty? lod) (...)]
[else  (if (diff-is-insertion (first lod)) (... fn-for-lod (rest lod)) (... fn-for-lod (rest lod)))]))




(define (updates lod str) (cond
[(empty? lod) str]
[else (if (diff-is-insertion (first lod)) (updates (rest lod) (insert str (first lod))) (updates (rest lod) (delete str (first lod))))
]))


(define lod0 empty)
(check-expect (updates lod0 "hello there") "hello there")

(define lod1 (list DQ-0 DQ-1 DQ-2 DQ-3 DQ-4 DQ-5 DQ-6))
(check-expect (updates lod1 "How are you today?") "How were you today?")



