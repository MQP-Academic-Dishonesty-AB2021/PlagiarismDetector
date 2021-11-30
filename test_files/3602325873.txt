

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 1102Assignment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
























(define-struct customer (fname lname age zip))


(define C1 (make-customer "Sophia" "Zhou" 17 01451)) 
(define C2 (make-customer "Bryce" "Lukens" 18 02818))


(check-expect (birthday C1) (make-customer "Sophia" "Zhou" 18 01451))
(check-expect (name-change C2 "Henry" "Thoreau") 
	(make-customer "Henry" "Thoreau" 18 02818))


 
              


(define (birthday customer)
	(make-customer
  (customer-fname customer) 
  (customer-lname customer)
  (+ 1 (customer-age customer))
  (customer-zip customer)))



(define (name-change customer newf newl)
	(make-customer newf newl
  	(customer-age customer)
    (customer-zip customer)))



(require 2htdp/image)










 
(define WITHIN 5)



(check-expect (bigger? SI01 SI02) false)
(check-expect (bigger? SI03 SI04) false)
(check-expect (bigger? SI02 SI03) true)





  
(check-within (build-image SI01) 
              (circle (sqrt (/ (simple-img-area SI01) pi)) "solid" (simple-img-color SI01)) 
              WITHIN)

(check-within (build-image SI03) 
              (triangle (sqrt (/ (simple-img-area SI03) (/ (sqrt 3) 4))) "solid" (simple-img-color SI03)) 
              WITHIN)
(check-expect (build-image SI05) (square 0 "solid" "white"))
(check-within (build-image SI02) (square (sqrt (simple-img-area SI02)) "solid" (simple-img-color SI02))WITHIN)



(define-struct simple-img (type area color))

(define SI01 (make-simple-img "circle" 400 "red"))
(define SI02 (make-simple-img "square" 560 "blue"))
(define SI03 (make-simple-img "triangle" 450 "orange"))
(define SI04 (make-simple-img "circle" 450 "purple"))
(define SI05 (make-simple-img "happy" 450 "purple"))


 



(define (bigger? simg1 simg2)
	(> (simple-img-area simg1) (simple-img-area  simg2)))
  	



(define (build-image simple-img)
  (cond [(string=? "circle" (simple-img-type simple-img)) (build-circle simple-img)]
  			[(string=? "square" (simple-img-type simple-img)) (build-square simple-img)]
  			[(string=? "triangle" (simple-img-type simple-img)) (build-triangle simple-img)]
        [else (square 0 "solid" "white")])) 



(define (build-circle simple-img)
  (circle (sqrt (/ (simple-img-area simple-img) pi)) "solid" (simple-img-color simple-img)))



(define (build-triangle simple-img)
  (triangle (sqrt (/ (* 4 (simple-img-area simple-img)) (sqrt 3))) "solid" (simple-img-color simple-img)))



(define (build-square simple-img)
  (square (sqrt (simple-img-area simple-img)) "solid" (simple-img-color simple-img)))













(define-struct idiff (index text))
(define-struct ddiff (index numchars))


(define D01 (make-idiff  1 "abc")) 
(define D01A "hello")

(define D02 (make-ddiff  3 3)) 
(define D02A "glockenspiel")
(define D02B "") 

(define D03 (make-idiff  4 "")) 
(define D03A "thune")

(define D04 (make-ddiff  2 0)) 
(define D04A "grasshopper")

(define D05 (make-ddiff  0 20)) 
(define D05A "espana")

(define D06 (make-ddiff -1 2))
(define D06A "hola")


 
 
  			


(define (update-i text idiff)
  (cond [(string=? "" text)
    				(idiff-text idiff)]
				[ (< (idiff-index idiff) 0)
							""]
        [(> (string-length text) (idiff-index idiff)) 
  					(dinsert text idiff)]
        [else ""])) 



(define (dinsert text idiff)
  (string-append 
   	(substring text 0 (idiff-index idiff))
   	(idiff-text idiff)
   	(substring text (idiff-index idiff))))



(define (update-d text ddiff)
  (cond [(string=? "" text) ""]
				[(or (< (ddiff-index ddiff) 0) (< (ddiff-numchars ddiff) 0))
							""]
  			[(> (ddiff-numchars ddiff) (- (string-length text) (ddiff-index ddiff))) 
						(string-remove text (make-ddiff (ddiff-index ddiff) (- (string-length text) (ddiff-index ddiff))))]
  			[(> (string-length text) (ddiff-index ddiff)) (string-remove text ddiff)]
        [else ""]))



(define (string-remove text ddiff)
  (string-append (substring text 0 (ddiff-index ddiff))
                 (substring text (+ (ddiff-index ddiff) 
                            (ddiff-numchars ddiff))  
                            (string-length text) )))


(check-expect (update-d D02B D02) "")

(check-expect (update-i D01A D01) 
              (string-append (substring D01A 0 (idiff-index D01) )
                             (idiff-text D01) 
                             (substring D01A (idiff-index D01))))

(check-expect (update-d D02A D02) 
              (string-append (substring D02A 0 (ddiff-index D02) )
                             (substring D02A (+ (ddiff-index D02) (ddiff-numchars D02)) )))

(check-expect (update-i D03A D03) 
              (string-append (substring D03A 0 (idiff-index D03) )
                             (idiff-text D03) 
                             (substring D03A (idiff-index D03))))

(check-expect (update-d D04A D04) 
              (string-append (substring D04A 0 (ddiff-index D04) )
                             (substring D04A (+ (ddiff-index D04) (ddiff-numchars D04)) )))
            
(check-expect (update-d D05A D05) 
              (substring D05A 0 (ddiff-index D05) ))

(check-expect (update-d D06A D06) "") 



(define DEL-ARE (make-ddiff 4 3))
(define INS-WERE (make-idiff 4 "were"))
(define DEL-TODAY (make-ddiff 13 5))
(define INS-YESTERDAY (make-idiff 13 "yesterday"))

(define EXP1 "How are you today?")
(define EXP2 (update-i (update-d (update-i (update-d EXP1 DEL-ARE) INS-WERE) DEL-TODAY) INS-YESTERDAY))

(check-expect (string=? EXP2 "How were you yesterday?")
              true)









(define LI1 empty)
(define LI2 (list (square 20 "solid" "blue")))
(define LI3 (list (square 20 "solid" "blue")
                  (rectangle 10 20 "outline" "purple")))
(define LI4 (list (square 20 "solid" "blue")
                  (rectangle 10 20 "outline" "purple")
                  (rectangle 5 10 "solid" "blue")))
(define LI5 (list (square 0 "solid" "white")))


 






(define (total-width loi)
  (cond [(empty? loi) 0]
  			[else 
  				(+ (image-width (first loi)) 
             (total-width (rest loi)))]))






(define (taller-than loi cut)
  (cond [(empty? loi) loi]
  			[else 
  				(if (> (image-height (first loi)) cut)
              (cons (first loi) (taller-than (rest loi) cut))
              (taller-than (rest loi) cut))]))

 


(check-expect (total-width LI1) 0)
(check-expect (total-width LI2) 
              (image-width (first LI2)))
(check-expect (total-width LI3)
              (+ 
               (image-width (first LI3))
               (image-width (first (rest LI3)))))

(check-expect (taller-than LI1 10) empty)
(check-expect (taller-than LI2 10) 
              (cons (first LI2) empty))
(check-expect (taller-than LI3 10)
							(cons (first LI3) (cons (first (rest LI3)) empty)))
(check-expect (taller-than LI4 10)
							(cons (first LI4) (cons (first (rest LI4)) empty)))
(check-expect (taller-than LI5 10) empty)











 



(define (find-shape losi type)
  (cond [(empty? losi) empty]
  			[(string=? type (simple-img-type (first losi))) 
  			(cons (build-image (first losi)) (find-shape  (rest losi) type))]
  			[else (find-shape  (rest losi) type )]
				))
(define SI (make-simple-img "circle" 120 "red"))
(define SI3 (make-simple-img "circle" 40 "purple"))
(define SI2 (make-simple-img "square" 120 "red"))
(define SI1 (make-simple-img "square" 40 "purple"))
(define SI4 (make-simple-img "" 40 ""))
  
(check-expect (find-shape (list SI SI1 SI2 SI3) "circle")
              (list . .))
(check-expect (find-shape empty "circle") empty)

(check-expect (find-shape (list SI SI4) "circle") (list .))











(define-struct wrapperdiff (diff identifier))





 


(define WD1 (make-wrapperdiff 
             	(make-idiff 1 "the") "insert"))
(define WD2 (make-wrapperdiff 
             	(make-ddiff 3 2) "delete"))
(define WD3 (make-wrapperdiff 
             	(make-idiff 0 "") "insert"))
(define WD4 (make-wrapperdiff 
             	(make-ddiff 0 3) "delete"))
(define WD5 (make-wrapperdiff 
             	(make-ddiff 0 0) "delete"))
(define WD6 (make-wrapperdiff 
             	(make-ddiff 0 0) "hello"))


(check-expect (updates "hello" empty) "hello")

(check-expect (updates "" 
                       (list WD1 WD2))
              (update-d (update-i "" (wrapperdiff-diff WD1)) (wrapperdiff-diff WD2)))

(check-expect (updates "The weather is nice." 
                       (list WD1 WD2))
              (update-d (update-i "The weather is nice." (wrapperdiff-diff WD1)) (wrapperdiff-diff WD2)))

(check-expect (updates "" (list WD2 WD4 WD5))
              (update-d (update-d (update-d "" (wrapperdiff-diff WD2)) (wrapperdiff-diff WD4)) (wrapperdiff-diff WD5)))

(check-expect (updates "The Phantom of the Opera." 
                       (list WD1 WD3 WD6))
              (update-i (update-i "The Phantom of the Opera." (wrapperdiff-diff WD1)) (wrapperdiff-diff WD3)))




(define (updates string lod)
  (cond [(empty? lod) string]		
 [(string=? (wrapperdiff-identifier (first lod)) "insert")
  	(updates 
            (update-i string (wrapperdiff-diff(first lod))) (rest lod))]
[(string=? (wrapperdiff-identifier (first lod)) "delete") 
       	(updates 
            (update-d string (wrapperdiff-diff(first lod))) (rest lod))]
				[else (updates string (rest lod))]))







