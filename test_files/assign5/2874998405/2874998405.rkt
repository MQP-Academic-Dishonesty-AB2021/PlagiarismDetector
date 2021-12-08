

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ASSIGNMENT5_part1 FINISHED|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct widget (name quantity price))





(define W (make-widget "WW" 1 1))
(define Z (make-widget "ZZZ" 51 16))
(define A (make-widget "AAAA" 2 3))
(define D (make-widget "D" 5 5))

(define Wee (make-widget "Wee" 5 6))
(define Shib (make-widget "Shib" 8 9))
(define F (make-widget "F" 10 19))

(define-struct bst (widget left right))






(define Ab (make-bst A false false))
(define Db (make-bst D false false))
(define Wb (make-bst W Db false))
(define Zb (make-bst Z Wb Ab)) 

(define (build-tree low)
  (foldr insert-name false low))




(check-expect (smaller? "D" Zb) true)
(check-expect (smaller? "ZZZZ" Db) false)
(check-expect (smaller? "WW" Wb) false)
(check-expect (smaller? "AAAA" Zb) false)

(define (smaller? s b)
  (cond
    [(< (string-length s) (string-length (widget-name(bst-widget b))))
     true]
    [else
     false]))


(check-expect (same? "D" Db) true)
(check-expect (same? "WW" Wb) true)
(check-expect (same? "ZZZ" Db) false)
 
(define (same? s b)
  (cond
    [(string=? s (widget-name(bst-widget b)))
     true]
    [else
     false]))


(check-expect (find-name "D" Db) D)
(check-expect (find-name "WW" Zb) W)
(check-expect (find-name "AAAA" Zb) A)  
(check-expect (find-name "urmom" Zb) false)
 


(define (find-name s b)
  (cond
    [(false? b) false]
    [else
     (cond
       [(same? s b) (bst-widget b)]
       [(false? (smaller? s b)) (find-name s (bst-right b))]
       [(smaller? s b) (find-name s (bst-left b))])]))


(check-expect (insert-name Wee Wb)
              (make-bst W
                        (make-bst D false false)
                        (make-bst Wee false false)))
(check-expect (insert-name F Zb)
              (make-bst Z
                        (make-bst W
                                  (make-bst D false
                                            (make-bst F false false)) false) Ab))
(check-expect (insert-name Shib Ab)
              (make-bst A false
                        (make-bst Shib false false)))
 
(define (insert-name w b)
  (cond
    [(false? b)
     (make-bst w false false)]
    [else
     (cond
       [(smaller? (widget-name w) b)
        (make-bst
         (bst-widget b)
         (insert-name w (bst-left b))
         (bst-right b))]
        [else
         (make-bst
         (bst-widget b)
         (bst-left b)
         (insert-name w (bst-right b)))])]))

  









(define (random-widgets num max)
  (build-list num
              (λ(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))





(define (random-widgets-string num slen nmax)
  (local
    [(define (random-string len)
       (list->string (build-list len (λ(dummy)
                                       (integer->char (+ 97 (random 26)))))))]
    (build-list num
                (λ(dummy)
                  (make-widget
                   (random-string slen) 
                   (random nmax)
                   (random nmax))))))





(define (render b)
  (local
    [
     (define TEXT-SIZE 20)    
     (define TEXT-COLOR1 "pink")
     (define TEXT-COLOR2 "orange")
     (define TAB 8)
     
     
     (define (blanks n)
       (list->string (build-list n (λ(x) #\ ))))
     
     
     (define (get-color d)
       (if (= (modulo d 2) 0)
           TEXT-COLOR1
           TEXT-COLOR2))
     
     
     (define (to-text side w d)
       (text  (string-append (blanks (* d TAB)) side (widget-name w))
              TEXT-SIZE
              (get-color d)))
     
     
     (define (render-helper b d img side)
       (if (false? b)
           img
           (above/align "left"
                        (to-text side (bst-widget b) d)
                        (render-helper (bst-left b) (+ d 1) img "L: ")
                        (render-helper (bst-right b) (+ d 1) img "R: "))))]
    (render-helper b 0 empty-image "T: ")))