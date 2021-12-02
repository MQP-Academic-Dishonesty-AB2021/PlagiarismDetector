

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |5 pt1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define C1 (make-widget "C1" 5 5))

(define-struct bst (widget left right))






(define (build-tree low)
  (foldr insert-name false low))

(define B-Z1 (make-bst Z1 false false))
(define B-A1 (make-bst A1 false false))
(define B-W1 (make-bst W1 false B-Z1))
(define B-D1 (make-bst D1 B-A1 B-W1))





(define (smaller? str bst)
  (string<? str (widget-name (bst-widget bst))))

(check-expect (smaller? "A" B-D1) #t)
(check-expect (smaller? "Z" B-D1) #f)
(check-expect (smaller? "D12" B-D1) #f)



(define (same? str bst)
  (string=? str (widget-name (bst-widget bst))))

(check-expect (same? "D1" B-D1) #t)
(check-expect (same? "D234465" B-D1) #f)
(check-expect (same? "W1" B-W1) #t)
(check-expect (same? "W234" B-W1) #f)




(define (find-name str bst) (cond
                              [(false? bst) false]
                              [(same? str bst) (bst-widget bst)]
                              [(smaller? str bst) (find-name str (bst-left bst))]
                              [else (find-name str (bst-right bst))]))

(check-expect (find-name "Z1" B-Z1) Z1)
(check-expect (find-name "W1" B-Z1) #f)
(check-expect (find-name "Z1" B-D1) Z1)
(check-expect (find-name "W1" B-D1) W1)
(check-expect (find-name "htrjyhu" B-D1) false)



(define (insert-name wid bst) (cond
                                [(false? bst) (make-bst wid false false)]
                                
                                [(smaller? (widget-name wid) bst) (make-bst (bst-widget bst)
                                                                            (insert-name wid (bst-left bst))
                                                                            (bst-right bst))]          
                                [else (make-bst (bst-widget bst)
                                                (bst-left bst)
                                                (insert-name wid (bst-right bst)))]))

(check-expect (insert-name Z1 B-A1) (make-bst A1 false (make-bst Z1 false false)))
(check-expect (insert-name A1 B-Z1) (make-bst Z1 (make-bst A1 false false) false))
(check-expect (insert-name C1 B-D1) (make-bst (make-widget "D1" 5 5)
                                              (make-bst A1 #f (make-bst C1 #f #f))
                                              (make-bst W1 #f (make-bst Z1 #f #f))))










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

