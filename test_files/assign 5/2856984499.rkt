

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter pt1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)






(define-struct widget (name quantity price))








(define-struct bst (widget left right))









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




(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 100 100))
(define C1 (make-widget "C1" 999 999))
(define D1 (make-widget "D1" 5 5))
(define W1 (make-widget "W1" 1 1))
(define X1 (make-widget "X1" 420 420))
(define Z1 (make-widget "Z1" 51 16))
(define E1 (make-widget "E1" 6 7))

(define BST7 (make-bst B1 false false))
(define BST4 (make-bst A1 false BST7))
(define BST5 (make-bst D1 false false))
(define BST2 (make-bst C1 BST4 BST5))
(define BST6 (make-bst X1 false false))
(define BST3 (make-bst Z1 BST6 false))
(define BST1 (make-bst W1 BST2 BST3)) 



(define (build-tree low)
  (foldr insert-name false low))



(check-expect (smaller? "A1" BST7) true) 
(check-expect (smaller? "B1" BST4) false) 
(check-expect (smaller? "C1" false) false) 

(define (smaller? key bst)
 (and (not (false? bst)) (string<? key (widget-name (bst-widget bst)))))



(check-expect (same? "A1" BST4) true) 
(check-expect (same? "B1" BST1) false) 
(check-expect (same? "C1" false) false) 

(define (same? key bst) 
  (and (not (false? bst)) (string=? key (widget-name (bst-widget bst)))))



(check-expect (find-name "A1" BST4) A1) 
(check-expect (find-name "A1" BST2) A1) 
(check-expect (find-name "X1" BST6) X1) 
(check-expect (find-name "Z1" BST4) false) 

(define (find-name key bst) 
  (cond
   [(false? bst) false]
   [(same? key bst)
    (bst-widget bst)]
   [(smaller? key bst)
    (find-name key (bst-left bst))]
   [else 
    (find-name key (bst-right bst))]))
  



(check-expect (insert-name E1 BST3)
              (make-bst Z1
                        (make-bst X1
                                  (make-bst E1 false false)
                                  false)
                        false)) 
(check-expect (insert-name E1 BST7)
              (make-bst B1
                        false
                        (make-bst E1 false false))) 
(check-expect (insert-name E1 BST4)
              (make-bst A1
                        false
                        (make-bst B1
                                  false
                                  (make-bst E1
                                            false
                                            false)))) 
(check-expect (insert-name E1 BST2)
              (make-bst C1
                        BST4
                        (make-bst D1
                                  false
                                  (make-bst E1
                                            false
                                            false)))) 
(check-expect (insert-name C1 BST2)
              BST2) 
(check-expect (insert-name C1 false)
              (make-bst C1 false false)) 

(define (insert-name widget bst)
  (cond
   [(false? bst) (make-bst widget
                           false
                           false)]
   [(smaller? (widget-name widget) bst) (make-bst (bst-widget bst)
                                 (insert-name widget (bst-left bst))
                                 (bst-right bst))]
   [(same? (widget-name widget) bst) bst]
   [else (make-bst (bst-widget bst)
                   (bst-left bst)
                   (insert-name widget (bst-right bst)))]))