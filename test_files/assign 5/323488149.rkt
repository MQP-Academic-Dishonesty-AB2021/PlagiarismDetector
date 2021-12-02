

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))






(require 2htdp/image)

(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define W1.1 (make-widget "W1" 0 .5))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define S1 (make-widget "S1" 5 5))
(define S2 (make-widget "S2" 5 5))

(define-struct bst (widget left right))



(define W1-NODE (make-bst W1 false false))
(define W1-BST (make-bst W1 (make-bst W1.1 false false) false))
(define W1-BST-FLIP (make-bst W1.1 false (make-bst W1 false false)))
(define Z1-NODE (make-bst Z1 false false))
(define ALPHABET (make-bst W1 
                           (make-bst A1 false (make-bst D1 false false))
                           (make-bst Z1 false false)))



(define (build-tree low)
  (foldr insert-name false low))




                              

(check-expect (smaller? "a" false) 
              false)
(check-expect (smaller? "" ALPHABET) 
              true)


(check-expect (smaller? "A" W1-BST) 
              true)
(check-expect (smaller? "Z"  W1-BST)
              false)            
(check-expect (smaller? "zz" ALPHABET)
              false)

(define (smaller? key bst) 
  (cond [(false? bst) false]
        [else (string<? key (widget-name (bst-widget bst)))]))








(check-expect (same? "" W1-NODE) false) 
(check-expect (same? "W1" W1-NODE) true) 
(check-expect (same? "W1" ALPHABET) true) 
(check-expect (same? "A1" ALPHABET) false) 

(define (same? key bst) 
  (string=? key (widget-name (bst-widget bst))))






(check-expect (find-name "" false) false) 
(check-expect (find-name "" W1-NODE) false) 
(check-expect (find-name "W1" W1-NODE) W1) 
(check-expect (find-name "W1" ALPHABET) W1) 
(check-expect (find-name "Z1" ALPHABET) Z1) 
(check-expect (find-name "W1" W1-BST) W1) 
(check-expect (find-name "W1" W1-BST-FLIP) W1.1) 
(check-expect (find-name "W1" Z1-NODE) false) 

(define (find-name key bst) 
  (cond [(false? bst) false]
        [(same? key bst) (bst-widget bst)]
        [(smaller? key bst) (find-name key (bst-left bst))]
        [else (find-name key (bst-right bst))]))








(check-expect (insert-name S1 W1-NODE)
              (make-bst
               (make-widget "W1" 1 1)
               (make-bst (make-widget "S1" 5 5) #false #false)
               #false)) 

(check-expect (insert-name W1.1 W1-NODE)
              (make-bst
               (make-widget "W1" 1 1) #false
               (make-bst (make-widget "W1" 0 0.5)
                         #false
                         #false))) 


(check-expect
 (insert-name S1 ALPHABET)
 (make-bst
  (make-widget "W1" 1 1)
  (make-bst
   (make-widget "A1" 2 3) #false
   (make-bst (make-widget "D1" 5 5) #false (make-bst (make-widget "S1" 5 5) #false #false)))
  (make-bst (make-widget "Z1" 51 16) #false #false))) 

(check-expect (insert-name S2 (insert-name S1 ALPHABET))
              (make-bst
               (make-widget "W1" 1 1)
               (make-bst
                (make-widget "A1" 2 3) #false
                (make-bst
                 (make-widget "D1" 5 5) #false
                          (make-bst
                           (make-widget "S1" 5 5) #false
                                    (make-bst
                                     (make-widget "S2" 5 5) #false #false))))
               (make-bst
                (make-widget "Z1" 51 16) #false #false))) 

(define (insert-name key bst)
  (cond [(false? bst) (make-bst key false false)]
        [else
         (if (smaller? (widget-name key) bst)
             (make-bst  (bst-widget bst) 
                        (insert-name key (bst-left bst))
                        (bst-right bst))
             (make-bst (bst-widget bst)
                       (bst-left bst)
                       (insert-name key (bst-right bst))))]))







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