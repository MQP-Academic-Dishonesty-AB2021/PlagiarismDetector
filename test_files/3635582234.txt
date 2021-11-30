

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

(define-struct widget (name quantity price))





(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 1 1010101))
(define C1 (make-widget "C1" 10 50))
(define D1 (make-widget "D1" 5 5))
(define E1 (make-widget "E1" 9 20))
(define V1 (make-widget "V1" 100 100))
(define W1 (make-widget "W1" 1 1))
(define X1 (make-widget "X1" 200 70))
(define Y1 (make-widget "Y1" 52 60))
(define Z1 (make-widget "Z1" 51 16))
(define free-stuff (make-widget "free stuff" 0 0))

(define-struct bst (widget left right))






(define (build-tree low)
  (foldr insert-name false low))


(define EMPTY false)

(define NO-CHILDREN (make-bst D1 false false))

(define BST1 (make-bst D1
                       (make-bst A1 false false)
                       (make-bst Z1
                                 (make-bst W1 false false)
                                 false)))

(define BST2 (make-bst D1
                       (make-bst A1 false false)
                       (make-bst Z1 false false)))

(define LEFT (make-bst Z1
                       (make-bst W1
                                 (make-bst D1
                                           (make-bst A1 false false)
                                           false)
                                 false)
                       false))

(define RIGHT (make-bst A1
                        false
                        (make-bst D1
                                  false
                                  (make-bst W1
                                            false
                                            (make-bst Z1 false false)))))










(check-expect (smaller? "D1" NO-CHILDREN) false) 
(check-expect (smaller? "A1" BST1) true) 
(check-expect (smaller? "Z1" RIGHT) false) 


(define (smaller? key bst)
  (string<? key (widget-name (bst-widget bst))))








(check-expect (same? "food" false) false) 
(check-expect (same? "B1" BST2) false) 
(check-expect (same? "D1" NO-CHILDREN) true) 
(check-expect (same? "Z1" LEFT) true) 
(check-expect (same? "A1" BST1) false) 
(check-expect (same? "Z1" RIGHT) false) 


(define (same? key bst)
  (cond [(false? bst) false]
        [else (string=? key (widget-name (bst-widget bst)))]))







(check-expect (find-name "lmao" EMPTY) false) 
(check-expect (find-name "hi" LEFT) false) 
(check-expect (find-name "D1" NO-CHILDREN) D1) 
(check-expect (find-name "W1" LEFT) W1) 
(check-expect (find-name "Z1" RIGHT) Z1) 
(check-expect (find-name "Z1" BST1) Z1) 


(define (find-name key bst)
  (cond [(false? bst) false]
        [else (cond [(same? key bst) (bst-widget bst)]
                    [(smaller? key bst) (find-name key (bst-left bst))]
                    [else (find-name key (bst-right bst))])]))







(check-expect (insert-name D1 EMPTY) NO-CHILDREN) 
(check-expect (insert-name A1 NO-CHILDREN) 
              (make-bst D1 (make-bst A1 false false) false)) 
(check-expect (insert-name W1 NO-CHILDREN)
              (make-bst D1 false (make-bst W1 false false))) 
(check-expect (insert-name V1 BST1) 
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst Z1 (make-bst W1 (make-bst V1 false false) false) false)))
(check-expect (insert-name free-stuff BST1) 
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst Z1
                                  (make-bst W1 false false)
                                  (make-bst free-stuff false false)))) 


(define (insert-name widget bst)
  (cond 
    [(false? bst) (make-bst widget false false)] 
    [(smaller? (widget-name widget) bst) 
     (make-bst (bst-widget bst) (insert-name widget (bst-left bst)) (bst-right bst))]
    [else 
     (make-bst (bst-widget bst) (bst-left bst) (insert-name widget (bst-right bst)))]))







(define (random-widgets num max)
  (build-list num
              (λ (dummy)
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