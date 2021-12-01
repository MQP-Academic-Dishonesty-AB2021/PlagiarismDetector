

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)




(define-struct widget (name quantity price))


(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 12 4))
(define F1 (make-widget "F1" 12 4))
(define D1 (make-widget "D1" 5 5))




(define-struct bst (widget left right))

(define BST1 (make-bst W1 false false))
(define BST2 (make-bst Z1 false false))
(define BST3 (make-bst A1 false false))

(define BST4 (make-bst D1 BST3 BST1))



(define (build-tree low)
  (foldr insert-name false low))



(define (smaller? str bst)
  (string<? str (widget-name (bst-widget bst)))
  )

(check-expect (smaller? "A1" BST1) true)  
(check-expect (smaller? "W1" BST1) false) 
(check-expect (smaller? "Z1" BST1) false) 



(define (same? str bst)
   (string=? str (widget-name (bst-widget bst)))
  )

(check-expect (same? "A1" BST1) false) 
(check-expect (same? "W1" BST1) true)  
(check-expect (same? "Z1" BST1) false) 



(define (find-name str bst)
  (if (boolean? bst)
      bst
      (cond
        [(same? str bst) (bst-widget bst)]
        [(smaller? str bst) (find-name str (bst-left bst))]
        [else (find-name str (bst-right bst))]
        )
      )
  )

(check-expect (find-name "A1" BST4) A1)    
(check-expect (find-name "W1" BST4) W1)    
(check-expect (find-name "D1" BST4) D1)    
(check-expect (find-name "B1" BST4) false) 



(define (insert-name wid bst)
  (cond
    [(boolean? bst) (make-bst wid false false)]
    [(equal? wid (bst-widget bst)) bst]
    [(smaller? (widget-name wid) bst) (make-bst (bst-widget bst) (insert-name wid (bst-left bst)) (bst-right bst))]
    [else (make-bst (bst-widget bst) (bst-left bst) (insert-name wid (bst-right bst)))]
    )
  )

(define B3 (make-widget "B3" 2 3))

(check-expect (insert-name B3 false) (make-bst B3 false false))                                          
(check-expect (insert-name A1 BST1) (make-bst W1 BST3 false))                                            
(check-expect (insert-name A1 BST1) (make-bst W1 (make-bst A1 false false) false))                       
(check-expect (insert-name B3 BST4) (make-bst D1 (make-bst A1 false (make-bst B3 false false)) BST1))    











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

