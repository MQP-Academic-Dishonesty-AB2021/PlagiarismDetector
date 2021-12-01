

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)



(define-struct widget (name quantity price))


(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define B1 (make-widget "B1" 12 2))
(define F1 (make-widget "F1" 12 4))
(define X1 (make-widget "X1" 7 6))




(define-struct bst (widget left right))

(define BSTA1 (make-bst A1 false false))
(define BSTD1 (make-bst D1 false false))
(define BSTZ1 (make-bst Z1 false false))
(define BSTX1 (make-bst X1 false false))
(define BSTB1 (make-bst B1 BSTA1 BSTD1))
(define BSTW1 (make-bst W1 false BSTZ1))
(define BSTF1 (make-bst F1 BSTB1 BSTW1))

(define L1 (make-widget "L1" 2 13))
(define M1 (make-widget "M1" 3 14))
(define N1 (make-widget "N1" 1 15))
(define O1 (make-widget "O1" 4 17))
(define P1 (make-widget "P1" 3 18))

(define BSTpri (make-bst L1                                 
                 (make-bst F1                               
                       (make-bst B1
                                 (make-bst W1 false false)
                                 (make-bst A1 false false))
                       (make-bst X1
                                 (make-bst D1 false false)
                                 false))
                 (make-bst M1                               
                       false
                       (make-bst O1
                                 (make-bst N1 false
                                           (make-bst Z1 false false))
                                 (make-bst P1 false false))
                       )))

(define BSTpri1 (make-bst L1                                
                 (make-bst F1                               
                       (make-bst B1
                                 (make-bst W1 false false)
                                 (make-bst A1 false false))
                       (make-bst X1
                                 false
                                 false))
                 (make-bst M1                               
                       false
                       (make-bst O1
                                 (make-bst N1 false
                                           (make-bst Z1 false false))
                                 (make-bst P1 false false))
                       )))
                                                          
(define BSTL1 (make-bst L1 false false))
(define BSTP1 (make-bst P1 false false))
(define BSTM1 (make-bst M1 BSTL1 false))
(define BSTO1 (make-bst O1 false BSTP1))
(define BSTpN1 (make-bst N1 BSTM1 BSTO1))

(define BSTL1w (make-bst L1 false false))
(define BSTP1w (make-bst P1 false false))
(define BSTM1w (make-bst M1 false false))
(define BSTO1w (make-bst O1 false BSTP1w))
(define BSTpN1w (make-bst N1 BSTM1w BSTO1w))




(define-struct db (field lt? eq? bst))

(define DB-quantity (make-db widget-quantity < = false))

(define DB-name (make-db widget-name string<? string=? BSTF1))
(define DB-name2 (make-db widget-name string<? string=? BSTpN1w))
(define DB-name3 (make-db widget-name string<? string=? BSTpN1))
(define DB-price (make-db widget-price < = BSTpri))
(define DB-price1 (make-db widget-price < = BSTpri1))




(check-expect (find "W1" DB-name) W1)    
(check-expect (find "A1" DB-name) A1)    
(check-expect (find "D1" DB-name) D1)    
(check-expect (find "Z1" DB-name) Z1)    
(check-expect (find "B1" DB-name) B1)    
(check-expect (find "F1" DB-name) F1)    
(check-expect (find "W2" DB-name) false) 

(define (find X DB)
  (if (boolean? (db-bst DB))
      (db-bst DB)
      (cond
        [((db-eq? DB) X ((db-field DB) (bst-widget (db-bst DB)))) (bst-widget (db-bst DB))]
        
        [((db-lt? DB) X ((db-field DB) (bst-widget (db-bst DB))))
         (find X (make-db (db-field DB) (db-lt? DB) (db-eq? DB) (bst-left (db-bst DB))))]
        
        [else (find X (make-db (db-field DB) (db-lt? DB) (db-eq? DB) (bst-right (db-bst DB))))]
        )
      )
  )




(define DB-empty (make-db widget-name string<? string=? false))
(define DB-emptyfill (make-db widget-name string<? string=? (make-bst D1 false false)))

(check-expect (db-bst (insert L1 DB-name2)) (db-bst DB-name3))           
(check-expect (db-bst (insert D1 DB-price1)) (db-bst DB-price))          
(check-expect (db-bst (insert D1 DB-empty)) (db-bst DB-emptyfill))       

(define (insert wid DB)
(local
  
  
  [(define (insert-name wid bst)
  (cond
    [(boolean? bst) (make-bst wid false false)]
    [((db-lt? DB) ((db-field DB) wid) ((db-field DB) (bst-widget bst)))
     (make-bst
      (bst-widget bst)
      (insert-name wid (bst-left bst))
      (bst-right bst))]
    [else (make-bst (bst-widget bst) (bst-left bst) (insert-name wid (bst-right bst)))]
    )
  )]
 (make-db (db-field DB) (db-lt? DB) (db-eq? DB) (insert-name wid (db-bst DB))))) 
  









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

