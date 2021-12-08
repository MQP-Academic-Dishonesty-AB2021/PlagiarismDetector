

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct widget (name quantity price))



(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define B1 (make-widget "B1" 10 32))

(define-struct bst (widget left right))



(define BST-NAME (make-bst W1 (make-bst A1 false
                                        (make-bst D1 false false))
                           (make-bst Z1 false false)))
(define BST-QUAN (make-bst D1
                           (make-bst A1 false false)
                           (make-bst Z1 false false)))
(define BST-PRICE (make-bst D1
                            (make-bst Z1 false false)
                            (make-bst A1 false (make-bst W1 false false))))





(define-struct db (field lessthan? equal? bst))
(define DB-NAME (make-db widget-name string<? string=? BST-NAME))
(define DB-QUAN (make-db widget-quantity < = BST-QUAN))
(define DB-PRICE (make-db widget-price > = BST-PRICE))




(check-expect (find "A1" DB-NAME) A1)    
(check-expect (find "Z1" DB-NAME) Z1)    
(check-expect (find "X1" DB-NAME) false) 

(check-expect (find 5 DB-QUAN) D1)       
(check-expect (find 51 DB-QUAN) Z1)      
(check-expect (find 1 DB-QUAN) false)    

(check-expect (find 16 DB-PRICE) Z1)     
(check-expect (find 5 DB-PRICE) D1)      
(check-expect (find 12 DB-PRICE) false)  

(define (find key db)
  (local[
         (define (find bst)
           (local [(define (get-field db)
                     ((db-field db) (bst-widget bst)))]
             (cond [(false? bst) false]
                   [else
                    (cond [((db-equal? db) key (get-field db)) (bst-widget bst)]
                          [((db-lessthan? db) key (get-field db))
                           (find (bst-left bst))]
                          [else (find (bst-right bst))])])))]
    (find (db-bst db))))


(check-expect (db-bst (insert W1 (make-db widget-name string<? string=? false)))  
             (make-bst W1 false false))

(check-expect (db-bst (insert W1 DB-QUAN)) 
             (make-bst D1
                           (make-bst A1 (make-bst W1 false false) false)
                           (make-bst Z1 false false)))
(check-expect (db-bst (insert B1 DB-PRICE)) 
             (make-bst D1
                            (make-bst Z1 (make-bst B1 false false) false)
                            (make-bst A1 false (make-bst W1 false false))))

(define (insert widget db)
  (local[
         (define (insert bst)
           (local [(define (get-field db)
                     ((db-field db) (bst-widget bst)))]
             (cond [(false? bst) (make-bst widget false false)]
                   [else
                    (if ((db-lessthan? db) ((db-field db) widget) (get-field db))
                        (make-bst (bst-widget bst)  
                                  (insert (bst-left bst)) (bst-right bst))
                        (make-bst (bst-widget bst)
                                  (bst-left bst) (insert (bst-right bst))))])))]
    (make-db (db-field db) (db-lessthan? db) (db-equal? db) (insert (db-bst db)))))





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


