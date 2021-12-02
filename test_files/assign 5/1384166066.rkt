

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define C1 (make-widget "C1" 12 12))
(define F1 (make-widget "F1" 12 22))
(define B1 (make-widget "B1" 10 3))
(define J1 (make-widget "J1" 4 12))


(define-struct bst (widget left right))




 

(define BST0 false)
(define BST1 (make-bst D1 (make-bst A1 false false)
                       (make-bst W1 false (make-bst Z1 false false)))) 
(define BST2 (make-bst W1 (make-bst D1 false false)
                       (make-bst Z1 false (make-bst Z1 false false))))
(define BST3 (make-bst A1 (make-bst W1 false false)
                       (make-bst Z1 (make-bst D1 false false) false)))
(define BST4 (make-bst D1 (make-bst C1
                                   (make-bst B1 false false) false) false)) 
(define BST5 (make-bst A1 false
                       (make-bst B1 false (make-bst D1 false false)))) 
(define BST6 (make-bst A1 false
                       (make-bst F1 (make-bst D1 false false) false))) 




(define-struct db (field lt? eq? bst))





(define DB-name (make-db widget-name string<? string=? BST1))  
(define DB-quantity (make-db widget-quantity < = BST3))
(define DB-price (make-db widget-price < = false))
(define DB-name2 (make-db widget-name string<? string=? BST4)) 
(define DB-name3 (make-db widget-name string<? string=? BST5)) 
(define DB-name4 (make-db widget-name string<? string=? BST6)) 





(check-expect (find "W1" DB-name) W1)     
(check-expect (find "W3" DB-name) false)  
(check-expect (find 1 DB-quantity) W1)    
(check-expect (find "W1" DB-price) false) 

(define (find val db)
  (cond
    [(false? (db-bst db)) false]
    [((db-eq? db) val ((db-field db) (bst-widget (db-bst db))))
     (bst-widget (db-bst db))]
    [((db-lt? db) val ((db-field db) (bst-widget (db-bst db))))
     (find val (make-db (db-field db) (db-lt? db) (db-eq? db)
                        (bst-left (db-bst db))))]
    [else
     (find val (make-db (db-field db) (db-lt? db) (db-eq? db)
                        (bst-right (db-bst db))))]))





(check-expect (db-bst (insert B1 DB-name))
           (db-bst (make-db (db-field DB-name) (db-lt? DB-name) (db-eq? DB-name)
                 (make-bst D1 (make-bst A1 false (make-bst B1 false false))
                  (make-bst W1 false (make-bst Z1 false false))))))

(check-expect (db-bst (insert B1 DB-quantity))
           (db-bst (make-db (db-field DB-name) (db-lt? DB-name) (db-eq? DB-name)
                     (make-bst A1 (make-bst W1 false false)
                                (make-bst Z1 (make-bst D1 false
                                                      (make-bst B1 false false))
                                          false)))))  

(check-expect (db-bst (insert B1 DB-price))   
              (db-bst (make-db (db-field DB-name)
                      (db-lt? DB-name) (db-eq? DB-name)
                                           (make-bst B1 false false))))

(define (insert widget db)

(local
  [(define (insert-widget widget bst)
  (cond
    [(false? bst) (make-bst widget false false)]
    [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
     (make-bst (bst-widget bst)
               (insert-widget widget (bst-left bst))
               (bst-right bst))]
    [else
     (make-bst (bst-widget bst)
               (bst-left bst)
               (insert-widget widget (bst-right bst)))]))]
  
     (make-db (db-field db) (db-lt? db) (db-eq? db)
              (insert-widget widget (db-bst db)))))





(define (build-tree low)
  (foldr insert-name false low))








(check-expect (smaller? "C1" BST1) true)    
(check-expect (smaller? "F1" BST1) false)   
(check-expect (smaller? "V1" BST2) true)    
(check-expect (smaller? "D1" BST1) false)   


(define (smaller? key bst)
  (cond [(false? bst) false]
        [(string<? key (widget-name (bst-widget bst))) true]
        [else false]))








(check-expect (same? "C1" BST1) false)    
(check-expect (same? "D1" BST1) true)     
(check-expect (same? "W1" BST2) true)     
(check-expect (same? "V1" BST2) false)    


(define (same? key bst)
  (cond [(false? bst) false]
        [(string=? key (widget-name (bst-widget bst))) true]
        [else false]))







(check-expect (find-name "D1" BST1) D1)    
(check-expect (find-name "W1" BST1) W1)    
(check-expect (find-name "C1" BST1) false) 
(check-expect (find-name "Z1" BST2) Z1)    
(check-expect (find-name "A1" BST0) false) 



(define (find-name key bst)
  (cond
    [(false? bst) false]
    [(smaller? key bst) (find-name key (bst-left bst))]
    [(same? key bst) (bst-widget bst)]
    [else
     (find-name key (bst-right bst))]))
                      





(check-expect (insert-name B1 BST1)             
              (make-bst D1
                        (make-bst A1 false (make-bst B1 false false))
                        (make-bst W1 false (make-bst Z1 false false))))
(check-expect (insert-name J1 BST1)             
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst W1 (make-bst J1 false false)
                                  (make-bst Z1 false false))))
(check-expect (insert-name A1 BST2)             
              (make-bst W1
                        (make-bst D1 (make-bst A1 false false) false)
                        (make-bst Z1 false (make-bst Z1 false false))))




(define (insert-name widget bst)
  (cond
    [(false? bst) (make-bst widget false false)]
    [(smaller? (widget-name widget) bst)
     (make-bst (bst-widget bst)
               (insert-name widget (bst-left bst))
               (bst-right bst))]
    [else
     (make-bst (bst-widget bst)
               (bst-left bst)
               (insert-name widget (bst-right bst)))]))








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