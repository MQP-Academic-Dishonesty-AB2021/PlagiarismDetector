

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname A5P2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






(require 2htdp/image)



(define-struct widget (name quantity price))




(define-struct bst (widget left right))












(define-struct db (field lt? eq? bst))







(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define B1 (make-widget "B1" 2 3))
(define D1 (make-widget "D1" 5 5))




(define BSTZ (make-bst Z1 false false))
(define BSTW (make-bst W1 false BSTZ))
(define BSTB (make-bst B1 false false))
(define BSTD (make-bst D1 BSTB BSTW))

(define dbBSTD (make-db widget-name string<? string=? BSTD))



(define BSTW-P (make-bst W1 false false))
(define BSTB-P (make-bst B1 BSTW-P false))
(define BSTZ-P (make-bst Z1 false false))
(define BSTD-P (make-bst D1 BSTB-P BSTZ-P))
(define dbnums (make-db widget-price < = BSTD-P))








(check-expect (find "Z1" dbBSTD) Z1) 
(check-expect (find "W1" dbBSTD) W1) 
(check-expect (find "WeHeartBeck" dbBSTD) false) 

(check-expect (find 16 dbnums) Z1) 
(check-expect (find  3 dbnums) B1) 
(check-expect (find 21 dbnums) false) 

(define (find x db)
  (cond
    [(false? (db-bst db)) false]
    [((db-eq? db) x ((db-field db) (bst-widget (db-bst db))))
     (bst-widget (db-bst db))]
    [((db-lt? db) x ((db-field db) (bst-widget (db-bst db))))
     (find x (make-db (db-field db)
                      (db-lt? db)
                      (db-eq? db)
                      (bst-left (db-bst db))))] 
    [else
     (find x (make-db (db-field db)
                      (db-lt? db)
                      (db-eq? db)
                      (bst-right (db-bst db))))])) 








(define yurt (make-widget "YURT" 1 1))
(define BSTY-check (make-bst yurt false false))
(define BSTZ-check (make-bst Z1 BSTY-check false))
(define BSTW-check (make-bst W1 false BSTZ-check))
(define BSTB-check (make-bst B1 false false))
(define BSTD-yurt-check (make-bst D1 BSTB-check BSTW-check))
(define DB-YURT (make-db widget-name string<? string=? BSTD-yurt-check) ) 

(define aye (make-widget "AYE" 3 4))
(define BSTZ-aye-check (make-bst Z1 false false))
(define BSTA-aye-check (make-bst aye false false))
(define BSTW-aye-check (make-bst W1 false BSTZ-aye-check))
(define BSTB-aye-check (make-bst B1 BSTA-aye-check false))
(define BSTD-aye-check (make-bst D1 BSTB-aye-check BSTW-aye-check))
(define DB-AYE (make-db widget-name string<? string=? BSTD-aye-check)) 



(define most-expensive (make-widget "EXPENSIVE" 3 24))
(define BST-expensive (make-bst most-expensive false false))
(define BSTW-P-insert(make-bst W1 false false))
(define BSTB-P-insert (make-bst B1 BSTW-P-insert false))
(define BSTZ-P-insert (make-bst Z1 false BST-expensive))
(define BSTD-P-insert (make-bst D1 BSTB-P-insert BSTZ-P-insert))
(define DB-most-expensive (make-db widget-price < = BSTD-P-insert)) 

(define least-expensive (make-widget "INEXPENSIVE" 3 0))
(define BST-inexepnsive (make-bst least-expensive false false))
(define BSTW-P-insert-inexpensive (make-bst W1 BST-inexepnsive false))
(define BSTB-P-insert-inexpensive (make-bst B1 BSTW-P-insert-inexpensive false))
(define BSTZ-P-insert-inexpensive (make-bst Z1 false false))
(define BSTD-P-insert-inexpensive (make-bst D1 BSTB-P-insert-inexpensive BSTZ-P-insert-inexpensive))
(define DB-least-expensive (make-db widget-price < = BSTD-P-insert-inexpensive)) 










(check-expect (db-bst(insert aye dbBSTD)) (db-bst DB-AYE)) 
(check-expect (db-bst(insert yurt dbBSTD)) (db-bst DB-YURT)) 
(check-expect (db-bst(insert most-expensive dbnums)) (db-bst DB-most-expensive)) 
(check-expect (db-bst(insert least-expensive dbnums)) (db-bst DB-least-expensive)) 
(check-expect (db-bst(insert least-expensive DB-least-expensive)) (db-bst DB-least-expensive)) 


(define (insert widget db)
  (local [
          (define (insert-local widget field lt? eq? bst)
            (cond 
              [(false? bst)
               (make-bst widget
                         false
                         false)] 
              [(smaller-or-same? widget field eq? bst)
               bst] 
              [(smaller-or-same? widget field lt? bst)
               (make-bst (bst-widget bst)
                         (insert-local widget field lt? eq? (bst-left bst))
                         (bst-right bst))] 
              [else
               (make-bst (bst-widget bst)
                         (bst-left bst)
                         (insert-local  widget field lt? eq? (bst-right bst)))])) 
          
          
          (define (smaller-or-same? widget field lt-or-eq? bst)
            (cond
              [(false? bst) false]
              [(lt-or-eq? (field widget) (field (bst-widget bst))) true]
              [else false]))]
    
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert-local widget
                           (db-field db)
                           (db-lt? db)
                           (db-eq? db)
                           (db-bst db))))) 










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