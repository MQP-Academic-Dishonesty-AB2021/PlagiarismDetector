

#reader(lib "htdp-advanced-reader.ss" "lang")((modname A5P3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))




(require 2htdp/image)

(define-struct bst (widget left right))




(define-struct widget (name quantity price))



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


(define BSTZ-c (make-bst Z1 false false))
(define BSTW-c (make-bst W1 false BSTZ-c))
(define BSTB-c (make-bst B1 false false))
(define BSTD-c (make-bst D1 BSTB-c BSTW-c))
(define dbBSTD-c (make-db widget-name string<? string=? BSTD-c))



(define BSTW-P (make-bst W1 false false))
(define BSTB-P (make-bst B1 BSTW-P false))
(define BSTZ-P (make-bst Z1 false false))
(define BSTD-P (make-bst D1 BSTB-P BSTZ-P))
(define dbnums (make-db widget-price < = BSTD-P))


(define BSTW-P-c (make-bst W1 false false))
(define BSTB-P-c (make-bst B1 BSTW-P-c false))
(define BSTZ-P-c (make-bst Z1 false false))
(define BSTD-P-c (make-bst D1 BSTB-P-c BSTZ-P-c))
(define dbnums-c (make-db widget-price < = BSTD-P-c))




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






(check-expect (db-bst (insert! aye dbBSTD)) (db-bst DB-AYE)) 
(check-expect (db-bst (insert! yurt dbBSTD-c)) (db-bst DB-YURT)) 
(check-expect (db-bst (insert! most-expensive dbnums)) (db-bst DB-most-expensive)) 
(check-expect (db-bst (insert! least-expensive dbnums-c)) (db-bst DB-least-expensive))

(define (insert! widget db)
  (local [ (define mut-bst (db-bst db))
           (define (insert-local widget field lt? eq? bst smaller-than-parent? parent-bst)
             (cond
               [(false? bst) 
                (begin (cond [smaller-than-parent?
                              (set-bst-left! parent-bst (make-bst widget
                                                                  false
                                                                  false)) ]
                             [(not smaller-than-parent?)
                              (set-bst-right! parent-bst (make-bst widget
                                                                   false
                                                                   false))]) 
                       mut-bst) 
                ] 
               [(smaller-or-same? widget field eq? bst)
                mut-bst] 
               [(smaller-or-same? widget field lt? bst)
                (insert-local widget field lt? eq? (bst-left bst) true bst)] 
               [else
                (insert-local  widget field lt? eq? (bst-right bst) false bst)])) 
          
           
           (define (smaller-or-same? widget field lt-or-eq? bst)
             (cond
               [(false? bst) false]
               [(lt-or-eq? (field widget) (field (bst-widget bst))) true]
               [else false]))]
    (begin (make-db (db-field db)
                    (db-lt? db)
                    (db-eq? db)
                    (insert-local widget
                                  (db-field db)
                                  (db-lt? db)
                                  (db-eq? db)
                                  (db-bst db)
                                  false
                                  false))
           db)))







(define w100    (make-widget "100" 100 100))
(define wnneg100(make-widget "-100" -100 -100))
(define wnneg20 (make-widget "-20" -20 -20))
(define wn5     (make-widget "5"    5   5))
(define wn7     (make-widget "7"    7   7))
(define wn10    (make-widget "10"  10  10))
(define wn13    (make-widget "13"  13  13))
(define wn15    (make-widget "15"  15  15))
(define wn20    (make-widget "20"  20  20))
(define wn100   (make-widget "100" 100 100))


(define bstneg100 (make-bst wnneg100 false false))
(define bst100 (make-bst w100 false false))
(define bst5 (make-bst wn5 bstneg100 false))
(define bst15 (make-bst wn15 false bst100))
(define bst10 (make-bst wn10 bst5 bst15))


(define db-base (make-db widget-price < = bst10))


(define bstneg20 (make-bst wnneg20 false false))
(define bst7 (make-bst wn7 false false))
(define bst13 (make-bst wn13 false false))
(define bst20 (make-bst wn20 false false))












(define bstneg100-t1 (make-bst wnneg100 false false))
(define bst5-t1 (make-bst wn5 false false))
(define bstneg20-t1 (make-bst wnneg20 bstneg100-t1 bst5-t1))

(define bst100-t1 (make-bst wn100 false false))
(define bst15-t1 (make-bst wn15 false bst100-t1))

(define bst10-t1 (make-bst wn10 bstneg20-t1 bst15-t1))










(define bstneg100-t2 (make-bst wnneg100 false false))
(define bst5-t2 (make-bst wn5 bstneg100-t2 false))

(define bst15-t2 (make-bst wn15 false false))
(define bst100-t2 (make-bst wn100 false false))
(define bst20-t2 (make-bst wn20 bst15-t2 bst100-t2))

(define bst10-t2 (make-bst wn10 bst5-t2 bst20-t2)) 










(define bstneg100-t3 (make-bst wnneg100 false false))
(define bst7-t3 (make-bst wn7 false false))
(define bst5-t3 (make-bst wn5 bstneg100-t3 bst7-t3))

(define bst100-t3 (make-bst wn100 false false))
(define bst15-t3 (make-bst wn15 false bst100-t3))

(define bst10-t3 (make-bst wn10 bst5-t3 bst15-t3))










(define bstneg100-t4 (make-bst wnneg100 false false))
(define bst5-t4 (make-bst wn5 bstneg100-t4 false))

(define bst13-t4 (make-bst wn13 false false))
(define bst100-t4 (make-bst wn100 false false))
(define bst15-t4 (make-bst wn15 bst13-t4 bst100-t4))

(define bst10-t4 (make-bst wn10 bst5-t4 bst15-t4))


(define db-false (make-db widget-price < = false))





(check-expect (db-bst (insert-avl wnneg20 db-base)) bst10-t1) 
(check-expect (db-bst (insert-avl wn20 db-base)) bst10-t2) 
(check-expect (db-bst (insert-avl wn7 db-base)) bst10-t3) 
(check-expect (db-bst (insert-avl wn13 db-base)) bst10-t4) 
(check-expect (db-bst (insert-avl wn10 db-false)) (make-bst wn10 false false)) 

(define (insert-avl widget db)
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
                         (balance (insert-local widget
                                                field
                                                lt?
                                                eq?
                                                (bst-left bst)))
                         (bst-right bst))] 
              [else
               (make-bst (bst-widget bst)
                         (bst-left bst)
                         (balance (insert-local widget
                                                field
                                                lt?
                                                eq?
                                                (bst-right bst))))])) 
          
          
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





(define (right-rotate bst)
  (local[(define new-root (bst-left bst))
         (define old-root (make-bst (bst-widget bst) (bst-right new-root) (bst-right bst)))
         (define root3 (make-bst (bst-widget new-root) (bst-left new-root) old-root))]
    root3))



(define (left-rotate bst)
  (local[(define new-root (bst-right bst))
         (define old-root (make-bst (bst-widget bst) (bst-left bst) (bst-left new-root)))
         (define root3 (make-bst (bst-widget new-root)  old-root (bst-right new-root)))]
    root3)) 






(define (balance bst)
  (cond
    [(>= (height-diff bst) 1)
     (cond
       [(>= (height (bst-left bst)) (height (bst-right bst)))
        (right-rotate bst)] 
       [else
        (right-rotate bst)])] 
    [(<= (height-diff bst) -1)
     (cond
       [(>= (height (bst-right bst)) (height (bst-left bst)))
        (left-rotate bst)] 
       [else
        (left-rotate bst)])] 
    [else bst])) 







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




(define (build-tree low db)
  (foldr insert db low))

(define (build-tree! low db)
  (foldr insert! db low))

(define (build-tree-avl low db)
  (foldr insert-avl db low))



(define (time-insert)
  (local [(define random-wids (random-widgets 250000 900000000))
          (define db (make-db widget-price < = bst10))
          (define db-2 (make-db widget-price < = bst10))
          ]
    (begin
      (time (build-tree random-wids db))
      (time (build-tree! random-wids db-2))
      "done")
    ))

 

(define (total-find low db)
  (cond [(empty? low) false]
        [else
         (begin (find ((db-field db)(first low)) db)
                (total-find (rest low) db))]))



(define (time-find)
  (local [(define random-wids (random-widgets 200000 900000))
          (define db (make-db widget-price < = bst10))
          (define db-2 (make-db widget-price < =  (db-bst (build-tree! random-wids db)))) 
          (define db-3 (insert-avl wn10 db-2))] 
    (begin
      (time (total-find random-wids db-2))
      (time (total-find random-wids db-3))
      "done")))




(define (insert-avl! widget db)
  (local [ (define mut-bst (db-bst db))
          (define (insert-local widget field lt? eq? bst smaller-than-parent? parent-bst)
            (cond 
              [(false? bst)
               (begin (cond [smaller-than-parent?
                             (set-bst-left! parent-bst (make-bst widget
                                                                 false
                                                                 false))]
                            [(not smaller-than-parent?)
                             (set-bst-right! parent-bst (make-bst widget
                                                                  false
                                                                  false))])
                      mut-bst)]
              [(smaller-or-same? widget field eq? bst)
               mut-bst] 
              [(smaller-or-same? widget field lt? bst)
               (make-bst (bst-widget bst)
                         (balance (insert-local widget
                                                field
                                                lt?
                                                eq?
                                                (bst-left bst)))
                         (bst-right bst))] 
              [else
               (make-bst (bst-widget bst)
                         (bst-left bst)
                         (balance (insert-local widget
                                                field
                                                lt?
                                                eq?
                                                (bst-right bst))))])) 
          
          
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







(define (height b)
  (local
    
    
    [(define (height-helper b d)
       (cond
         [(false? b) d]
         [else
          (max d
               (height-helper (bst-left b) (add1 d))
               (height-helper (bst-right b) (add1 d)))]))]
    (height-helper b 0)))
 




(define (height-diff b)
  (if (false? b)
      0
      (- (height (bst-left b))
         (height (bst-right b)))))

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