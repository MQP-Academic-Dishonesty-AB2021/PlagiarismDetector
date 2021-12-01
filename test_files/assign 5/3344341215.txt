

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Part 3 Daniel B Benjamin S final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct bst (widget left right))




(define-struct widget (name quantity price))
(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define H1 (make-widget "H1" 20 20))
(define P1 (make-widget "P1" 0 0))
(define ZZ1 (make-widget "ZZ1" 100 100))
(define M1 (make-widget "M1" -1 20))
(define B1 (make-widget "B1" 150 12))




(define-struct db (field lt? eq? bst))

(define db-name (make-db widget-name string<? string=?
                         (make-bst D1 (make-bst A1 false false)
                                   (make-bst W1 false (make-bst Z1 false false)))))
(define db-quantity (make-db widget-quantity < =
                             (make-bst D1 (make-bst A1 (make-bst W1 false false) false)
                                       (make-bst Z1 false false))))
(define db-price (make-db widget-price > =
                          (make-bst D1 
                                    false
                                    (make-bst A1 false (make-bst W1 false false)))))
(define db-balanced (make-db widget-price > =
                             (make-bst D1 
                                       false
                                       (make-bst A1 false
                                                 (make-bst W1 false false))))) 
(define db-ll (make-db widget-quantity < =
                       (make-bst D1 (make-bst A1 (make-bst W1 false false) false)
                                 (make-bst Z1 false false)))) 
(define db-rr (make-db widget-name string<? string=?
                       (make-bst D1 (make-bst A1 false false)
                                 (make-bst W1 false (make-bst Z1 false false))))) 

(define db-lr (make-db widget-quantity < =
                       (make-bst D1 (make-bst W1
                                              (make-bst P1 false false)
                                              (make-bst A1 false false))
                                 (make-bst Z1 false false))))
(define db-rl (make-db widget-quantity < =
                       (make-bst D1 (make-bst W1 false
                                              false)
                                 (make-bst Z1 (make-bst H1 false false)
                                           (make-bst ZZ1 false false)))))
  


(define (insert widget db) 
  (local [(define (helper widget bst)
            (cond
              [(false? bst) (make-bst widget false false)]
              [else (if ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst))) 
                        (make-bst (bst-widget bst) (helper widget (bst-left bst)) (bst-right bst))
                        (make-bst (bst-widget bst) (bst-left bst) (helper widget (bst-right bst))))]
              )
            )]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (helper widget (db-bst db)))
    ))









(define (insert! widget db) 
  (local [(define (compare? widget db)
            ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget (db-bst db)))))
          (define (insert-helper widget bst original-bst)
            (cond
              [(false? bst) (make-bst widget false false)]
              [else (if ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
                        (if (false? (bst-left bst))
                            (begin (set-bst-left! bst (make-bst widget false false)) original-bst)
                            (insert-helper widget (bst-left bst) original-bst))
                        (if (false? (bst-right bst))
                            (begin (set-bst-right! bst (make-bst widget false false)) original-bst)
                            (insert-helper widget (bst-right bst) original-bst)))]
              )
            )]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert-helper widget (db-bst db) (db-bst db)))))

  
 
                                                                            




(define bstLL (make-bst 5 (make-bst 3 (make-bst 2 false false) false) false))
(define bstRR (make-bst 2 false (make-bst 3 false (make-bst 5 false false))))
(define bstLR (make-bst 10 (make-bst 5 (make-bst 3 (make-bst 2 false false) false)
                                     (make-bst 8 false false)) (make-bst 15 false false)))
(define bstRL (make-bst 10 (make-bst 5 false false)
                        (make-bst 15 (make-bst 12 false false)
                                  (make-bst 20 false (make-bst 25 false false)))))
(define balanced-tree (make-bst 5 (make-bst 3 (make-bst 2 false false) (make-bst 4 false false))
                                (make-bst 8 false false)))






(define (insert-avl widget db)
  (local [(define (balanced? bst)
            (cond
              [(false? bst) true]
              [else (and (or (= (height-diff bst) -1)
                             (= (height-diff bst) 1) (= (height-diff bst) 0))
                         (balanced? (bst-left bst)) (balanced? (bst-right bst)))]))
          
          (define (left-left? bst)
            (and (= 2 (height-diff bst)) (= 1 (height-diff (bst-left bst)))
                 (= 0 (height-diff (bst-left (bst-left bst))))))

          (define (left-left-rot bst)
            (make-bst (bst-widget (bst-left bst)) (bst-left (bst-left bst))
                      (make-bst (bst-widget bst) false false)))
          
          
          (define (left-right? bst)
            (and (= 2 (height-diff bst))
                 (balanced? (bst-left bst)) (not (false? (bst-right (bst-left bst))))))
          
          (define (left-right-rot bst)
            (balance (make-bst (bst-widget (bst-right (bst-left bst)))
                               (make-bst (bst-widget (bst-left bst))
                                         (bst-left (bst-left bst)) false)
                               (make-bst (bst-widget bst) false (bst-right bst))))) 



            
          
          (define (right-left? bst)
            (and (= -2 (height-diff bst))  (balanced? (bst-right bst)) 
                 (not (false? (bst-left (bst-right bst))))))
          
          (define (right-left-rot bst)
            (balance (make-bst (bst-widget (bst-left (bst-right bst)))
                               (make-bst (bst-widget bst) (bst-left bst) false)
                               (make-bst (bst-widget (bst-right bst)) false
                                         (bst-right (bst-right bst))))))
                                     
          
          (define (right-right? bst)
            (and (= -2 (height-diff bst)) (= -1 (height-diff (bst-right bst)))
                 (= 0 (height-diff (bst-right (bst-right bst))))))
          
          (define (right-right-rot bst)
            (make-bst (bst-widget (bst-right bst)) (make-bst (bst-widget bst) false false)
                      (bst-right (bst-right bst))))
          
          (define (balance bst)
            (cond
              [(balanced? bst) bst]
              [else
               (cond [(left-left? bst) (left-left-rot bst)]
                     [(left-right? bst) (left-right-rot bst)]
                     [(right-left? bst) (right-left-rot bst)]
                     [(right-right? bst) (right-right-rot bst)]
                     [else 
                      (make-bst (bst-widget bst) (balance (bst-left bst))
                                (balance (bst-right bst)))])]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (balance (db-bst (insert! widget db))))))

  
  
 
  
  


(define test-db (make-db widget-quantity < = false))

(define (build-low size)
  (local [
          (define (random-widget x)
            (make-widget (number->string (random 100000)) (random 100000) (random 100000)))]
    (build-list size random-widget)))

(define (insert-widgets low db)
  (local [(define (helper low bst)
            (cond [(empty? low) bst]
                  [else (db-bst (insert (first low) (insert-widgets (rest low) db)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (helper low (db-bst db)))))

(define (insert!-widgets low db)
  (local [(define (helper low bst)
            (cond [(empty? low) bst]
                  [else (db-bst (insert! (first low) (insert!-widgets (rest low) db)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (helper low (db-bst db)))))

(define (insert-avl-widgets low db)
  (local [(define (helper low bst)
            (cond [(empty? low) bst]
                  [else (db-bst (insert-avl (first low) (insert-avl-widgets (rest low) db)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (helper low (db-bst db)))))
 
(define (find widget db)
  (cond
    [(false? (db-bst db)) false]
    [((db-eq? db) ((db-field db) widget) ((db-field db) (bst-widget (db-bst db)))) widget]
    [else (if (not (false? (find widget (make-db (db-field db) (db-lt? db)
                                                 (db-eq? db) (bst-left (db-bst db))))))
              widget
              (find widget (make-db (db-field db) (db-lt? db) (db-eq? db)
                                    (bst-right (db-bst db)))))]
    )
  )
(define (find-all low db)
  (cond [(empty? low) "done"]
        [else (begin (find (first low) db) (find-all (rest low) db))]))



 






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




(define tenk-1 (build-low 10000))
(define tenk-2 (build-low 10000))
(define tenk-3 (build-low 10000))
(define tenk-4 (build-low 10000))
(define tenk-5 (build-low 10000))
(define tenk-6 (build-low 10000))
(define tenk-7 (build-low 10000))
(define tenk-8 (build-low 10000))
(define tenk-9 (build-low 10000))
(define tenk-10 (build-low 10000))
(define test-db1 test-db)
(define test-db2 test-db)
(define test-db3 test-db)
(define test-db4 test-db)
(define test-db5 test-db)
(define test-db6 test-db)
(define test-db7 test-db)
(define test-db8 test-db)
(define test-db9 test-db)
(define test-db10 test-db)

(begin (local [(define low tenk-1) (define db (insert-widgets low test-db1))]
         (time (find-all low db))) "trial 1-bst complete")

(begin (local [(define low  tenk-2) (define db (insert-widgets low test-db2))]
         (time (find-all low db))) "trial 2-bst complete")

(begin (local [(define low  tenk-3) (define db (insert-widgets low test-db3))]
         (time (find-all low db))) "trial 3-bst complete")

(begin (local [(define low  tenk-4) (define db (insert-widgets low test-db4))]
         (time (find-all low db))) "trial 4-bst complete")

(begin (local [(define low  tenk-5) (define db (insert-widgets low test-db5))]
         (time (find-all low db))) "trial 5-bst complete")
(begin (local [(define low  tenk-6) (define db (insert!-widgets low test-db6))]
         (time (find-all low db))) "trial 6-bst complete")
(begin (local [(define low  tenk-7) (define db (insert!-widgets low test-db7))]
         (time (find-all low db))) "trial 7-bst complete")
(begin (local [(define low  tenk-8) (define db (insert!-widgets low test-db8))]
         (time (find-all low db))) "trial 8-bst complete")
(begin (local [(define low  tenk-9) (define db (insert!-widgets low test-db9))]
         (time (find-all low db))) "trial 9-bst complete")
(begin (local [(define low  tenk-10) (define db (insert!-widgets low test-db10))]
         (time (find-all low db))) "trial 10-bst complete") 


(begin (local [(define low tenk-1) (define db (insert-avl-widgets low test-db1))]
         (time (find-all low db))) "trial 1-avl complete")

(begin (local [(define low  tenk-2) (define db (insert-avl-widgets low test-db2))]
         (time (find-all low db))) "trial 2-avl complete")

(begin (local [(define low  tenk-3) (define db (insert-avl-widgets low test-db3))] 
         (time (find-all low db))) "trial 3-avl complete")

(begin (local [(define low  tenk-4) (define db (insert-avl-widgets low test-db4))]
         (time (find-all low db))) "trial 4-avl complete")

(begin (local [(define low  tenk-5) (define db (insert-avl-widgets low test-db5))]
         (time (find-all low db))) "trial 5-avl complete")

(begin (local [(define low  tenk-6) (define db (insert-avl-widgets low test-db6))]
         (time (find-all low db))) "trial 6-avl complete")
(begin (local [(define low  tenk-7) (define db (insert-avl-widgets low test-db7))]
         (time (find-all low db))) "trial 7-avl complete")
(begin (local [(define low  tenk-8) (define db (insert-avl-widgets low test-db8))]
         (time (find-all low db))) "trial 8-avl complete")
(begin (local [(define low  tenk-9) (define db (insert-avl-widgets low test-db9))]
         (time (find-all low db))) "trial 9-avl complete")
(begin (local [(define low  tenk-10) (define db (insert-avl-widgets low test-db10))]
         (time (find-all low db))) "trial 10-avl complete") 
