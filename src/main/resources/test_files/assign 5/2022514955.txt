

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct bst (widget left right))




(define-struct widget (name quantity price))




(define-struct db (field lt? eq? bst))



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


(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define C1 (make-widget "C1" 10 10))
(define MANY (make-widget "MANY" 500 500))

(define BST0 (make-bst W1 false false))
(define BSTZ (make-bst Z1 false false))
(define BST1 (make-bst Z1 BST0 false))
(define BST2 (make-bst A1 false false))
(define BST3 (make-bst D1 BST2 BST1))
(define BSTC (make-bst C1 false false))
(define BST4 (make-bst D1 (make-bst A1 false BSTC) BST1))
(define BSTM (make-bst MANY false false))

(define BSTQ (make-bst D1 (make-bst A1 BST0 false)
                       (make-bst Z1 BSTC false)))
(define BSTQ2 (make-bst D1 (make-bst A1 BST0 false)
                        (make-bst Z1 BSTC BSTM)))

(define BSTP (make-bst D1 (make-bst Z1 false BSTC)
                       (make-bst A1 false BST0)))
(define BSTP2 (make-bst D1 (make-bst Z1 BSTM BSTC)
                        (make-bst A1 false BST0)))
(define AVL0 (make-bst Z1 (make-bst W1 BSTC false) false))
(define AVL1 (make-bst W1 BSTC BSTZ))


(define DB0 (make-db widget-name string<? string=? false))
(define DB00 (make-db widget-name string<? string=? false))
(define DB-name (make-db widget-name string<? string=? BST4))
(define DB-name2 (make-db widget-name string<? string=? BST3))
(define DB-quantity (make-db widget-quantity < = BSTQ))
(define DB-AVL (make-db widget-name string<? string=? BST1))

(define DB-price (make-db widget-price > = false))
(define DB-price2 (make-db widget-price > = BSTP))


(define 3W1 (make-widget "3W1" 1 1))
(define 3W2 (make-widget "3W2" 2 1))
(define 3W3 (make-widget "3W3" 3 1))
(define 3W4 (make-widget "3W4" 4 1))
(define 3W5 (make-widget "3W5" 5 1))
(define 3W6 (make-widget "3W6" 6 1))

(define 3B1 (make-bst 3W1 false false))
(define 3B2 (make-bst 3W2 false false))
(define 3B4 (make-bst 3W4 false false))
(define 3B3 (make-bst 3W3 3B2 3B4))
(define 3B6 (make-bst 3W6 false false))
(define 3B5 (make-bst 3W5 3B3 3B6))

(define 4B2 (make-bst 3W2 3B1 false))
(define 4B5 (make-bst 3W5 3B4 3B6))
(define 4B3 (make-bst 3W3 4B2 4B5))

(define 3DB1 (make-db widget-quantity < = 3B5))

(define 5B3 (make-bst 3W3 false false))
(define 5B4 (make-bst 3W4 false false))
(define 5B5 (make-bst 3W5 5B4 3B6))
(define 5B2 (make-bst 3W2 3B1 5B5))

(define 6B2 (make-bst 3W2 3B1 5B3))
(define 6B5 (make-bst 3W5 false 3B6))
(define 6B4 (make-bst 3W4 6B2 6B5))

(define 3DB2 (make-db widget-quantity < = 5B2))

(define 7B3 (make-bst 3W3 3B1 false))

(define 8B2 (make-bst 3W2 3B1 5B3))

(define 3DB3 (make-db widget-quantity < = 7B3))





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


(define (find key db)
  (local [
          (define (same? key bst)
            ((db-eq? db) key ((db-field db) (bst-widget bst))))

          (define (smaller? key bst)
            ((db-lt? db) key ((db-field db) (bst-widget bst))))

          (define (find-key bst)
            (cond [(false? bst) false]
                  [(same? key bst) (bst-widget bst)]
                  [(smaller? key bst)
                   (find-key (bst-left bst))]
                  [else
                   (find-key (bst-right bst))]))
          ]
    (find-key (db-bst db))))

(define (insert item db)
  (local [
          (define (smaller? item bst)
            ((db-lt? db) ((db-field db) item)
                         ((db-field db) (bst-widget bst))))


          (define (insert-item bst)
            (cond [(false? bst) (make-bst item false false)]
                  [(smaller? item bst)
                   (make-bst (bst-widget bst)
                             (insert-item (bst-left bst))
                             (bst-right bst))]
                  [else
                   (make-bst (bst-widget bst)
                             (bst-left bst)
                             (insert-item (bst-right bst)))]))
          ]
    (make-db (db-field db) (db-lt? db) (db-eq? db)
             (insert-item (db-bst db)))))



(define (insert! item db)
  (local [
          (define (smaller? item bst)
            ((db-lt? db) ((db-field db) item)
                         ((db-field db) (bst-widget bst))))

          (define (if-helper fn fn! bst)
            (if (false? (fn bst))
                (fn! bst (make-bst item false false))
                (insert-item (fn bst))))

          (define (insert-item bst)
            (cond [(false? bst) (set-db-bst! db (make-bst item false false))]
                  [(smaller? item bst)
                   (if-helper bst-left set-bst-left! bst)]
                  [else
                   (if-helper bst-right set-bst-right! bst)]))
          ]
    (begin (insert-item (db-bst db))
           db)))


(check-expect (db-bst (insert! W1 DB00)) BST0)            
(check-expect (db-bst (insert! C1 DB-name2)) BST4)       
(check-expect (db-bst (insert! MANY DB-quantity)) BSTQ2) 
(check-expect (db-bst (insert! MANY DB-price2)) BSTP2)   



(define (insert-avl item db)
  (local [
          (define (smaller? item bst)
            ((db-lt? db) ((db-field db) item)
                         ((db-field db) (bst-widget bst))))

          (define (insert-bst bst tree)
            (cond [(false? tree) bst]
                  [(smaller? (bst-widget bst) tree)
                   (make-bst (bst-widget tree)
                             (insert-bst bst (bst-left tree))
                             (bst-right tree))]
                  [else
                   (make-bst (bst-widget tree)
                             (bst-left tree)
                             (insert-bst bst (bst-right tree)))]))

          (define (balance b)
            (local [
                    (define (left-left b)
                      (local ((define output
                                (make-bst (bst-widget (bst-left b))
                                          (bst-left (bst-left b))
                                          (make-bst (bst-widget b) false (bst-right b)))))
                        (if (false? (bst-right (bst-left b)))
                            output
                            (insert-bst (bst-right (bst-left b)) output))))

                    (define (left-right b)
                      (local ((define output
                                (make-bst (bst-widget b)
                                          (make-bst (bst-widget (bst-right (bst-left b)))
                                                    (make-bst (bst-widget (bst-left b))
                                                                 (bst-left (bst-left b)) false)
                                                    (bst-right (bst-right (bst-left b))))
                                          (bst-right b))))
                        (if (false? (bst-left (bst-right (bst-left b))))
                            (left-left output)
                            (left-left (insert-bst (bst-left (bst-right (bst-left b))) output)))))

                    (define (right-right b)
                      (local ((define output
                                (make-bst (bst-widget (bst-right b))
                                          (make-bst (bst-widget b) (bst-left b) false)
                                          (bst-right (bst-right b)))))
                        (if (false? (bst-left (bst-right b)))
                            output
                            (insert-bst (bst-left (bst-right b)) output))))

                    (define (right-left b)
                      (local ((define output
                                (make-bst (bst-widget b)
                                          (bst-left b)
                                          (make-bst (bst-widget (bst-left (bst-right b)))
                                                    (bst-left (bst-left (bst-right b)))
                                                    (make-bst (bst-widget (bst-right b))
                                                                 false
                                                                 (bst-right (bst-right b)))))))
                        (if (false? (bst-right (bst-left (bst-right b))))
                            (right-right output)
                            (right-right (insert-bst (bst-right (bst-left (bst-right b)))
                                                     output)))))

                    ]
              (cond [(> (height-diff b) 1)
                     (if (> (height-diff (bst-left b)) 0)
                         (left-left b)
                         (left-right b))]
                    [(< (height-diff b) -1)
                     (if (> (height-diff (bst-right b)) 0)
                         (right-left b)
                         (right-right b))]
                    [else b])))

          (define (insert-item bst)
            (cond [(false? bst) (make-bst item false false)]
                  [(smaller? item bst)
                   (balance (make-bst (bst-widget bst)
                                      (insert-item (bst-left bst))
                                      (bst-right bst)))]
                  [else
                   (balance (make-bst (bst-widget bst)
                                      (bst-left bst)
                                      (insert-item (bst-right bst))))]))
          ]
    (make-db (db-field db) (db-lt? db) (db-eq? db)
             (insert-item (db-bst db)))))

(check-expect (db-bst (insert-avl W1 DB0)) BST0)
(check-expect (db-bst (insert-avl C1 DB-AVL)) AVL1)   
(check-expect (db-bst (insert-avl 3W2 3DB3)) 8B2)     
(check-expect (db-bst (insert-avl 3W1 3DB1)) 4B3)     
(check-expect (db-bst (insert-avl 3W3 3DB2)) 6B4)     



(define (time-insert)
  (local ((define widgets (random-widgets-string 250000 7 500000))

          (define (make-tree fn low db)
            (cond [(empty? low) db]
                  [else (make-tree fn (rest low) (fn (first low) db))]))

          (define DB (make-db widget-name string<? string=? false))
          )
    (begin (time (make-tree insert widgets DB))
           (time (make-tree insert! widgets DB))
           "done")))

(define (time-find)
  (local ((define widgets (random-widgets-string 10000 6 20000))

          (define DB (make-db widget-name string<? string=? false))

          (define (make-tree fn low db)
            (cond [(empty? low) db]
                  [else (make-tree fn (rest low) (fn (first low) db))]))

          (define (find-every low db)
            (cond [(empty? low) "done"]
                  [else (begin (find ((db-field db) (first low)) db)
                               (find-every (rest low) db))]))

          (define (repeat-find low db times)
            (cond [(= 0 times) "done"]
                  [else (begin (find-every low db)
                               (repeat-find low db (- times 1)))]))

          (define BST (make-tree insert widgets DB))
          (define AVL (make-tree insert-avl widgets DB))

          )
    (begin (time (find-every widgets BST))
           (time (find-every widgets AVL))
           (time (repeat-find widgets BST 10))
           (time (repeat-find widgets AVL 10))
           )))





 



 
 
 




