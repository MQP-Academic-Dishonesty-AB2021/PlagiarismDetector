

#reader(lib "htdp-advanced-reader.ss" "lang")((modname starter_pt3-1v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))





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

(define-struct bst (widget left right))




(define-struct widget (name quantity price))




(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16)) 
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 6))
(define G1 (make-widget "G1" 14 9))
(define B1 (make-widget "B1" 8 2))



(define BSTname (make-bst D1
                          (make-bst A1
                                    false
                                    false)
                          (make-bst W1
                                    false
                                    (make-bst Z1 false false))))

(define BSTquantity (make-bst D1
                              (make-bst A1
                                        (make-bst W1 false false)
                                        false)
                              (make-bst G1
                                        false
                                        (make-bst Z1 false false))))

(define BSTprice (make-bst D1
                           (make-bst A1
                                     (make-bst W1 false false)
                                     false)
                           (make-bst G1
                                     false
                                     (make-bst Z1 false false))))

(define BSTrr (make-bst
               A1
               false
               (make-bst D1 false (make-bst Z1 false false))     
               )
  )

(define BSTrl (make-bst
               A1
               false
               (make-bst D1 (make-bst B1 false false) false)     
               )
  )

(define BSTll (make-bst
               Z1
               (make-bst B1 (make-bst A1 false false) false)
               false
               )     
               
  )

(define BSTlr (make-bst
               Z1
               (make-bst B1  false (make-bst G1 false false))
               false     
               )
  )

(define BSTll2 (make-bst
                Z1
                (make-bst B1 false false)
                false
                )     
               
  )

(define BSTbig (make-bst
                B1
                (make-bst A1 false false)
                (make-bst G1
                          (make-bst D1 false false)
                          (make-bst W1 false
                                    (make-bst Z1 false false)))
                )     
               
  )

(define ZZ1 (make-widget "ZZ1" 521 236))

(define K1 (make-widget "K1" 1 1))
(define P1 (make-widget "P1" 51 16)) 
(define V1 (make-widget "V1" 2 3))
(define U1 (make-widget "U1" 5 6))
(define E1 (make-widget "E1" 14 9))
(define C1 (make-widget "C1" 8 2))

(define-struct db (field lt? eq? bst))


(define DBname (make-db widget-name string<=? string=? BSTname))

(define DBquantity (make-db widget-quantity < = BSTquantity))

(define DBprice (make-db widget-price < = BSTprice))

(define DBLL (make-db widget-name string<=? string=? BSTll2))

(define DBbig (make-db widget-name string<=? string=? BSTbig))

(define DBempty (make-db widget-name string<=? string=? false)) 










(check-expect (db-bst (insert! B1 DBname))
              (make-bst D1
                        (make-bst A1
                                  false
                                  (make-bst B1
                                            false
                                            false))
                        (make-bst W1
                                  false
                                  (make-bst Z1 false false))))
(check-expect (db-bst (insert! B1 DBquantity))
              (make-bst D1
                        (make-bst A1
                                  (make-bst W1 false false)
                                  false)
                        (make-bst G1
                                  (make-bst B1
                                            false
                                            false)
                                  (make-bst Z1 false false))))
(check-expect (db-bst (insert! B1 DBprice))
              (make-bst D1
                        (make-bst A1
                                  (make-bst W1 false (make-bst B1 false false))
                                  false)
                        (make-bst G1
                                  false
                                  (make-bst Z1 false false))))


 
(define (insert! widget db)
  (local
    [
     (define (insertBst widget bst lastnode wentRight?)
       (cond [(false? bst) (if (false? wentRight?)
                               (set-bst-left! lastnode (make-bst widget false false))
                               (set-bst-right! lastnode (make-bst widget false false))
                               )]
             
             [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
              (insertBst widget (bst-left bst) bst false) 
              ]
             
             [(false? ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst))))
              (insertBst widget (bst-right bst) bst true)
              ]
             )
       )
     ]


    (begin (set-db-bst! db
                 (begin
                   (insertBst widget (db-bst db) false false)
                   (db-bst db)
                   )
                 )
           db)
    
    )
  )










(check-expect (db-bst (insert-avl A1 DBLL))
              (make-bst
               B1
               (make-bst A1 false false)
               (make-bst Z1 false false)))


(check-expect (db-bst (insert-avl ZZ1 DBbig))
              (make-bst
               G1
               (make-bst
                B1
                (make-bst
                 A1
                 false
                 false)
                (make-bst
                 D1
                 false
                 false))
               (make-bst
                Z1
                (make-bst
                 W1
                 false
                 false)
                (make-bst
                 ZZ1
                 false
                 false)))
              )



(define (insert-avl widget db)
  (local
    [
     (define (insertBst widget bst)
       (cond [(false? bst) (make-bst widget false false)]
             [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
              (balance (make-bst (bst-widget bst)
                                 (balance (insertBst widget (bst-left bst)))
                                 (bst-right bst)
                                 ))
              ]
             [(false? ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst))))
              (balance (make-bst (bst-widget bst)
                                 (bst-left bst)
                                 (balance (insertBst widget (bst-right bst)))
                                 ))
              ]
             )
       )
     ]
    
    (begin (set-db-bst!  db (insertBst widget (db-bst db)))
             db
             ) 
    
   
    )
  )





(check-expect (balance BSTrl)(make-bst
                             B1
                             (make-bst A1 false false)
                             (make-bst D1 false false)       
                             ))
(check-expect (balance BSTlr)(make-bst
                             G1
                             (make-bst B1  false false)
                             (make-bst Z1 false false )))

(check-expect (balance (make-bst
                             G1
                             (make-bst B1  false false)
                             (make-bst Z1 false false )))
              (make-bst
                             G1
                             (make-bst B1  false false)
                             (make-bst Z1 false false )))




(define (balance bst)
  (cond [(< (height-diff bst) -1)
         (fixRight bst)
         ]
        [(> (height-diff bst) 1)
         (fixLeft bst)
         ]
        [else bst]
        )
  )





(check-expect (fixRight BSTrl)(make-bst
                             B1
                             (make-bst A1 false false)
                             (make-bst D1 false false)       
                             ))
(check-expect (fixRight BSTrr)(make-bst
                             D1
                             (make-bst A1 false false)
                             (make-bst Z1 false false)       
                             ))


(define (fixRight bst)
  
  (if (> (height (bst-right (bst-right bst))) (height (bst-left (bst-right bst))))
      
      (RRbst bst)
      
      (RLbst bst)
      )
  )




(check-expect (RRbst BSTrr) (make-bst
                             D1
                             (make-bst A1 false false)
                             (make-bst Z1 false false)       
                             ))


(define (RRbst bst)
  (local
    [
     (define rightrightNode (bst-right (bst-right bst)))
     (define rightNode (bst-right bst))

     (define (fix bst)
       (make-bst
        (bst-widget rightNode)
        (make-bst (bst-widget bst) (bst-left bst) (bst-left rightNode))
        rightrightNode
        )
       )
     ]
    (fix bst)
    )
  )




(check-expect (RLbst BSTrl) (make-bst
                             B1
                             (make-bst A1 false false)
                             (make-bst D1 false false)       
                             ))


(define (RLbst bst)
  (local
    [
     (define rightleftNode (bst-left (bst-right bst)))
     (define rightNode (bst-right bst))

     (define (fix bst)
       (make-bst
        (bst-widget rightleftNode)
        (make-bst (bst-widget bst) (bst-left bst) (bst-left rightleftNode))
        (make-bst (bst-widget rightNode) (bst-right rightleftNode) (bst-right rightNode))
        )
       )
     ]
    (fix bst)
    )
  )





(check-expect (fixLeft BSTll) (make-bst
                             B1
                             (make-bst A1  false false)
                             (make-bst Z1 false false)))
(check-expect (fixLeft BSTlr)(make-bst
                             G1
                             (make-bst B1  false false)
                             (make-bst Z1 false false)))


(define (fixLeft bst)
  (if (> (height (bst-left (bst-left bst))) (height (bst-right (bst-left bst))))
      
      (LLbst bst)
      
      (LRbst bst)
      )
  )




(check-expect (LLbst BSTll) (make-bst
                             B1
                             (make-bst A1  false false)
                             (make-bst Z1 false false)))

(define (LLbst bst)
  (local
    [
     (define leftLeftNode (bst-left (bst-left bst)))
     (define leftNode (bst-left bst))

     (define (fix bst)
       (make-bst
        (bst-widget leftNode)
        leftLeftNode
        (make-bst (bst-widget bst) (bst-right leftNode) (bst-right bst))
        )
       )
     ]
    (fix bst)
    )
  )




(check-expect (LRbst BSTlr) (make-bst
                             G1
                             (make-bst B1  false false)
                             (make-bst Z1 false false))) 


(define (LRbst bst)
  (local
    [
     (define leftRightNode (bst-right (bst-left bst)))
     (define leftNode (bst-left bst))

     (define (fix bst)
       (make-bst
        (bst-widget leftRightNode)
        (make-bst (bst-widget leftNode) (bst-left leftNode) (bst-left leftRightNode))
        (make-bst (bst-widget bst) (bst-right leftRightNode) (bst-right bst))
        )
       )
     ]
    (fix bst)
    )
  )










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



(define (insert widget db)
  (local
    [
     (define (insertBst widget bst)
       (cond [(false? bst) (make-bst widget false false)]
             [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
              (make-bst (bst-widget bst)
                        (insertBst widget (bst-left bst))
                        (bst-right bst)
                        )
              ]
             [(false? ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst))))
              (make-bst (bst-widget bst)
                        (bst-left bst)
                        (insertBst widget (bst-right bst))
                        )
              ]
             )
       )
     ]
    (make-db
     (db-field db)
     (db-lt? db)
     (db-eq? db)
     (insertBst widget (db-bst db))
     )
    )
  )


(define (time-insert )
  
  (local [
          (define 250KWIDGET (random-widgets 250000 999999))
          (define blankdb (make-db widget-name string<=? string=?
                                   (make-bst (first 250KWIDGET) false false)))
          (define (regInsert db0 low)
            (cond [(empty? low) false]
                  [else
                   (regInsert (insert (first low) db0) (rest low))]))

          (define (quickInsert db0 low)
            (cond [(empty? low) false]
                  [else
                   (quickInsert (insert! (first low) db0) (rest low))]))]

    (begin
      (time (quickInsert blankdb 250KWIDGET))
      (set! blankdb (make-db widget-name string<=? string=?
                             (make-bst (first 250KWIDGET) false false)))
      (time (regInsert blankdb 250KWIDGET))
      )))

(define 10KWIDGET (random-widgets 10000 999999))
(define blankdb (make-db widget-name string<=? string=? (make-bst (first 10KWIDGET) false false)))


(define (insertList fn low db)
  (cond [(empty? low) "done"]
                  [else
                   (insertList fn (rest low) (fn (first low) db))])
  )


(insertList insert! 10KWIDGET blankdb)

(define blankdbAVL (make-db widget-name string<=? string=? false))

(insertList insert-avl 10KWIDGET blankdbAVL)


(define (time-find)
  
  (local [
          
          (define (findbst db0 low)
            (cond [(empty? low) false]
                  [else
                   (begin (find ((db-field db0) (first low)) db0)
                   (findbst db0 (rest low)))]))

          (define (findavl db0 low)
            (cond [(empty? low) false]
                  [else
                   (begin (find ((db-field db0) (first low)) db0)
                   (findavl db0 (rest low)))]))]

    (begin
      
      (time(findbst blankdb 10KWIDGET))
      (time(= 0 0))
      (time(findavl blankdbAVL 10KWIDGET))
      
      
      )))


(define (find field db)
  (local [
          (define (searchBst field bst)
            (cond
              [(false? bst) false]
              [((db-eq? db) field ((db-field db) (bst-widget bst)))
               (bst-widget bst) 
               ]
              [((db-lt? db) field ((db-field db) (bst-widget bst)))
               (searchBst field (bst-left bst))
               ]
              [else
               (searchBst field (bst-right bst))
               ]
              )
            )
          ]
    (searchBst field (db-bst db))
    )

  )



(require 2htdp/image)

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


.
