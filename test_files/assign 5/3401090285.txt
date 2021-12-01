

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |starter pt3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))




(define-struct bst (widget left right))




(define-struct widget (name quantity price))



(define-struct db (field lt? eq? bst))




(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 4 58))
(define C1 (make-widget "C1" 1 10))
(define D1 (make-widget "D1" 6 5))
(define F1 (make-widget "F1" 5 35))
(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))

(define simple-bst (make-bst W1 false false))
(define test-bst (make-bst D1 (make-bst A1 false false) (make-bst W1 false false)))
(define complex-bst (make-bst W1 (make-bst D1 (make-bst A1 false false) false) 
                                 (make-bst Z1 false false)))

(define DB-quantity1 (make-db widget-quantity < = simple-bst))
(define DB-quantity2 (make-db widget-quantity < = test-bst))
(define DB-name1 (make-db widget-name string<? string=? simple-bst))
(define DB-name2 (make-db widget-name string<? string=? complex-bst))




(define (insert widget db)
  (local [
      
    
    (define (new-db-bst left?)
      (db-bst (insert widget (make-db (db-field db)
            (db-lt? db)
            (db-eq? db)
            (if left?
              (bst-left (db-bst db))
              (bst-right (db-bst db))
            )
          )
        )
      )
    )

    
    (define (new-make-db left?)
      (make-db (db-field db)
        (db-lt? db)
        (db-eq? db)
        (make-bst (bst-widget (db-bst db))
          (if left? (new-db-bst true) (bst-left (db-bst db)))
          (if left? (bst-right (db-bst db)) (new-db-bst false))
        )
      )
    )
  ]
    (cond
      [(false? (db-bst db))
        (make-db (db-field db) (db-lt? db) (db-eq? db) (make-bst widget false false))
      ]
      [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget (db-bst db))))
        (new-make-db true) 
      ]
      [(not ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget (db-bst db)))))
        (new-make-db false) 
      ]
    )
  )
)



(define (insert! widget db)
  (local [
    
    (define (new-db-bst left?)
      (db-bst (insert! widget (make-db (db-field db)
            (db-lt? db)
            (db-eq? db)
            (if left?
              (bst-left (db-bst db))
              (bst-right (db-bst db))
            )
          )
        )
      )
    )

    
    (define (new-make-db left?)
      (make-db (db-field db)
        (db-lt? db)
        (db-eq? db)
        (make-bst (bst-widget (db-bst db))
          (if left? (new-db-bst true) (bst-left (db-bst db)))
          (if left? (bst-right (db-bst db)) (new-db-bst false))
        )
      )
    )
  ]
    (cond
      [(false? (db-bst db))
        (begin (set-db-bst! db (make-bst widget false false)) db)
      ]
      [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget (db-bst db))))
        (new-make-db true)
      ]
      [(not ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget (db-bst db)))))
        (new-make-db false)
      ]
    )
  )
)

(check-expect (db-bst (insert! Z1 DB-name1)) (make-bst W1 false (make-bst Z1 false false))) 
(check-expect (db-bst (insert! D1 DB-quantity1)) (make-bst W1 false (make-bst D1 false false))) 
(check-expect (db-bst (insert! Z1 DB-quantity2)) (make-bst D1 (make-bst A1 false false)  
                                                  (make-bst W1 false (make-bst Z1 false false))))



(define (insert-avl widget db)
  (local [
    (define (balance bst)
      (local [
          (define diff (height-diff bst))
          (define (balanced? bst)
            (if (< (abs diff) 2)
                true
                false
             )
           )
           (define (left-left? bst)
             (and (= 2 diff) (not (false? (bst-left (bst-left bst)))))
           )
           (define (left-right? bst)
             (and (= 2 diff) (not (false? (bst-right (bst-left bst)))))
           )
           (define (right-left? bst)
             (and (= 2 diff) (not (false? (bst-left (bst-right bst)))))
           )
           (define (right-right? bst)
             (and (= 2 diff) (not (false? (bst-right (bst-right bst)))))
           )
           (define (left-left bst)
             (make-bst (bst-widget (bst-left bst)) (bst-left (bst-left bst))
              (make-bst (bst-widget bst) false false))
           )
           (define (left-right bst)
             (left-left (make-bst (bst-widget bst) (bst-left bst) (bst-right (bst-left bst))))
           )
           (define (right-right bst)
             (make-bst (bst-widget (bst-right bst)) (make-bst (bst-widget bst) false false)
              (bst-right (bst-right bst)))
           )
           (define (right-left bst)
             (right-right (make-bst (bst-widget bst) (bst-right bst) (bst-left (bst-right bst))))
           )
          ]
    (cond
      [(balanced? bst) bst]
      [else
       (cond
         [(left-left? bst) (left-left bst)]
         [(left-right? bst) (left-right bst)]
         [(right-left? bst) (right-left bst)]
         [(right-right? bst) (right-right bst)]
         [else
          (make-bst (bst-widget bst) (balance (bst-left bst)) (balance (bst-right bst)))
         ]
       )
      ]
    )
  )
)
      
    
    (define (new-db-bst left?)
      (balance (db-bst (insert-avl widget (make-db (db-field db)
              (db-lt? db)
              (db-eq? db)
              (if left?
                (bst-left (db-bst db))
                (bst-right (db-bst db))
              )
            )
          )
        )
      )
    )

    
    (define (new-make-db left?)
      (make-db (db-field db)
        (db-lt? db)
        (db-eq? db)
        (make-bst (bst-widget (db-bst db))
          (if left? (new-db-bst true) (bst-left (db-bst db)))
          (if left? (bst-right (db-bst db)) (new-db-bst false))
        )
      )
    )
  ]
    (cond
      [(false? (db-bst db))
        (make-db (db-field db) (db-lt? db) (db-eq? db) (make-bst widget false false))
      ]
      [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget (db-bst db))))
        (new-make-db true) 
      ]
      [(not ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget (db-bst db)))))
        (new-make-db false) 
      ]
    )
  )
)


(check-expect (db-bst (insert-avl Z1 DB-quantity1)) (make-bst W1 false (make-bst Z1 false false)))
(check-expect (db-bst (insert-avl D1 DB-quantity1)) (make-bst W1 false (make-bst D1 false false)))
(check-expect (db-bst (insert-avl Z1 DB-quantity2))
              (make-bst D1 (make-bst A1 false false) 
                           (make-bst W1 false (make-bst Z1 false false))))


(check-expect (db-bst (insert-avl D1 DB-name1)) (make-bst W1 (make-bst D1 false false) false))
(check-expect (db-bst (insert-avl Z1 DB-name1)) (make-bst W1 false (make-bst Z1 false false)))
(check-expect (db-bst (insert-avl F1 DB-name2)) 
              (make-bst W1 (make-bst D1 (make-bst A1 false false) (make-bst F1 false false)) 
                           (make-bst Z1 false false)))




(define (build-tree low)
  (foldr insert (make-db widget-quantity < = false) low))



(define (find val db)
  (local [

    
    (define (find-leaf left?)
      (find val (make-db (db-field db)
          (db-lt? db)
          (db-eq? db)
          (if left? (bst-left (db-bst db)) (bst-right (db-bst db)))
        )
      )
    )
  ]
    (cond
      [(false? (db-bst db)) false]
      [((db-eq? db) val ((db-field db) (bst-widget (db-bst db))))
        (bst-widget (db-bst db))
      ]
      [((db-lt? db) val ((db-field db) (bst-widget (db-bst db))))
        (find-leaf true) 
      ]
      [(not ((db-lt? db) val ((db-field db) (bst-widget (db-bst db)))))
        (find-leaf false) 
      ]
      [else
        false
      ]
    )
  )
)





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



(define sample-db (make-db widget-name string<? string=? false))

(define (time-insert db)
  (local
    [
      (define random-list (random-widgets 250000 250000))
    ]
    (begin (time (foldl insert db random-list))
           (time (foldl insert! db random-list))
           "Done")
  )
)

(define (time-find db)
  (local 
    [
    (define random-list (random-widgets 10000 10000))
    ]
    (begin (time (for-each (lambda (n) (find n db)) random-list))
           (time (for-each (lambda (n) (insert-avl n db)) random-list))
           "Done")
  )
)