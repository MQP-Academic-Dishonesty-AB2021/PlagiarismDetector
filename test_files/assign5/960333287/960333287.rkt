

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |starter pt3-1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))




(define-struct bst (widget left right))




(define-struct widget (name quantity price))



(define-struct db (field lt? eq? bst))




(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 2 3))
(define C1 (make-widget "C1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define E1 (make-widget "E1" 2 3))

(define Z (make-widget "Z" 1 1))
(define Y (make-widget "Y" 2 3))
(define X (make-widget "X" 7 3))
(define T1 (make-widget "A" 45 6))
(define T2 (make-widget "B" 37 50))
(define T3 (make-widget "C" 65 2))
(define T4 (make-widget "D" 1 10))

(define DB-quantity (make-db widget-quantity < = false))
(define DB-price (make-db widget-price > = false))
(define DB-name (make-db widget-name string<? string=? false))



(define bst-ll (make-bst Z
                         (make-bst Y
                                   (make-bst X
                                             (make-bst T1 false false)
                                             (make-bst T2 false false))
                                   (make-bst T3 false false))
                         (make-bst T4 false false)))


(define bst-lr (make-bst Z
                         (make-bst Y
                                   (make-bst T1 false false)
                                   (make-bst X
                                             (make-bst T2 false false)
                                             (make-bst T3 false false)))
                         (make-bst T4 false false)))


(define bst-rr (make-bst Z
                         (make-bst T1 false false)
                         (make-bst Y
                                   (make-bst T2 false false)
                                   (make-bst X
                                             (make-bst T3 false false)
                                             (make-bst T4 false false)))))

(define bst-rl (make-bst Z
                         (make-bst T1 false false)
                         (make-bst Y
                                   (make-bst X
                                             (make-bst T2 false false)
                                             (make-bst T3 false false))
                                   (make-bst T4 false false))))



(define balanced-tree-ll (make-bst Y
                        (make-bst X
                                  (make-bst T1 false false)
                                  (make-bst T2 false false))
                        (make-bst Z
                                  (make-bst T3 false false)
                                  (make-bst T4 false false))))


(define balanced-tree-rr (make-bst Y
                        (make-bst Z
                                  (make-bst T1 false false)
                                  (make-bst T2 false false))
                        (make-bst X
                                  (make-bst T3 false false)
                                  (make-bst T4 false false))))


(define balanced-tree-lr (make-bst X
                        (make-bst Y
                                  (make-bst T1 false false)
                                  (make-bst T2 false false))
                        (make-bst Z
                                  (make-bst T3 false false)
                                  (make-bst T4 false false))))


(define balanced-tree-rl (make-bst X
                        (make-bst Z
                                  (make-bst T1 false false)
                                  (make-bst T2 false false))
                        (make-bst Y
                                  (make-bst T3 false false)
                                  (make-bst T4 false false))))







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







(define (left-rotate x)
  (local [(define y (bst-right x))
          (define T1 (bst-left x))
          (define T2 (bst-left y))
          (define T3 (bst-right y))]
    (make-bst (bst-widget y)
              (make-bst (bst-widget x)
                        T1 
                        T2)
              T3)))


(check-expect (left-rotate
               (make-bst A1
                         #f
                         (make-bst B1 #f #f)))
              (make-bst B1
                        (make-bst A1 #f #f)
                        #f))


(check-expect (left-rotate
               (make-bst B1
                         (make-bst A1 #f #f)
                         (make-bst D1
                                   (make-bst C1 #f #f)
                                   (make-bst E1 #f #f))))
              (make-bst D1
                        (make-bst B1
                                  (make-bst A1 #f #f)
                                  (make-bst C1 #f #f))
                        (make-bst E1 #f #f)))



(define (right-rotate b)
  (local [(define T4 (bst-right b))
          (define y (bst-left b))
          (define x (bst-left y))
          (define T3 (bst-right y))]
    (make-bst (bst-widget y)
              x
              (make-bst (bst-widget b)
                        T3
                        T4))))


(check-expect (right-rotate
               (make-bst B1
                         (make-bst A1 #f #f)
                         #f))
              (make-bst A1
                        #f
                        (make-bst B1 #f #f)))


(check-expect (right-rotate
               (make-bst D1
                         (make-bst B1
                                   (make-bst A1 #f #f)
                                   (make-bst C1 #f #f))
                         (make-bst E1 #f #f)))

              (make-bst B1
                        (make-bst A1 #f #f)
                        (make-bst D1
                                  (make-bst C1 #f #f)
                                  (make-bst E1 #f #f))))




(define (balance z)
  (local [(define l (bst-left z))
          (define r (bst-right z))]
    (cond
      [(> (height-diff z) 1)
       (if (> (height-diff l) 0)
           
           (right-rotate z)
           
           (right-rotate (make-bst (bst-widget z)
                                   (left-rotate l)
                                   r)))]
      [(< (height-diff z) -1) 
       (if (> (height-diff r) 0)
           
           (left-rotate (make-bst (bst-widget z)
                                  l
                                  (right-rotate r)))
           
           (left-rotate z))]
      [else z])))

(check-expect (balance balanced-tree-ll)
              balanced-tree-ll)


(check-expect (balance bst-ll)
              balanced-tree-ll)


(check-expect (balance bst-lr)
              balanced-tree-lr)


(check-expect (balance bst-rr)
              balanced-tree-rr)


(check-expect (balance bst-rl)
              balanced-tree-rl)





(define (insert w db)
  
  (local [(define (insert--bst w b dbfield dblt? dbeq?)
            (cond [(eq? b #f) (make-bst w #f #f)]
                  [(dblt? (dbfield w) (dbfield (bst-widget b)))
                   (make-bst
                    (bst-widget b)
                    (insert--bst w (bst-left b) dbfield dblt? dbeq?)
                    (bst-right b))]
                  [else
                   (make-bst
                    (bst-widget b)
                    (bst-left b)
                    (insert--bst w (bst-right b) dbfield dblt? dbeq?))]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert--bst w
                          (db-bst db)
                          (db-field db)
                          (db-lt? db)
                          (db-eq? db)))))




(define (insert-avl w db)
  
  (local [(define (insert--bst w b dbfield dblt? dbeq?)
            (balance
             (cond [(eq? b #f) (make-bst w #f #f)]
                   [(dblt? (dbfield w) (dbfield (bst-widget b)))
                    (make-bst
                     (bst-widget b)
                     (insert--bst w (bst-left b) dbfield dblt? dbeq?)
                     (bst-right b))]
                   [else
                    (make-bst
                     (bst-widget b)
                     (bst-left b)
                     (insert--bst w (bst-right b) dbfield dblt? dbeq?))])))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert--bst w
                          (db-bst db)
                          (db-field db)
                          (db-lt? db)
                          (db-eq? db)))))


(check-expect (db-bst (insert-avl A1 DB-name))
              (make-bst A1
                        #f
                        #f))

(check-expect (db-bst (insert-avl W1 (insert W1 DB-name)))
              (make-bst W1
                        #f
                        (make-bst W1 #f #f)))

(check-expect (db-bst (insert-avl A1 (insert W1 DB-name)))
              (make-bst W1
                        (make-bst A1 #f #f)
                        #f))

(check-expect (db-bst (insert-avl Z1 (insert W1 DB-name)))
              (make-bst W1
                        #f
                        (make-bst Z1 #f #f)))

(check-expect (db-bst (insert-avl D1 (insert A1 (insert W1 DB-name))))
              (make-bst D1
                        (make-bst A1 #f #f)
                        (make-bst W1 #f #f)))



(define (insert! w db)
  
  (local [(define (insert--bst! w b dbfield dblt? dbeq? parent set-fn!)
            (cond [(eq? b #f)
                   (set-fn! parent (make-bst w #f #f))]
                  [(dblt? (dbfield w) (dbfield (bst-widget b)))
                   (insert--bst! w (bst-left b) dbfield dblt? dbeq? b set-bst-left!)]
                  [else
                   (insert--bst! w (bst-right b) dbfield dblt? dbeq? b set-bst-right!)]))]
    (begin
      (if (false? (db-bst db))
          (set-db-bst! db (make-bst w #f #f))
          (insert--bst! w
                        (db-bst db)
                        (db-field db)
                        (db-lt? db)
                        (db-eq? db)
                        #f
                        #f))
      db)))


(check-expect (db-bst
               (insert! A1 (make-db widget-name string<? string=? false)))
              (make-bst A1
                        #f
                        #f))

(check-expect (db-bst
               (insert! W1
                        (insert! W1
                                 (make-db widget-name string<? string=? false))))
              (make-bst W1
                        #f
                        (make-bst W1 #f #f)))

(check-expect (db-bst
               (insert! A1
                        (insert! W1
                                 (make-db widget-name string<? string=? false))))
              (make-bst W1
                        (make-bst A1 #f #f)
                        #f))

(check-expect (db-bst
               (insert! Z1
                        (insert! W1
                                 (make-db widget-name string<? string=? false))))
              (make-bst W1
                        #f
                        (make-bst Z1 #f #f)))

(check-expect (db-bst
               (insert! D1
                        (insert! A1
                                 (insert! W1
                                          (make-db
                                           widget-name string<? string=? false)))))
              (make-bst W1
                        (make-bst A1
                                  #f
                                  (make-bst D1 #f #f))
                        #f))




(define (find val db)
  (if (not (false? (db-bst db)))
      (local
        
        [(define (find bst)
           (local
             [(define (smaller? val)
                ((db-lt? db) val
                             ((db-field db) (bst-widget bst))))
              (define (same? val)
                ((db-eq? db) val
                             ((db-field db) (bst-widget bst))))]
             (cond [(same? val) (bst-widget bst)]
                   [(smaller? val) (find (bst-left bst))]
                   [else (find (bst-right bst))])))]
        (find (db-bst db)))
      false))








(define (time-insert num trials)
  (cond [(<= trials 0) (void)]
        [else
         (begin 
           (local [(define widgets (random-widgets num 10000000))
                   (define db0 (make-db widget-name string<? string=? false))
                   (define db1 (make-db widget-name string<? string=? false))]
             (begin
               (display (string-append "Time using insert for trial #"
                                       (number->string trials)
                                       ": "))
               (time (foldl insert db0 widgets))
               (display (string-append "Time using insert! for trial #"
                                       (number->string trials)
                                       ": "))
               (time (foldl insert! db1 widgets))
               #f))
           (time-insert num (- trials 1)))]))














(define (time-find num repetitions trials)
  (cond [(<= trials 0) (void)]
        [else
         (begin 
           (local [(define widgets (random-widgets num 10000000))
                   (define db0-empty (make-db widget-name string<? string=? false))
                   (define db0 (foldl insert db0-empty widgets))
                   (define db1-empty (make-db widget-name string<? string=? false))
                   (define db1 (foldl insert-avl db1-empty widgets))]
             (begin
               (display (string-append "Time finding widgets in regular BST for trial #"
                                       (number->string trials)
                                       ": "))
               (time (for-each (λ (_)
                                 (for-each (λ (x) (find (widget-name x) db0)) widgets))
                               (build-list repetitions add1)))
               (display (string-append "Time finding widgets in AVL tree for trial #"
                                       (number->string trials)
                                       ": "))
               (time (for-each (λ (_)
                                 (for-each (λ (x) (find (widget-name x) db1)) widgets))
                               (build-list repetitions add1)))
               #f))
           (time-find num repetitions (- trials 1)))]))
































































































































































































































