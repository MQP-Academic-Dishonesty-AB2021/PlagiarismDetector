

#reader(lib "htdp-advanced-reader.ss" "lang")((modname Assignment5Part3SarahConnor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))



(require 2htdp/image)


(define-struct bst (widget left right))




(define-struct widget (name quantity price))



(define-struct db (field lt? eq? bst))






(define (build-tree low)
  (foldr insert-name false low))

(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 7 1))
(define C1 (make-widget "C1" 8 4))
(define D1 (make-widget "D1" 5 5))
(define E1 (make-widget "E1" 4 3))
(define W1 (make-widget "W1" 1 1))
(define X1 (make-widget "X1" 70 30))
(define Z1 (make-widget "Z1" 51 16))

(define bstA (make-bst A1 false false))
(define bstB (make-bst B1 false false))
(define bstZ (make-bst Z1 false false))

(define bstD (make-bst D1 bstA false))
(define bst1 (make-bst W1 bstA bstZ))
(define bst2 (make-bst D1 false bstZ))
(define bst3 (make-bst D1 bstA bstZ))
(define bst4 (make-bst E1 bstB false))

(define bstF false)

(define dbA (make-db widget-name string<? string=? bstA))
(define dbZ (make-db widget-quantity < = bstZ))
(define dbD (make-db widget-price < = bstD))

(define db1 (make-db widget-name string<? string=? bst1))
(define db2 (make-db widget-quantity < = bst2))
(define db3 (make-db widget-price < = bst3))
(define db4 (make-db widget-price < = bst4))
(define dbF (make-db widget-name string<? string=? bstF))





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








(check-expect (db-bst (insert A1 dbF)) (db-bst(make-db (db-field dbF) (db-lt? dbF) (db-eq? dbF)
                                                       (make-bst A1 false false))))


(check-expect (db-bst (insert D1 db1)) (db-bst (make-db (db-field db1) (db-lt? db1) (db-eq? db1)
                                                        (make-bst W1
                                                                  (make-bst A1
                                                                            false
                                                                            (make-bst D1 false false))
                                                                  (make-bst Z1 false false)))))

(check-expect (db-bst (insert A1 dbZ)) (db-bst (make-db (db-field dbZ) (db-lt? dbZ) (db-eq? dbZ)
                                                        (make-bst Z1
                                                                  (make-bst A1 false false)
                                                                  false))))

(check-expect (db-bst (insert X1 db2)) (db-bst (make-db (db-field db2) (db-lt? db2) (db-eq? db2)
                                                        (make-bst D1
                                                                  false
                                                                  (make-bst Z1
                                                                            false
                                                                            (make-bst X1 false false))))))

(define (insert key d)
  (local [(define (smaller? key d)
            ((db-lt? d) ((db-field d) key) ((db-field d) (bst-widget (db-bst d)))))
          (define (get-left d)
            (make-db (db-field d) (db-lt? d) (db-eq? d) (bst-left (db-bst d))))
          (define (get-right d)
            (make-db (db-field d) (db-lt? d) (db-eq? d) (bst-right (db-bst d))))

          (define (insert-inner key d)
            (cond [(false? (db-bst d))
                   (make-bst key false false)]
                  [(smaller? key d)
                   (make-bst (bst-widget (db-bst d))
                             (db-bst (insert key (get-left d)))
                             (db-bst (get-right d)))]
                  [(not (smaller? key d))
                   (make-bst (bst-widget (db-bst d))
                             (db-bst (get-left d))
                             (db-bst (insert key (get-right d))))]))]
    
    (make-db (db-field d) (db-lt? d) (db-eq? d) (insert-inner key d))))






(check-expect (insert-name A1 bstZ) (make-bst Z1
                                              (make-bst A1 false false)
                                              false))


(check-expect (insert-name D1 bst1) (make-bst W1
                                              (make-bst A1
                                                        false
                                                        (make-bst D1 false false))
                                              (make-bst Z1 false false)))


(check-expect (insert-name Z1 bstD) (make-bst D1
                                              (make-bst A1 false false)
                                              (make-bst Z1 false false)))
(define (insert-name wid b)
  (local [(define (smaller? k b)
            (cond [(false? b) false]
                  [else
                   (string<? k (widget-name (bst-widget b)))]))]
          
    (cond
      [(false? b) (make-bst wid false false)]
      [(smaller? (widget-name wid) b)
       (make-bst (bst-widget b)
                 (insert-name wid (bst-left b))
                 (bst-right b))]
      [(not (smaller? (widget-name wid) b))
       (make-bst (bst-widget b)
                 (bst-left b)
                 (insert-name wid (bst-right b)))])))






(check-expect (find "W1" db1) W1)    
(check-expect (find "D1" db1) false) 
(check-expect (find "A1" dbA) A1)    


(check-expect (find 51 dbZ) Z1)   
(check-expect (find 5 db2) D1)    
(check-expect (find 3 db2) false) 


(check-expect (find 3 dbD) A1)    
(check-expect (find 16 db3) Z1)   
(check-expect (find 0 dbD) false) 


(define (find key d)
  (local [(define (smaller? key d)
            ((db-lt? d) key ((db-field d) (bst-widget (db-bst d)))))
          (define (same? key d)
            ((db-eq? d) key ((db-field d) (bst-widget (db-bst d)))))
          (define (get-left d)
            (make-db (db-field d) (db-lt? d) (db-eq? d) (bst-left (db-bst d))))
          (define (get-right d)
            (make-db (db-field d) (db-lt? d) (db-eq? d) (bst-right (db-bst d))))]
    
    (cond [(false? (db-bst d)) false]
          [(smaller? key d)
           (find key (get-left d))]
          [(same? key d)
           (bst-widget (db-bst d))]
          [(not (smaller? key d))
           (find key (get-right d))])))








(check-expect (db-bst (insert! A1 dbF)) (db-bst(make-db (db-field dbF) (db-lt? dbF) (db-eq? dbF)
                                                        (make-bst A1 false false))))


(check-expect (db-bst (insert! D1 db1)) (db-bst (make-db (db-field db1) (db-lt? db1) (db-eq? db1)
                                                         (make-bst W1
                                                                   (make-bst A1
                                                                             false
                                                                             (make-bst D1 false false))
                                                                   (make-bst Z1 false false)))))

(check-expect (db-bst (insert! A1 db2)) (db-bst (make-db (db-field db2) (db-lt? db2) (db-eq? db2)
                                                         (make-bst D1
                                                                   (make-bst A1 false false)
                                                                   (make-bst Z1 false false)))))


(check-expect (db-bst (insert! C1 db4)) (db-bst (make-db (db-field db4) (db-lt? db4) (db-eq? db4)
                                                         (make-bst E1
                                                                   (make-bst B1 false false)
                                                                   (make-bst C1 false false)))))

(define (insert! key d)
  (local [(define (smaller? key wid)
            ((db-lt? d) ((db-field d) key) ((db-field d) wid)))
          (define (insert-inner bst parent set-func!)
    
            (cond [(false? bst)
                   (if (empty? parent)
                       (set-db-bst! d (make-bst key false false))
                       (set-func! parent
                                  (make-bst key false false)))]
                  [(smaller? key (bst-widget bst))
                   (insert-inner (bst-left bst) bst set-bst-left!)]
          
                  [(not (smaller? key (bst-widget bst)))
                   (insert-inner (bst-right bst) bst set-bst-right!)]))]
    (begin (insert-inner (db-bst d) empty set-bst-left!) d)))





(define A1-avl (make-widget "A1" 2 3))
(define B1-avl (make-widget "B1" 7 1))
(define C1-avl (make-widget "C1" 3 0))
(define D1-avl (make-widget "D1" 5 5))
(define E1-avl (make-widget "E1" 4 3))
(define J1-avl (make-widget "J1" 6 6))
(define W1-avl (make-widget "W1" 1 1))
(define Z1-avl (make-widget "Z1" 51 16))

(define bstA-avl (make-bst A1-avl false false))
(define bstB-avl (make-bst B1-avl false false))
(define bstA-lB  (make-bst A1-avl false bstB-avl))
(define bstA-rB  (make-bst A1-avl bstB-avl false))
(define bstZ-avl (make-bst Z1-avl false false))
(define bstD-avl (make-bst D1-avl false bstZ-avl))
(define bstE-avl (make-bst E1-avl bstA-avl false))

(define avl1 (make-bst W1-avl bstA-lB bstZ-avl))
(define avl2 (make-bst D1-avl bstE-avl bstZ-avl))
(define avl3 (make-bst D1-avl bstA-rB bstZ-avl))
(define avl4 (make-bst E1-avl bstB-avl bstD-avl))

(define bstF-avl false)

(define db1-a (make-db widget-name string<? string=? avl1))
(define db2-a (make-db widget-quantity < = avl2))
(define db3-a (make-db widget-price < = avl3))
(define db4-a (make-db widget-price < = avl4))
(define dbF-a (make-db widget-name string<? string=? bstF-avl))







(check-expect (db-bst (insert-avl A1 dbF-a)) (db-bst(make-db (db-field dbF) (db-lt? dbF) (db-eq? dbF)
                                                        (make-bst A1 false false))))




(check-expect (db-bst (insert-avl C1-avl db1-a)) (db-bst (make-db (db-field db1-a) (db-lt? db1-a) (db-eq? db1-a)
                                                                  (make-bst W1-avl
                                                                            (make-bst B1-avl
                                                                                      (make-bst A1-avl false false)
                                                                                      (make-bst C1-avl false false))
                                                                            (make-bst Z1-avl false false)))))



(check-expect (db-bst (insert-avl C1-avl db3-a)) (db-bst (make-db (db-field db3-a) (db-lt? db3-a) (db-eq? db3-a)
                                                                  (make-bst D1-avl
                                                                            (make-bst B1-avl
                                                                                      (make-bst C1-avl false false)
                                                                                      (make-bst A1-avl false false))
                                                                            (make-bst Z1-avl false false)))))



(check-expect (db-bst (insert-avl C1-avl db2-a)) (make-bst D1-avl
                                                           (make-bst C1-avl
                                                                     (make-bst A1-avl false false)
                                                                     (make-bst E1-avl false false))
                                                           (make-bst Z1-avl false false)))



(check-expect (db-bst (insert-avl J1-avl db4-a)) (make-bst E1-avl
                                                           (make-bst B1-avl false false)
                                                           (make-bst J1-avl
                                                                     (make-bst D1-avl false false)
                                                                     (make-bst Z1-avl false false))))
                                                        

(define (insert-avl key d)
  (local [(define (smaller? key d)
            ((db-lt? d) ((db-field d) key) ((db-field d) (bst-widget (db-bst d)))))
          (define (get-left d)
            (make-db (db-field d) (db-lt? d) (db-eq? d) (bst-left (db-bst d))))
          (define (get-right d)
            (make-db (db-field d) (db-lt? d) (db-eq? d) (bst-right (db-bst d))))]
     
    (cond [(false? (db-bst d))
           (make-db (db-field d) (db-lt? d) (db-eq? d)
                    (make-bst key false false))]
          [(smaller? key d)
           (make-db (db-field d) (db-lt? d) (db-eq? d)
                    (make-bst (bst-widget (db-bst d))
                              (db-bst (balance (insert-avl key (get-left d))))
                              (db-bst (get-right d))))]
          [(not (smaller? key d))
           (make-db (db-field d) (db-lt? d) (db-eq? d)
                    (make-bst (bst-widget (db-bst d))
                              (db-bst (get-left d))
                              (db-bst (balance (insert-avl key (get-right d))))))])))



(check-expect (db-bst (balance (make-db widget-name
                                        string<?
                                        string=?
                                        (make-bst A1-avl
                                                  false
                                                  (make-bst B1-avl
                                                            false
                                                            (make-bst C1-avl
                                                                      false
                                                                      false))))))
              (make-bst
               (make-widget "B1" 7 1)
               (make-bst (make-widget "A1" 2 3) false false)
               (make-bst (make-widget "C1" 3 0) false false)))


(check-expect (db-bst (balance (make-db widget-name
                                        string<?
                                        string=?
                                        (make-bst C1-avl
                                                  (make-bst B1-avl
                                                            (make-bst A1-avl
                                                                      false
                                                                      false)
                                                            false)
                                                  false)
                                        )))
              (make-bst
               (make-widget "B1" 7 1)
               (make-bst (make-widget "A1" 2 3) false false)
               (make-bst (make-widget "C1" 3 0) false false)))


(check-expect (db-bst (balance (make-db widget-name
                                        string<?
                                        string=?
                                        (make-bst A1-avl
                                                  false
                                                  (make-bst C1-avl
                                                    
                                                            (make-bst B1-avl
                                                                      false
                                                                      false)
                                                            false)))))
              (make-bst
               (make-widget "B1" 7 1)
               (make-bst (make-widget "A1" 2 3) false false)
               (make-bst (make-widget "C1" 3 0) false false)))


(check-expect (db-bst (balance (make-db widget-name
                                        string<?
                                        string=?
                                        (make-bst C1-avl
                                                  (make-bst A1-avl
                                                            false
                                                            (make-bst B1-avl
                                                                      false
                                                                      false)
                                                            )
                                                  false)
                                        )))
              (make-bst
               (make-widget "B1" 7 1)
               (make-bst (make-widget "A1" 2 3) false false)
               (make-bst (make-widget "C1" 3 0) false false)))





(define (balance db)
  (local [
          (define (make-db-bst bst)
            (make-db (db-field db) (db-lt? db) (db-eq? db) bst))

          
          (define (left-left? bst)
            (if (false? (bst-left bst))
                false
                (not (false? (bst-left (bst-left bst))))))
          (define (right-right? bst)
            (if (false? (bst-right bst))
                false
                (not (false?(bst-right (bst-right bst))))))

          (define (left-right? bst)
            (if (false? (bst-left bst))
                false
                (not (false?(bst-right (bst-left bst))))))

          (define (right-left? bst)
            (if (false? (bst-right bst))
                false
                (not (false? (bst-left (bst-right bst))))))

          
          
          
          (define (left-rotate-l bst)
            (make-bst (bst-widget bst) 
                      (make-bst (bst-widget (bst-right (bst-left bst)))
                                (make-bst (bst-widget (bst-left bst)) false false)
                                false)
                      false))

          
          (define (right-rotate-l bst)
            (make-bst (bst-widget (bst-left bst))
                      (bst-left (bst-left bst))
                      (make-bst (bst-widget bst) false false)))


          
          (define (left-rotate-r bst)
            (make-bst (bst-widget (bst-right bst))
                      (make-bst (bst-widget bst) false false)
                      (make-bst (bst-widget (bst-right (bst-right bst))) false false)))

          
          (define (right-rotate-r bst)
            (make-bst (bst-widget bst) 
                      false
                      (make-bst (bst-widget (bst-left (bst-right bst)))
                                false
                                (make-bst (bst-widget (bst-right bst)) false false))))]
    (if (> (abs (height-diff (db-bst db))) 1)
        (cond [(left-left? (db-bst db)) (make-db-bst (right-rotate-l (db-bst db)))]
              [(right-right? (db-bst db)) (make-db-bst (left-rotate-r (db-bst db)))]
              [(left-right? (db-bst db)) (make-db-bst (right-rotate-l (left-rotate-l (db-bst db))))]
              [(right-left? (db-bst db)) (make-db-bst (left-rotate-r (right-rotate-r (db-bst db))))])

        db)))







(define (random-widgets num max)
  (build-list num
              (λ(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))

(define time-tree (build-tree (random-widgets 250000 9999999)))
(define time-db (make-db widget-name string<? string=? time-tree))





(define (time-insert db)
  (local [(define time-list1 (random-widgets 250000 9999999))]
    (begin
      (time (foldl insert db time-list1))
      (time (foldl insert! db time-list1))
      "done!")))








(set-db-bst! time-db false)
(define time-avl (foldl insert-avl time-db (random-widgets 10000 9999999)))
(define time-db-avl (make-db widget-name string<? string=? time-avl))





(define (time-find db)
  (local [(define (time-f widgets db)
            (cond [(empty? widgets) empty]
                  [else (begin (find ((db-field db) (first widgets)) db)
                               (time-f (rest widgets) db) empty)]))]
    (local [(define time-list1 (random-widgets 10000 9999999))
            (define bst (foldl insert time-db time-list1))
            (define time-avl (foldl insert-avl time-db time-list1))]

      (begin
        (time (time-f time-list1 bst))
        (time (time-f time-list1 time-avl))
        "done!")
      )))








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