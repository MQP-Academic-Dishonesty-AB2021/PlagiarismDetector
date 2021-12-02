

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment5Part2SarahConnor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(define-struct widget (name quantity price))



(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define-struct bst (widget left right))



(define bstA (make-bst A1 false false))
(define bstZ (make-bst Z1 false false))
(define bstD (make-bst D1 bstA false))

(define bst1 (make-bst W1 bstA bstZ))
(define bst2 (make-bst D1 false bstZ))
(define bst3 (make-bst D1 bstA bstZ))
(define bstF false)

(define-struct db (field lt? eq? bst))






(define dbA (make-db widget-name string<? string=? bstA))
(define dbZ (make-db widget-quantity < = bstZ))
(define dbD (make-db widget-price < = bstD))

(define db1 (make-db widget-name string<? string=? bst1))
(define db2 (make-db widget-quantity < = bst2))
(define db3 (make-db widget-price < = bst3))


(define dbF (make-db widget-name string<? string=? bstF))








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

(check-expect (db-bst (insert A1 db2)) (db-bst (make-db (db-field db2) (db-lt? db2) (db-eq? db2)
                                                        (make-bst D1
                                                                  (make-bst A1 false false)
                                                                  (make-bst Z1 false false))))) 

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

