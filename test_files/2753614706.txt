

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AustinPart2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define-struct widget (name quantity price))





(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define V1 (make-widget "V1" 75 99))
(define W1 (make-widget "W1" 1 1))
(define X1 (make-widget "X1" 55 0))
(define Y1 (make-widget "Y1" 60 50))
(define Z1 (make-widget "Z1" 51 16))



(define-struct bst (widget left right))


(define BST1S (make-bst D1
                        (make-bst A1 false false)
                        (make-bst Z1
                                  (make-bst W1 false false)
                                  false)))

(define BST1Q (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1 false false)
                                  false)))

(define BST1P (make-bst A1
                        (make-bst W1 false false)
                        (make-bst V1
                                  (make-bst Y1 false false)
                                  false)))

(define BST-LEFT (make-bst Z1
                           (make-bst W1
                                     (make-bst D1
                                               (make-bst A1 false false)
                                               false)
                                     false)
                           false))

(define BST-RIGHT (make-bst A1
                            false
                            (make-bst D1
                                      false
                                      (make-bst W1
                                                false
                                                (make-bst Z1 false false)))))



(define-struct db (field lt? eq? bst))


(define DBS (make-db widget-name string<? string=? BST1S)) 
(define DBQ (make-db widget-quantity < = BST1Q)) 
(define DBP (make-db widget-price < = BST1P)) 







(check-expect (smaller? (widget-name A1) BST1S DBS) true) 
(check-expect (smaller? (widget-name Z1) BST1S DBS) false) 
(check-expect (smaller? (widget-name D1) BST1S DBS) false) 
(check-expect (smaller? (widget-quantity W1) BST1Q DBQ) true) 
(check-expect (smaller? (widget-quantity Z1) BST1Q DBQ) false) 
(check-expect (smaller? (widget-price W1) BST1P DBP) true) 
(check-expect (smaller? (widget-price V1) BST1P DBP) false) 

(define (smaller? x bst db)
  ((db-lt? db) x ((db-field db) (bst-widget bst))))




(check-expect (same? (widget-name D1) BST1S DBS) true) 
(check-expect (same? (widget-name A1) BST1S DBS) false) 
(check-expect (same? (widget-quantity A1) BST1Q DBQ) true) 
(check-expect (same? (widget-quantity Z1) BST1Q DBQ) false) 

(define (same? x bst db)
  ((db-eq? db) x ((db-field db) (bst-widget bst))))





(check-expect (find "DNE" DBS) false) 
(check-expect (find (widget-name D1) DBS) D1) 
(check-expect (find (widget-name Z1) DBS) Z1) 
(check-expect (find (widget-quantity A1) DBQ) A1) 
(check-expect (find (widget-price Y1) DBP) Y1) 

(define (find k db)
  (local [(define (fn-for-find k bst)
            (cond [(false? bst) false]
                  [else (cond [(same? k bst db) (bst-widget bst)]
                              [(smaller? k bst db) (fn-for-find k (bst-left bst))]
                              [else (fn-for-find k (bst-right bst))])]))]
    (fn-for-find k (db-bst db))))




(check-expect (db-bst (insert A1 (make-db (db-field DBS)
                                          (db-lt? DBS)
                                          (db-eq? DBS)
                                          (make-bst D1 false false))))
              (make-bst D1 (make-bst A1 false false) false)) 
(check-expect (db-bst (insert X1 DBS))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst Z1
                                  (make-bst W1 false (make-bst X1 false false))
                                  false))) 
(check-expect (db-bst (insert X1 DBQ))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1 false false)
                                  (make-bst X1 false false)))) 
(check-expect (db-bst (insert X1 DBP))
              (make-bst A1
                        (make-bst W1 (make-bst X1 false false) false)
                        (make-bst V1
                                  (make-bst Y1 false false)
                                  false))) 

(define (insert w db)
  (local [(define (fn-for-insert bst)
            (cond [(false? bst) (make-bst w false false)] 
                  [(smaller? ((db-field db) w) bst db) 
                   (make-bst (bst-widget bst) (fn-for-insert (bst-left bst)) (bst-right bst))]
                  [else (make-bst (bst-widget bst) (bst-left bst) (fn-for-insert (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (fn-for-insert (db-bst db)))))