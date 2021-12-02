

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment5part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





(define-struct widget (name quantity price))




(define-struct bst (widget left right))






(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define C1 (make-widget "C1" 8 4))
(define D1 (make-widget "D1" 5 5))

(define BST0 (make-bst W1 false false))
(define BST1 (make-bst D1 false (make-bst Z1 false false)))
(define BST2 (make-bst W1 (make-bst A1 false (make-bst D1 false false)) (make-bst Z1 false false)))
(define BST3 false) 

(define-struct db (field lt? eq? bst))









(define DB-quantity (make-db widget-quantity < = false))
(define DB-quantity2 (make-db widget-quantity < = BST0))
(define DB-name2 (make-db widget-name string<? string=? BST2))
(define DB-name1 (make-db widget-name string<? string=? BST1))
(define DB-name (make-db widget-name string<? string=? BST0))

(define DB-price (make-db widget-price < = false))





(define (b-val field b) (field (bst-widget b)))






(check-expect (find 9 DB-quantity) false) 
(check-expect (find "Z1" DB-name2) Z1) 
(check-expect (find "A1" DB-name1) false) 
(check-expect (find "W1" DB-name) W1) 

(define (find val dtb)
  (local [
          (define (find-widget k b field smaller? same?)
            (cond
              [(false? b) false]
              [(same? k (b-val field b))
               (bst-widget b)]
              [(smaller? k (b-val field b))
               (find-widget k (bst-left b) field smaller? same?)]
              [else
               (find-widget k (bst-right b) field smaller? same?)]))]
    (find-widget val (db-bst dtb) (db-field dtb) (db-lt? dtb) (db-eq? dtb))))





(check-expect (db-bst (insert W1 DB-quantity)) 
              (make-bst W1 false false))

(check-expect (db-bst (insert C1 DB-name2)) 
              (make-bst W1 (make-bst A1 false
                        (make-bst D1 (make-bst C1 false false) false))
                        (make-bst Z1 false false)))

(check-expect (db-bst (insert Z1 DB-quantity2)) 
              (make-bst W1 false (make-bst Z1 false false)))


(define (insert widg dtb)
  (local [
          (define (insert-widget b field smaller?)
            (cond
              [(false? b)
               (make-bst widg false false)]
              [(smaller? (field widg) (b-val field b))
               (make-bst (bst-widget b)
                         (insert-widget (bst-left b) field smaller?) (bst-right b))]
              [else
               (make-bst (bst-widget b)
                         (bst-left b) (insert-widget (bst-right b) field smaller?))]))]

    
    (make-db (db-field dtb) (db-lt? dtb) (db-eq? dtb)
             (insert-widget (db-bst dtb) (db-field dtb) (db-lt? dtb)))))


