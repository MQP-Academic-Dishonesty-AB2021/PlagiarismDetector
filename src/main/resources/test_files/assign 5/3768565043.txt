

#reader(lib "htdp-advanced-reader.ss" "lang")((modname Part3-Main) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)


(define-struct bst (widget left right))




(define-struct widget (name quantity price))


(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define X1 (make-widget "X1" 3 12))
(define Y1 (make-widget "Y1" 8 4))

(define-struct db (field lt? eq? bst))

(define DB-quantity (make-db widget-quantity < = false))
(define DB-name (make-db widget-name string<? string=? false))
(define DB-price (make-db widget-price < = false))








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


















(define (insert! widget db)
  (cond
    [(false? (db-bst db)) (begin (set-db-bst! db (make-bst widget false false))
                                 db)]
    [else
     (local [(define-struct which-side (set-operation side-operation))
             (define less-than? (db-lt? db))
             (define field (db-field db))
             (define db-widget (bst-widget (db-bst db)))
             (define left (make-which-side set-bst-left! bst-left))
             (define right (make-which-side set-bst-right! bst-right))
             (define left-or-right
               (if (less-than? (field widget) (field db-widget))
                   left
                   right))
             
             (define (insert-bst! bst parent-bst l-or-r)
               (cond [(false? bst)
                      (begin ((which-side-set-operation l-or-r)
                              parent-bst
                              (make-bst widget false false))
                             bst)]
                     [else
                      (local [(define l-r
                                (if (less-than? (field widget) (field (bst-widget bst)))
                                    left
                                    right))]
                        (insert-bst! ((which-side-side-operation left-or-right) bst) bst l-r))]))]
       (begin
         (insert-bst! (db-bst db) (db-bst db) left-or-right)
         db))]))


(check-expect
 (db-bst (insert! A1 (make-db widget-name string<? string=? (make-bst Z1 false false))))
 (make-bst Z1 (make-bst A1 false false) false))


(check-expect
 (db-bst (insert! Z1 (make-db widget-name string<? string=? (make-bst A1 false false))))
 (make-bst A1 false (make-bst Z1 false false)))


(check-expect
 (db-bst (insert! Z1 (make-db widget-name string<? string=? (make-bst D1
                                                                      (make-bst A1 false false)
                                                                      (make-bst W1 false false)))))
 
 (make-bst D1
           (make-bst A1 false false)
           (make-bst W1 false
                     (make-bst Z1 false false))))


(check-expect
 (db-bst (insert! A1 (make-db widget-name string<? string=? false)))
 (make-bst A1 false false))









(define loopable-tree (make-bst 50
                                (make-bst 30
                                          (make-bst 10 false false)
                                          (make-bst 40 false false))
                                (make-bst 70 false false)))

(define loopable-tree2 (make-bst 50
                                 (make-bst 30
                                           (make-bst 10
                                                     (make-bst 5
                                                               (make-bst 2 false false)
                                                               false)
                                                     false)
                                           false)
                                 (make-bst 70 false false)))

(define loopable-tree3 (make-bst 20
                                 (make-bst 15 false false)
                                 (make-bst 40 false
                                           (make-bst 55 false
                                                     (make-bst 60 false false)))))




(define (rotate-right bst)
  (make-bst (bst-widget (bst-left bst))
            (bst-left (bst-left bst))
            (make-bst (bst-widget bst)
                      (bst-right (bst-left bst))
                      (bst-right bst))))



(check-expect (rotate-right loopable-tree)
              (make-bst 30
                        (make-bst 10 false false)
                        (make-bst 50
                                  (make-bst 40 false false)
                                  (make-bst 70 false false))))

(check-expect (rotate-right (make-bst 70
                                      (make-bst 50
                                                (make-bst 30
                                                          (make-bst 10
                                                                    (make-bst 5
                                                                              (make-bst 2 false false)
                                                                              false)
                                                                    false)
                                                          false)
                                                false)
                                      false))
              loopable-tree2)


(check-expect (rotate-right (make-bst 40
                                      (make-bst 20
                                                (make-bst 15 false false)
                                                false)
                                      (make-bst 55
                                                false
                                                (make-bst 60 false false))))
              loopable-tree3)





(define (rotate-left bst)
  (make-bst (bst-widget (bst-right bst))
            (make-bst (bst-widget bst)
                      (bst-left bst)
                      (bst-left (bst-right bst)))
            (bst-right (bst-right bst))))




(check-expect (rotate-left (make-bst 30
                                     (make-bst 10 false false)
                                     (make-bst 50
                                               (make-bst 40 false false)
                                               (make-bst 70 false false))))
              loopable-tree)


(check-expect (rotate-left loopable-tree2)
              (make-bst 70
                        (make-bst 50
                                  (make-bst 30
                                            (make-bst 10
                                                      (make-bst 5
                                                                (make-bst 2 false false)
                                                                false)
                                                      false)
                                            false)
                                  false)
                        false))


(check-expect (rotate-left loopable-tree3)
              (make-bst 40
                        (make-bst 20
                                  (make-bst 15 false false)
                                  false)
                        (make-bst 55
                                  false
                                  (make-bst 60 false false))))












(define (balance b)
  (cond [(false? b) false]
        [else
         (local [(define balanced-b (make-bst (bst-widget b)
                                              (balance (bst-left b))
                                              (balance (bst-right b))))]
           (cond [(< (height b) 2) b]
                 [(< (abs (height-diff b)) 2) balanced-b]
                 [(negative? (height-diff b))
                  (rotate-left balanced-b)]
                 [else
                  (rotate-right balanced-b)]))]))


(define unbalanced-bst (make-bst 10
                                 (make-bst 8 (make-bst 7 (make-bst 6 false false) false) (make-bst 9 false false))
                                 (make-bst 12 false false)))

(define unbalanced-bst2 (make-bst 14
                                  false
                                  (make-bst 15
                                            false
                                            (make-bst 16
                                                      false
                                                      (make-bst 17 false false)))))
(define unbalanced-bst3 (make-bst 10
                                  (make-bst 9
                                            (make-bst 8
                                                      (make-bst 7 false false)
                                                      false)
                                            false)
                                  false))


(check-expect (balance unbalanced-bst)
              (make-bst 8
                        (make-bst 7
                                  (make-bst 6 false false)
                                  false)
                        (make-bst 10
                                  (make-bst 9 false false)
                                  (make-bst 12 false false))))


(check-expect (balance unbalanced-bst2)
              (make-bst 16
                        (make-bst 14 false
                                  (make-bst 15 false false))
                        (make-bst 17 false false)))


(check-expect (balance unbalanced-bst3)
              (make-bst 8
                        (make-bst 7 false false)
                        (make-bst 10
                                  (make-bst 9 false false)
                                  false)))






(define (insert-avl widget db)
  (local [(define (insert-bst bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
                   (balance (make-bst (bst-widget bst) (insert-bst (bst-left bst)) (bst-right bst)))]
                  [else
                   (balance (make-bst (bst-widget bst) (bst-left bst) (insert-bst (bst-right bst))))]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (balance (insert-bst (db-bst db))))))


(define unbalanced-widgets-bst (make-bst D1 false (make-bst W1 false (make-bst Z1 false false))))
(define unbalanced-widgets-bst2 (make-bst W1 false
                                          (make-bst A1
                                                    false
                                                    (make-bst D1
                                                              false
                                                              (make-bst Y1
                                                                        false
                                                                        (make-bst Z1 false false))))))
(define unbalanced-widgets-bst3 (make-bst A1
                                          (make-bst W1 false false)
                                          (make-bst D1 false
                                                    (make-bst Z1 false false))))

(define name-db-test (make-db widget-name string<? string=? unbalanced-widgets-bst))
(define quantity-db-test2 (make-db widget-quantity < = unbalanced-widgets-bst2))
(define price-db-test3 (make-db widget-price < = unbalanced-widgets-bst3))


(check-expect (db-bst (insert-avl A1 name-db-test))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst W1 false (make-bst Z1 false false))))


(check-expect (db-bst (insert-avl X1 quantity-db-test2))
              (make-bst D1
                        (make-bst A1
                                  (make-bst W1 false false)
                                  (make-bst X1 false false))
                        (make-bst Y1
                                  false
                                  (make-bst Z1 false false))))


(check-expect (db-bst (insert-avl X1 price-db-test3))
              (make-bst D1
                        (make-bst A1
                                  (make-bst W1 false false)
                                  false)
                        (make-bst Z1
                                  (make-bst X1 false false)
                                  false)))






















(define (build-tree low)
  (foldr insert-name false low))



(define (smaller? k b)
  (cond
    [(false? b) false]
    [else
     (string<?
      k
      (widget-name (bst-widget b)))]))




(define (same? k b)
  (cond
    [(false? b) false]
    [else
     (string=?
      k
      (widget-name (bst-widget b)))]))
   



(define (find-name k b)
  (cond
    [(false? b) false]
    [(same? k b)
     (bst-widget b)]
    [else
     (if (smaller? k b)
         (find-name k (bst-left b))
         (find-name k (bst-right b)))]))





(define (insert-name w b)
  (cond
    [(false? b) (make-bst w false false)]
    [(smaller? (widget-name w) b)
     (make-bst (bst-widget b) (insert-name w (bst-left b)) (bst-right b))]
    [else
     (make-bst (bst-widget b) (bst-left b) (insert-name w (bst-right b)))]))







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
  (local [(define (smaller? bst)
            ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst))))
          (define (insert-bst bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [(smaller? bst)
                   (make-bst (bst-widget bst) (insert-bst (bst-left bst)) (bst-right bst))]
                  [else
                   (make-bst (bst-widget bst) (bst-left bst) (insert-bst (bst-right bst)))]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert-bst (db-bst db)))))


(define (find key db)
  (local [(define (same? bst)
            ((db-eq? db) key ((db-field db) (bst-widget bst))))
          (define (smaller? bst)
            ((db-lt? db) key ((db-field db) (bst-widget bst))))
          (define (find bst)
            (cond [(false? bst) false]
                  [(same? bst)
                   (bst-widget bst)]
                  [(smaller? bst)
                   (find (bst-left bst))]
                  [else
                   (find (bst-right bst))]))]
    (find (db-bst db))))















(define large-low (random-widgets 250000 99999999))
(define DB-name-large-tree (make-db widget-name string<? string=? false))






(define (time-insert db)
  (begin
    (time (foldr insert db large-low))
    "done"))






(define (time-insert! db)
  (begin
    (time (foldr insert! db large-low))
    "done"))

(time-insert DB-name-large-tree)
(time-insert! DB-name-large-tree)





(define (test-time-b-bst num db)
  (local [(define many-rand (random-widgets num num))
          (define tree (build-tree many-rand))
          (define ndb (make-db (db-field db) (db-lt? db) (db-eq? db) tree))
          (define (start throw)
            (find-widgets many-rand))
          (define (find-widgets low)
            (cond [(empty? low) "done"]
                  [else (begin (find ((db-field db) (first low)) ndb)
                               (find-widgets (rest low)))]))]
    (time (start 0))))


(test-time-b-bst 10000 DB-name)








(define (test-time-b-avl num db)
  (local [(define many-rand (random-widgets num num))
          (define tree (balance (build-tree many-rand)))
          (define ndb (make-db (db-field db) (db-lt? db) (db-eq? db) tree))
          (define (start throw)
            (find-widgets many-rand))
          (define (find-widgets low)
            (cond [(empty? low) true]
                  [else (begin (find ((db-field db)(first low)) ndb)
                               (find-widgets (rest low)))]))]
    (time (start 0))))

(test-time-b-avl 10000 DB-name)