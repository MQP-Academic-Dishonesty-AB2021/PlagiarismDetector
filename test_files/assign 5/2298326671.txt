

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |starter pt3-1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))





(define-struct widget (name quantity price))








(define-struct bst (widget left right))







(define-struct db (field lt? eq? bst))

(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 100 100))
(define C1 (make-widget "C1" 999 999))
(define D1 (make-widget "D1" 5 5))
(define W1 (make-widget "W1" 1 1))
(define X1 (make-widget "X1" 420 420))
(define Z1 (make-widget "Z1" 51 16))
(define E1 (make-widget "E1" 6 7))

(define BST7 (make-bst B1 false false))
(define BST4 (make-bst A1 false BST7))
(define BST5 (make-bst D1 false false))
(define BST2 (make-bst C1 BST4 BST5))
(define BST6 (make-bst X1 false false))
(define BST3 (make-bst Z1 BST6 false))
(define BST1 (make-bst W1 BST2 BST3))



(define (insert widget db)
  (local [(define lt-func (db-lt? db))
          (define eq-func (db-eq? db))
          (define field-func (db-field db))
          (define (insert-internal bst)
            (cond
              [(false? bst) (make-bst widget
                                      false
                                      false)]
              [(lt-func (field-func widget)
                        (field-func (bst-widget bst)))
               (make-bst (bst-widget bst)
                         (insert-internal (bst-left bst))
                         (bst-right bst))]
              [(eq-func (field-func widget)
                        (field-func (bst-widget bst)))
               bst]
              [else (make-bst (bst-widget bst)
                              (bst-left bst)
                              (insert-internal (bst-right bst)))]))]
    (db--set-bst db (insert-internal (db-bst db)))))



(define DB-name (make-db widget-name string<? string=? BST1))



(check-expect (db-bst (insert! E1 (db--set-bst DB-name BST3)))
              (make-bst Z1
                        (make-bst X1
                                  (make-bst E1 false false)
                                  false)
                        false)) 
(check-expect (db-bst (insert! E1 (db--set-bst DB-name BST7)))
              (make-bst B1
                        false
                        (make-bst E1 false false))) 
(check-expect (db-bst (insert! E1 (db--set-bst DB-name BST4)))
              (make-bst A1
                        false
                        (make-bst B1
                                  false
                                  (make-bst E1
                                            false
                                            false)))) 
(check-expect (db-bst (insert! E1 (db--set-bst DB-name BST2)))
              (make-bst C1
                        BST4
                        (make-bst D1
                                  false
                                  (make-bst E1
                                            false
                                            false)))) 
(check-expect (db-bst (insert! B1 (db--set-bst DB-name BST7)))
              BST7) 
(check-expect (db-bst (insert! B1 (db--set-bst DB-name false)))
              (make-bst B1 false false)) 



(define-struct insertion-point (bst direction))
(define (insert! widget db)
  (local [(define lt-func (db-lt? db))
          (define eq-func (db-eq? db))
          (define field-func (db-field db))
          (define (find-insertion-point bst widget)
            (local [(define (find-insertion-point bst prevbst direction)
                      (cond [(false? bst) (make-insertion-point prevbst direction)]
                            [else (letrec ([bw (bst-widget bst)]
                                           [wf (field-func widget)]
                                           [bwf (field-func bw)])
                                    (cond [(lt-func wf bwf)
                                           (find-insertion-point (bst-left bst)
                                                                 bst
                                                                 'left)]
                                          [(eq-func wf bwf)
                                           (make-insertion-point prevbst
                                                                 direction)]
                                          [else (find-insertion-point (bst-right bst)
                                                                      bst
                                                                      'right)]))]))]
              (find-insertion-point bst #f 'none)))]
    (if (false? (db-bst db))
        (db--set-bst db (make-bst widget false false))
        (let ([insertion-point (find-insertion-point (db-bst db) widget)])
          (begin (match (insertion-point-direction insertion-point)
                   ['none (void)]
                   ['left (set-bst-left! (insertion-point-bst insertion-point)
                                         (make-bst widget #f #f))]
                   ['right (set-bst-right! (insertion-point-bst insertion-point)
                                           (make-bst widget #f #f))])
                 db)))))



(define (find key db)
  (local [(define (find-internal bst)
            (cond
              [(false? bst) false]
              [((db-eq? db) key
                            ((db-field db) (bst-widget bst)))
               (bst-widget bst)]
              [((db-lt? db) key
                            ((db-field db) (bst-widget bst)))
               (find-internal (bst-left bst))]
              [else
               (find-internal (bst-right bst))]))]
    (find-internal (db-bst db))))



(define (db--set-bst db bst)
  (make-db (db-field db)
           (db-lt? db)
           (db-eq? db)
           bst))









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



(define (insert-avl widget db)
  (local [(define (balance-avl bst new-key keyfunc lt? eq?)
            (local [(define (avl-rotate-right bst)
                      (local [(define old-left (bst-left bst))
                              (define new-right (bst-right old-left))]
                        (begin (set-bst-right! old-left bst)
                               (set-bst-left! bst new-right)
                               old-left)))
                    (define (avl-rotate-left bst)
                      (local [(define old-right (bst-right bst))
                              (define new-left (bst-left old-right))]
                        (begin (set-bst-left! old-right bst)
                               (set-bst-right! bst new-left)
                               old-right)))
                    (define balance-factor (height-diff bst))]
              (begin
                (cond [(and (> balance-factor 1)
                            (lt? new-key
                                 (keyfunc (bst-widget (bst-left bst))))) 
                       (avl-rotate-right bst)]
                      [(and (< balance-factor -1)
                            (not (eq? new-key
                                      (keyfunc (bst-widget (bst-right bst)))))
                            (not (lt? new-key
                                      (keyfunc (bst-widget (bst-right bst)))))) 
                       (avl-rotate-left bst)]
                      [(and (> balance-factor 1)
                            (not (eq? new-key
                                      (keyfunc (bst-widget (bst-left bst)))))
                            (not (lt? new-key
                                      (keyfunc (bst-widget (bst-left bst)))))) 
                       (avl-rotate-right (make-bst (bst-widget bst)
                                                   (avl-rotate-left (bst-left bst))
                                                   (bst-right bst)))]
                      [(and (< balance-factor -1)
                            (lt? new-key
                                 (keyfunc (bst-widget (bst-right bst))))) 
                       (avl-rotate-left (make-bst (bst-widget bst)
                                                  (bst-left bst)
                                                  (avl-rotate-right (bst-right bst))))]
                      [else bst]))))
          (define (insert-avl--internal bst)
            (cond
              [(false? bst) (make-bst widget
                                      false
                                      false)]
              [((db-lt? db) ((db-field db) widget)
                            ((db-field db) (bst-widget bst)))
               (balance-avl (make-bst (bst-widget bst)
                                      (insert-avl--internal (bst-left bst))
                                      (bst-right bst))
                            ((db-field db) widget)
                            (db-field db)
                            (db-lt? db)
                            (db-eq? db))]
              [((db-eq? db) ((db-field db) widget)
                            ((db-field db) (bst-widget bst)))
               bst]
              [else (balance-avl (make-bst (bst-widget bst)
                                           (bst-left bst)
                                           (insert-avl--internal (bst-right bst)))
                                 ((db-field db) widget)
                                 (db-field db)
                                 (db-lt? db)
                                 (db-eq? db))]))]
    (db--set-bst db (insert-avl--internal (db-bst db)))))

(define BST8 (make-bst X1 false false))
(define DB-quantity (make-db widget-quantity < = BST8))

(check-expect (db-bst (insert-avl Z1
                                  (insert-avl W1
                                              (insert-avl E1
                                                          DB-quantity))))
              (make-bst
               (make-widget "E1" 6 7)
               (make-bst (make-widget "W1" 1 1) #false #false)
               (make-bst (make-widget "X1" 420 420)
                         (make-bst (make-widget "Z1" 51 16) #false #false) #false)))
(check-expect (db-bst (insert-avl A1
                                  (insert-avl Z1
                                              (insert-avl W1
                                                          (insert-avl E1
                                                                      DB-quantity)))))
              (make-bst
               (make-widget "E1" 6 7)
               (make-bst (make-widget "W1" 1 1)
                         #false
                         (make-bst (make-widget "A1" 2 3) #false #false))
               (make-bst (make-widget "X1" 420 420)
                         (make-bst (make-widget "Z1" 51 16) #false #false)
                         #false)) )
(check-expect (db-bst (insert-avl C1
                                  (insert-avl A1
                                              (insert-avl Z1
                                                          (insert-avl W1
                                                                      (insert-avl E1 DB-quantity))))))
              (make-bst
               (make-widget "E1" 6 7)
               (make-bst (make-widget "W1" 1 1)
                         #false
                         (make-bst (make-widget "A1" 2 3) #false #false))
               (make-bst (make-widget "X1" 420 420)
                         (make-bst (make-widget "Z1" 51 16) #false #false)
                         (make-bst (make-widget "C1" 999 999) #false #false))))
(check-expect (db-bst (insert-avl B1
                                  (insert-avl A1
                                              (insert-avl Z1
                                                          (insert-avl W1
                                                                      (insert-avl E1 DB-quantity))))))
              (make-bst
               (make-widget "E1" 6 7)
               (make-bst (make-widget "W1" 1 1)
                         #false
                         (make-bst (make-widget "A1" 2 3) #false #false))
               (make-bst (make-widget "B1" 100 100)
                         (make-bst (make-widget "Z1" 51 16) #false #false)
                         (make-bst (make-widget "X1" 420 420) #false #false))))
(check-expect (db-bst (insert-avl B1
                                  (insert-avl C1
                                              (insert-avl A1
                                                          (insert-avl Z1
                                                                      (insert-avl W1
                                                                                  (insert-avl E1
                                                                                              DB-quantity)))))))
              (make-bst
               (make-widget "E1" 6 7)
               (make-bst (make-widget "W1" 1 1)
                         #false
                         (make-bst (make-widget "A1" 2 3) #false #false))
               (make-bst
                (make-widget "X1" 420 420)
                (make-bst (make-widget "Z1" 51 16)
                          #false
                          (make-bst (make-widget "B1" 100 100) #false #false))
                (make-bst (make-widget "C1" 999 999)
                          #false
                          #false))))
(check-expect (db-bst (insert-avl X1
                                  (insert-avl B1
                                              (insert-avl C1
                                                          (insert-avl A1
                                                                      (insert-avl Z1
                                                                                  (insert-avl W1
                                                                                              (insert-avl E1
                                                                                                          DB-quantity))))))))
              (make-bst
               (make-widget "E1" 6 7)
               (make-bst (make-widget "W1" 1 1)
                         #false
                         (make-bst (make-widget "A1" 2 3) #false #false))
               (make-bst
                (make-widget "X1" 420 420)
                (make-bst (make-widget "Z1" 51 16)
                          #false
                          (make-bst (make-widget "B1" 100 100) #false #false))
                (make-bst (make-widget "C1" 999 999) #false #false))))
(check-expect (db-bst (insert-avl D1
                                  (insert-avl B1
                                              (insert-avl C1
                                                          (insert-avl A1
                                                                      (insert-avl Z1
                                                                                  (insert-avl W1
                                                                                              (insert-avl E1
                                                                                                          DB-quantity))))))))
              (make-bst
               (make-widget "E1" 6 7)
               (make-bst (make-widget "A1" 2 3)
                         (make-bst (make-widget "W1" 1 1) #false #false)
                         (make-bst (make-widget "D1" 5 5) #false #false))
               (make-bst
                (make-widget "X1" 420 420)
                (make-bst (make-widget "Z1" 51 16)
                          #false
                          (make-bst (make-widget "B1" 100 100) #false #false))
                (make-bst (make-widget "C1" 999 999) #false #false))))
(check-expect (db-bst (insert-avl B1
                                  (insert-avl D1
                                              (insert-avl C1
                                                          (insert-avl Z1
                                                                      (insert-avl A1
                                                                                  (insert-avl X1
                                                                                              (insert-avl E1
                                                                                                          DB-quantity))))))))
              (make-bst
               (make-widget "E1" 6 7)
               (make-bst (make-widget "A1" 2 3)
                         #false
                         (make-bst (make-widget "D1" 5 5) #false #false))
               (make-bst
                (make-widget "X1" 420 420)
                (make-bst (make-widget "Z1" 51 16)
                          #false
                          (make-bst (make-widget "B1" 100 100)
                                    #false
                                    #false))
                (make-bst (make-widget "C1" 999 999)
                          #false
                          #false))))








(define (random-widgets num max)
  (build-list num
              (λ(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))





(check-expect (db-bst (widgetlist-to-db null insert (empty-db widget-name string<? string=?)))
              false) 
(check-expect (db-bst (widgetlist-to-db (list (make-widget "hey" 1 2)) insert (empty-db widget-name string<? string=?)))
              (make-bst (make-widget "hey" 1 2) #f #f)) 
(define (widgetlist-to-db low insertion-func db)
  (local [(define (widgetlist-to-db low db)
            (cond [(empty? low) db]
                  [else (widgetlist-to-db (rest low)
                                          (insertion-func (first low) db))]))]
    (widgetlist-to-db low db)))





(define (empty-db field lt? eq?)
  (make-db field lt? eq? false))

(define RANDOM-WIDGETS (random-widgets 250000 (+ 8192 (random 1337))))




(define (better-time func)
  (let ([starttime (current-milliseconds)]
        [_ (func)]
        [endtime (current-milliseconds)])
    (- endtime starttime)))




(check-error (mean null))
(check-expect (mean (list 1 5 3)) 3)
(define (mean lon)
  (if (empty? lon)
      (error "the mean of an empty list is undefined")
      (/ (apply + lon) (length lon))))




(define (insertion-test insertion-func)
  (build-list 5
                    (lambda (_)
                      (better-time (lambda () (widgetlist-to-db RANDOM-WIDGETS
                                                                insertion-func
                                                                (empty-db widget-quantity < =)))))))


(define INSERT-TIMINGS (insertion-test insert))
(define MUT-INSERT-TIMINGS (insertion-test insert!))

(define AVG-TIME-FOR-INSERT (exact->inexact (mean INSERT-TIMINGS)))
(define AVG-TIME-FOR-MUT-INSERT (exact->inexact (mean MUT-INSERT-TIMINGS)))

(printf "Avg. time of insert with ~a widgets: ~ams\n" (length RANDOM-WIDGETS) AVG-TIME-FOR-INSERT)
(printf "Avg. time of insert! with ~a widgets: ~ams\n" (length RANDOM-WIDGETS) AVG-TIME-FOR-MUT-INSERT)

(printf "Timings for insert: ~a\n" INSERT-TIMINGS)
(printf "Timings for insert!: ~a\n" MUT-INSERT-TIMINGS) 




(check-expect (to-2places 4.567) 4.56) 
(check-expect (to-2places 4.25) 4.25) 
(check-expect (to-2places 4) 4) 
(define (to-2places n)
  (/ (floor (* n 100)) 100))

(printf "% difference between insert and insert!: ~a%\n"
        (exact->inexact (to-2places (* 100
                                       (/ (- AVG-TIME-FOR-MUT-INSERT
                                             AVG-TIME-FOR-INSERT)
                                          AVG-TIME-FOR-INSERT)))))

(define FIND-TEST-RANDOM-WIDGETS (random-widgets 10000 1337))
(define FIND-TEST-DB (widgetlist-to-db FIND-TEST-RANDOM-WIDGETS
                                       insert!
                                       (empty-db widget-quantity < =)))
(define FIND-TIMINGS (build-list 10
                                 (lambda (_)
                                   (better-time (lambda ()
                                                  (map (lambda (w)
                                                         (find (widget-quantity w) FIND-TEST-DB))
                                                       FIND-TEST-RANDOM-WIDGETS))))))
(define AVG-TIME-FOR-FIND
  (exact->inexact (mean FIND-TIMINGS)))

(printf "Regular BST: find timings: ~a\n" FIND-TIMINGS)
(printf "Regular BST: avg. find timing: ~a\n" AVG-TIME-FOR-FIND)
(define FIND-AVL-TEST-DB (widgetlist-to-db FIND-TEST-RANDOM-WIDGETS
                                           insert-avl
                                           (empty-db widget-quantity < =)))
(define FIND-AVL-TIMINGS (build-list 10
                                     (lambda (_)
                                       (better-time (lambda ()
                                                      (map (lambda (w)
                                                             (find (widget-quantity w) FIND-AVL-TEST-DB))
                                                           FIND-TEST-RANDOM-WIDGETS))))))
(define AVG-TIME-FOR-FIND-AVL
  (exact->inexact (mean FIND-AVL-TIMINGS)))

(printf "AVL BST: find timings: ~a\n" FIND-AVL-TIMINGS)
(printf "AVL BST: avg. find timing: ~a\n" AVG-TIME-FOR-FIND-AVL)