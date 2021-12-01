

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |5 pt3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define-struct bst (widget left right))




(define-struct widget (name quantity price))



(define-struct db (field lt? eq? bst))








(define (getheight b)
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
      (- (getheight (bst-left b))
         (getheight (bst-right b)))))







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
                        (render-helper (bst-right b) (+ d 1) img "R: ")
                        (to-text side (bst-widget b) d)
                        (render-helper (bst-left b) (+ d 1) img "L: "))))]
    (render-helper b 0 empty-image "T: ")))



(define (DB-quantity bst) (make-db widget-quantity < = bst))
(define (DB-name bst) (make-db widget-name string<? string=? bst))
(define (DB-name2 bst) (make-db (compose string->number	widget-name)  < = bst))

(define (find db value)
  (local {(define (find bst) (cond
                               [(false? bst) false]
                               [(comp? (db-eq? db) db value bst) (bst-widget bst)]
                               [(comp? (db-lt? db) db value bst) (find (bst-left bst))]
                               [else (find (bst-right bst))]))}
    (find (db-bst db))))


(define (comp? fn? db value bst)
  (fn? value ((db-field db) (bst-widget bst))))

(define (insert db wid)
  (local {(define (insert bst) (cond
                                 [(false? bst) (make-bst wid false false)]
                                 [(comp? (db-lt? db) db ((db-field db) wid) bst) (make-bst (bst-widget bst)
                                                                                           (insert (bst-left bst))
                                                                                           (bst-right bst))]          
                                 [else (make-bst (bst-widget bst)
                                                 (bst-left bst)
                                                 (insert (bst-right bst)))]))}
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert (db-bst db)))))




(define (insert! db wid)
  (local {(define (insert bst last lr) (cond
                                         [(false? bst) ((if lr set-bst-left! set-bst-right!)
                                                        last (make-bst wid false false))]
                                         [(comp? (db-lt? db) db ((db-field db) wid) bst) (insert (bst-left bst) bst #t)]          
                                         [else (insert (bst-right bst) bst #f)]))}
    (begin (insert (db-bst db) #f #f) db)))

(define db-in (DB-name (make-bst (make-widget "D1" 5 5) #f #f)))
(begin 
  (insert! db-in (make-widget "W1" 1 1))
  (insert! db-in (make-widget "Z1" 51 16))
  (insert! db-in (make-widget "A1" 2 3))
  (void))
(check-expect (db-bst db-in) (make-bst
                              (make-widget "D1" 5 5)
                              (make-bst (make-widget "A1" 2 3) #f #f)
                              (make-bst (make-widget "W1" 1 1) #f (make-bst (make-widget "Z1" 51 16) #f #f))))





(define (insert-avl db wid)
  (local {(define (insert bst) (cond
                                 [(false? bst) (make-bst wid false false)]
                                 [(comp? (db-lt? db) db ((db-field db) wid) bst) (balance (make-bst (bst-widget bst)
                                                                                                    (insert (bst-left bst))
                                                                                                    (bst-right bst)))]          
                                 [else (balance (make-bst (bst-widget bst)
                                                          (bst-left bst)
                                                          (insert (bst-right bst))))]))}
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert (db-bst db)))))



(define (balance bst)
  (letrec ([diff (height-diff bst)]
           [child ((if (positive? diff) bst-left bst-right) bst)]
           [chdiff (height-diff child)]
           [child2 ((if (positive? diff) bst-left bst-right) child)])
    (if (zero? (* diff chdiff)) bst
        (if (positive? diff)
            (if (positive? chdiff)
                (make-bst (bst-widget child)
                          (bst-left child)
                          (make-bst (bst-widget bst) (bst-right child) (bst-right bst)))
                (make-bst (bst-widget (bst-right child))
                          (make-bst (bst-widget child) (bst-left child) (bst-left (bst-right child)))
                          (make-bst (bst-widget bst) (bst-right (bst-right child)) (bst-right bst))))
            (if (negative? chdiff)
                (make-bst (bst-widget child)
                          (make-bst (bst-widget bst) (bst-left bst) (bst-left child))
                          (bst-right child))
                (make-bst (bst-widget (bst-left child))
                          (make-bst (bst-widget bst) (bst-left bst) (bst-left (bst-left child)))
                          (make-bst (bst-widget child) (bst-right (bst-left child)) (bst-right child))))))))
(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define C1 (make-widget "C1" 12 5))
(define B1 (make-widget "B1" 15 4))

(define B-Z1 (make-bst Z1 false false))
(define B-A1 (make-bst A1 false false))
(define B-W1 (make-bst W1 false B-Z1))
(define B-D1 (make-bst D1 B-A1 B-W1))

(define cb1 (make-bst D1 (make-bst C1 (make-bst B1 false false) false) false))
(define cb2 (make-bst C1 false (make-bst Z1 (make-bst D1 false false) false)))
(define cb3 (make-bst C1 false (make-bst D1 false (make-bst Z1 false false))))
(define cb4 (make-bst Z1 (make-bst D1 false (make-bst C1 false false)) false))
(check-expect (balance B-D1) (make-bst (make-widget "W1" 1 1)
                              (make-bst (make-widget "D1" 5 5)
                               (make-bst (make-widget "A1" 2 3) #f #f) #f)
                              (make-bst (make-widget "Z1" 51 16) #f #f)))
(check-expect (balance cb1) (make-bst (make-widget "C1" 12 5)
                                      (make-bst  (make-widget "B1" 15 4)  #f  #f)
                                      (make-bst  (make-widget "D1" 5 5)  #f  #f)))
(check-expect (balance cb3) (make-bst (make-widget "D1" 5 5)
                                      (make-bst (make-widget "C1" 12 5) #f #f)
                                      (make-bst (make-widget "Z1" 51 16) #f #f)))

(check-expect (balance cb2)(make-bst (make-widget "D1" 5 5)
                                     (make-bst (make-widget "C1" 12 5) #f #f)
                                     (make-bst (make-widget "Z1" 51 16) #f #f)))

(check-expect (balance cb4) (make-bst (make-widget "C1" 12 5)
                                      (make-bst (make-widget "D1" 5 5) #f #f)
                                      (make-bst (make-widget "Z1" 51 16) #f #f)))



"initialize lists"
(define bst-tests (build-list 10 (λ (n) (random-widgets-string 250000 16 256))))
(define avl-tests (build-list 20 (λ (n) (random-widgets-string 10000 16 256))))
"insert"
(begin (time (map (λ (n) (time (foldl (λ (a b) (insert b a))
       (DB-name (make-bst (first n) #f #f))
       (rest n)))) bst-tests)) 0)
"insert!"
(begin (time (map (λ (n) (time (foldl (λ (a b) (insert! b a))
       (DB-name (make-bst (first n) #f #f))
       (rest n)))) bst-tests)) 0)
"make bsts"
(define bst-trees (time (map (λ (n) (time (foldl (λ (a b) (insert b a))
                                                 (DB-name (make-bst (first n) #f #f))
                                                 (rest n)))) avl-tests)))
"make avls"
(define avl-trees (time (map (λ (n) (time (foldl (λ (a b) (insert-avl b a))
                                                 (DB-name (make-bst (first n) #f #f))
                                                 (rest n)))) avl-tests)))
"find bst"
(begin (time (map (λ (index) (time (map (λ (el) (find (list-ref bst-trees index) (widget-name el))) (list-ref avl-tests index)))) (build-list 10 identity))) 0)
"find avl"
(begin (time (map (λ (index) (time (map (λ (el) (find (list-ref avl-trees index) (widget-name el))) (list-ref avl-tests index)))) (build-list 10 identity))) 0)









