

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






(define-struct widget(name quantity time price parts))

(define (fn-for-widget-tree w)
  (local
    [(define (fn-for-widget w)
       (... (widget-name w)
            (widget-quantity w)
            (widget-time w)
            (widget-price w)
            (fn-for-low (widget-parts w))))
     (define (fn-for-low low)
       (cond
         [(empty? low) empty]
         [(cons? low)
          (... (fn-for-widget (first low))
               (fn-for-low (rest low)))]))]
    (fn-for-widget w)))








(check-expect (find-widget-name-longer-than-trampoline Wire 3)
              (list Wire)) 
(check-expect (find-widget-name-longer-than-trampoline Wire 4) null) 
(check-expect (find-widget-name-longer-than-trampoline Cord 3) 
              (list Cord Wire))
(check-expect (find-widget-name-longer-than-trampoline Telephone 5) 
              (list Telephone Receiver Buttons Numbers))
(define (find-widget-name-longer-than-trampoline w namelen)
  (local
    [(define (fn-for-widget w namelen)
       (cond
         [(> (string-length (widget-name w)) namelen)
          (cons w (fn-for-low (widget-parts w) namelen))]
         [else (fn-for-low (widget-parts w) namelen)]))
     (define (fn-for-low low namelen)
       (cond
         [(empty? low) empty]
         [(cons? low)
          (append (fn-for-widget (first low) namelen)
                  (fn-for-low (rest low) namelen))]))]
    (fn-for-widget w namelen)))






(check-expect (find-widget-quantity-over-trampoline Glass 4) (list Glass)) 
(check-expect (find-widget-quantity-over-trampoline Glass 9) empty) 
(check-expect (find-widget-quantity-over-trampoline Beads 4) (list Beads Glass)) 
(define (find-widget-quantity-over-trampoline w stock)
  (local
    [(define (fn-for-widget w stock)
       (cond [(> (widget-quantity w) stock)
              (cons w (fn-for-low (widget-parts w) stock))]
             [else (fn-for-low (widget-parts w) stock)]))
     (define (fn-for-low low stock)
       (cond [(empty? low) empty]
             [(cons? low)
              (append (fn-for-widget (first low) stock)
                      (fn-for-low (rest low) stock))]))]
    (fn-for-widget w stock)))






(check-expect (find-widgets-cheaper-than-trampoline Wire 3) null) 
(check-expect (find-widgets-cheaper-than-trampoline Wire 13)
              (list Wire)) 
(check-expect (find-widgets-cheaper-than-trampoline Telephone 15) 
              (list Receiver Buttons Numbers Cord Wire))
(define (find-widgets-cheaper-than-trampoline w maxprice)
  (local
    [(define (fn-for-widget w maxprice)
       (cond
         [(< (widget-price w) maxprice)
          (cons w
                (fn-for-low
                 (widget-parts w)
                 maxprice))]
         [else (fn-for-low
                (widget-parts w)
                maxprice)]))
     (define (fn-for-low low maxprice)
       (cond
         [(empty? low) empty]
         [(cons? low)
          (append (fn-for-widget (first low) maxprice)
                  (fn-for-low (rest low) maxprice))]))]
    (fn-for-widget w maxprice)))







(check-expect (find-widget-hard-make-trampoline Telephone 10 5)
              (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widget-hard-make-trampoline Telephone 0 10000) 
              null)
(check-expect (find-widget-hard-make-trampoline Wire 10 5) 
              (list Wire))
(define (find-widget-hard-make-trampoline w minquantity maxcost)
  (local
    [(define (widget-is-hard? w)
       (or (< (widget-quantity w) minquantity)
           (> (widget-price w) maxcost)))
     (define (fn-for-widget w)
       (cond
         [(widget-is-hard? w)
          (cons w (fn-for-low (widget-parts w)))]
         [else (fn-for-low (widget-parts w))]))
     (define (fn-for-low low)
       (cond
         [(empty? low) empty]
         [(cons? low)
          (append (fn-for-widget (first low))
                  (fn-for-low (rest low)))]))]
    (fn-for-widget w)))
          

(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 5 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 5 empty))
(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
(define Receiver (make-widget "Receiver" 10 5 7 empty))
(define Telephone (make-widget "Telephone" 5 20 15
                               (list Receiver Buttons Cord)))


(define Glass (make-widget "Glass" 6 9 4 empty))
(define Beads (make-widget "Beads" 25 12 7 (list Glass)))
(define Bracelet (make-widget "Bracelet" 5 3 5 (list Beads)))
(define Chain (make-widget "Chain" 7 2 1 empty))
(define Pendant (make-widget "Pendant" 4 3 1 empty))
(define Necklace (make-widget "Necklace" 10 7 3
                              (list Chain Pendant)))
(define Rings (make-widget "Rings" 15 8 11 empty))
(define Jewelry (make-widget "Jewelry set" 4 17 30
                             (list Rings Necklace Bracelet)))







(check-expect
 (filter-widget-tree
  (lambda (w) (<= (widget-price w) 20))
  Jewelry)
 (list Rings Necklace Chain Pendant Bracelet Beads Glass))
(define (filter-widget-tree predicate w)
  (local
    [(define (fn-for-widget w)
       (if (predicate w)
           (cons w (fn-for-low (widget-parts w)))
           (fn-for-low (widget-parts w))))
     (define (fn-for-low low)
       (cond
         [(empty? low) empty]
         [(cons? low)
          (append (fn-for-widget (first low))
                  (fn-for-low (rest low)))]))]
    (fn-for-widget w)))






(check-expect (find-widget-name-longer-than Wire 3)
              (list Wire)) 
(check-expect (find-widget-name-longer-than Wire 4) null) 
(check-expect (find-widget-name-longer-than Cord 3) 
              (list Cord Wire))
(check-expect (find-widget-name-longer-than Telephone 5) 
              (list Telephone Receiver Buttons Numbers))
(define (find-widget-name-longer-than w namelen)
  (filter-widget-tree
   (λ (w) (> (string-length (widget-name w)) namelen))
   w))






(check-expect (find-widget-quantity-over Glass 4) (list Glass)) 
(check-expect (find-widget-quantity-over Glass 9) empty) 
(check-expect (find-widget-quantity-over Beads 4) (list Beads Glass)) 
(define (find-widget-quantity-over w stock)
  (filter-widget-tree
   (λ (w) (> (widget-quantity w) stock))
   w))






(check-expect (find-widgets-cheaper-than Wire 3) null) 
(check-expect (find-widgets-cheaper-than Wire 13)
              (list Wire)) 
(check-expect (find-widgets-cheaper-than Telephone 15) 
              (list Receiver Buttons Numbers Cord Wire))
(define (find-widgets-cheaper-than w maxprice)
  (filter-widget-tree
   (λ (w) (< (widget-price w) maxprice))
   w))







(check-expect (find-widget-hard-make Telephone 10 5)
              (list Telephone Receiver Buttons Numbers Cord Wire))
(define (find-widget-hard-make w minquantity maxcost)
  (local
    [(define (widget-is-hard? w)
       (or (< (widget-quantity w) minquantity)
           (> (widget-price w) maxcost)))]
    (filter-widget-tree
     widget-is-hard?
     w)))










(define (generic-qsort comparator property lox)
  (cond
    [(empty? lox) empty]
    [(empty? (rest lox)) (cons (first lox) empty)]
    [else
     (local
       [(define pivot (first lox))
        (define left
          (filter
           (lambda (x)
             (comparator (property x) (property pivot)))
           (rest lox)))
        (define middle
          (filter
           (lambda (x)
             (eq? x pivot))
           lox))
        (define right
          (filter
           (lambda (x)
             (comparator (property pivot) (property x)))
           (rest lox)))]
       (append
        (generic-qsort comparator property left)
        middle
        (generic-qsort comparator property right)))]))

(check-expect (generic-qsort < identity null) null)         
(check-expect (generic-qsort < identity (list 1)) (list 1)) 






(check-expect
 (length
  (generic-qsort < identity
                 (build-list 500 identity)))
 500)

(check-expect
 (length
  (generic-qsort < identity
                 (build-list 10000 (lambda (x) (random 10000)))))
 10000)


(check-expect
 (generic-qsort < identity (list 1 3 7 5 11 9 13 15 29 24 42 37 99 101 35 41))
 (list 1 3 5 7 9 11 13 15 24 29 35 37 41 42 99 101))
(check-expect
 (generic-qsort > identity (reverse (list 1 3 7 5 11 9 13 15 29 24 42 37 99 101 35 41)))
 (reverse (list 1 3 5 7 9 11 13 15 24 29 35 37 41 42 99 101)))
(check-expect
 (generic-qsort < identity (build-list 100 identity))
 (build-list 100 identity))








(check-expect (flatten-widget-tree Wire) (list Wire))              
(check-expect (flatten-widget-tree Cord) (list Cord Wire))         
(check-expect (flatten-widget-tree Telephone)
              (list Telephone Receiver Buttons Numbers Cord Wire)) 
(define (flatten-widget-tree w)
  (local
    [(define (fn-for-widget w)
       (cons w (fn-for-low (widget-parts w))))
     (define (fn-for-low low)
       (cond
         [(empty? low) empty]
         [(cons? low)
          (append (fn-for-widget (first low))
                  (fn-for-low (rest low)))]))]
    (fn-for-widget w)))








(define (qsort comparator property)
  (lambda (w)
    (local
      [(define flattened-tree (flatten-widget-tree w))]
      (generic-qsort comparator property flattened-tree))))





(check-expect
 (map widget-name (sort-strings Telephone))
 (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))
(define sort-strings (qsort string<? widget-name))





(check-expect
 (map widget-name (sort-overstocked Necklace))
 (list "Necklace" "Chain" "Pendant"))
(define sort-overstocked (qsort > widget-quantity))