

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget(name quantity time price parts))












(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 10 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 9 empty))
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


(define (fn-for-widget widget)
  ( ... (widget-name widget)
        (widget-quantity widget)
        (widget-time widget)
        (widget-price widget)
        (local [(define (fn-for-low low)
                  (cond [(empty? low) ...]
                        [else
                         ... (fn-for-widget (first low))
                         (fn-for-low (rest low))]))]
          (fn-for-low (widget-parts widget)))))









(define (find-widgets selector comparator threshold root)
  (cond [(and
          (comparator (selector root) threshold)
          (empty? (widget-parts root)))
         (append (list root))]  
        [else
         (if (comparator (selector root) threshold)
             (cons
              root
              (foldr append empty (map
                                   (λ (W)(find-widgets selector comparator threshold W))
                                   (widget-parts root))))
             (foldr append empty (map
                                  (λ (W)(find-widgets selector comparator threshold W))
                                  (widget-parts root))))]))






(check-expect (find-widget-name-longer-than Wire 20) empty)  


(check-expect (find-widget-name-longer-than Telephone 100) empty)


(check-expect (find-widget-name-longer-than Telephone 6) (list Telephone Receiver Buttons Numbers))


(check-expect
 (find-widget-name-longer-than Telephone 0)
 (list Telephone Receiver Buttons Numbers Cord Wire))


(check-expect (find-widget-name-longer-than Cord 3)(list Cord Wire))


(define (find-widget-name-longer-than root min-len)
  (find-widgets
   (λ (w)(string-length (widget-name w)))
   >
   min-len
   root))






(check-expect (find-widget-quantity-over Glass 20) empty)


(check-expect (find-widget-quantity-over Bracelet 5)(list Beads Glass))


(check-expect (find-widget-quantity-over Jewelry 9)(list Rings Necklace Beads)) 


(define (find-widget-quantity-over root min-stock)
  (find-widgets
   widget-quantity
   >
   min-stock
   root))






(check-expect (find-widgets-cheaper-than Wire 4) empty)

(check-expect (find-widgets-cheaper-than Cord 9)(list Wire))

(check-expect (find-widgets-cheaper-than Telephone 10)(list Receiver Buttons Numbers Wire))

(define (find-widgets-cheaper-than root max-price)
  (find-widgets
   widget-price
   <
   max-price
   root))





(check-expect (find-widget-hard-make Rings 0 120) empty) 

(check-expect (find-widget-hard-make Necklace 5 2) (list Necklace Pendant))

(check-expect (find-widget-hard-make Jewelry 10 10) (list Rings Jewelry Chain Pendant Bracelet Glass))

(define (find-widget-hard-make root max-stock min-price)
  (local [
          (define price-list (find-widgets
                              widget-price
                              >
                              min-price
                              root))
          (define stock-list(find-widgets
                             widget-quantity
                             <
                             max-stock
                             root))
          (define (remove-duplicates low)
            (cond[(empty? low) empty]
                 [(member (first low) (rest low)) (remove-duplicates (rest low))]
                 [else (cons (first low) (remove-duplicates (rest low)))]))
          ]
    (remove-duplicates (append price-list stock-list))))
  






(check-expect (qsort-tree string<? widget-name Telephone)
              (list Buttons Cord Numbers Receiver Telephone Wire))

(check-expect (qsort-tree < widget-quantity Jewelry)
              (list Jewelry Pendant Bracelet Glass Chain Necklace Rings Beads))


(define (qsort-tree comparator selector widget)
  (qsort-on-list comparator selector (all-parts widget)))






(check-expect (all-parts Receiver) (list Receiver))
(check-expect (all-parts Telephone)(list Telephone Receiver Buttons Numbers Cord Wire))

(define (all-parts widget)
  (local [(define (all-parts-low low)
            (cond [(empty? low) empty]
                  [else
                   (append (all-parts (first low))
                           (all-parts-low (rest low)))]))]

    
    (cons widget
          (all-parts-low (widget-parts widget)))))




(check-expect (qsort-on-list string<? widget-name (list Numbers Cord Wire))
              (list Cord Numbers Wire))

(check-expect (qsort-on-list < widget-time empty) empty) 



(define (qsort-on-list comparator selector low) 
  (local [(define (less-than piv low)
            (filter (λ (w) (comparator (selector w) (selector piv))) low))
          (define (greater-than piv low)
            (filter (λ (w) (not(comparator (selector w) (selector piv)))) low))]

    (cond [(empty? low) empty]
          [else
           (local [(define pivot (first low))]
             (append
              (qsort-on-list comparator selector (less-than pivot (rest low)))
              (list pivot)
              (qsort-on-list comparator selector (greater-than pivot (rest low)))))])))






(define (qsort comparator selector)
  (λ (w) (qsort-tree comparator selector w)))



(define sort-strings (qsort string<? widget-name))
