

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






(define-struct widget(name quantity time price parts))
                       

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

(define (fn-for-widget w)
  (... (widget-name w)
       (widget-quantity w)
       (widget-time w)
       (widget-price w)
       (fn-for-widgetlist (widget-parts w))))
(define (fn-for-widgetlist low)
  (cond
    [(empty? low) empty]
    [(cons? low)
     (... (fn-for-widget (first low))
          (fn-for-widgetlist (rest low)))]))






(check-expect (find-widget-name-longer-than--low (widget-parts Necklace) 4) (list Chain Pendant)) 
(check-expect (find-widget-name-longer-than--low (widget-parts Necklace) 7) null) 
(define (find-widget-name-longer-than--low low namelen)
  (cond
    [(empty? low) empty]
    [(cons? low)
     (append
      (find-widget-name-longer-than (first low) namelen)
      (find-widget-name-longer-than--low (rest low) namelen))]))






(check-expect (find-widget-name-longer-than Wire 3)
              (list Wire)) 
(check-expect (find-widget-name-longer-than Wire 4) null) 
(check-expect (find-widget-name-longer-than Cord 3) 
              (list Cord Wire))
(check-expect (find-widget-name-longer-than Telephone 5) 
              (list Telephone Receiver Buttons Numbers))
(define (find-widget-name-longer-than w namelen)
  (cond
    [(> (string-length (widget-name w)) namelen)
     (cons w (find-widget-name-longer-than--low (widget-parts w) namelen))]
    [else (find-widget-name-longer-than--low (widget-parts w) namelen)]))






(check-expect (find-widget-quantity-over--low (widget-parts Glass) 4) empty) 
(check-expect (find-widget-quantity-over--low (widget-parts Beads) 4) (list Glass)) 
(check-expect (find-widget-quantity-over--low (widget-parts Necklace) 1) (list Chain Pendant)) 
(define (find-widget-quantity-over--low low stock)
  (cond [(empty? low) empty]
        [(cons? low)
         (append (find-widget-quantity-over (first low) stock)
                 (find-widget-quantity-over--low (rest low) stock))]))






(check-expect (find-widget-quantity-over Glass 4) (list Glass)) 
(check-expect (find-widget-quantity-over Glass 9) empty) 
(check-expect (find-widget-quantity-over Beads 4) (list Beads Glass)) 
(define (find-widget-quantity-over w stock)
  (cond [(> (widget-quantity w) stock)
         (cons w
               (find-widget-quantity-over--low
                (widget-parts w) stock))]
        [else (find-widget-quantity-over--low
               (widget-parts w) stock)]))






(check-expect (find-widgets-cheaper-than--low (list Wire) 3) null) 
(check-expect (find-widgets-cheaper-than--low (widget-parts Telephone) 15) 
              (list Receiver Buttons Numbers Cord Wire))
(define (find-widgets-cheaper-than--low low maxprice)
  (cond
    [(empty? low) empty]
    [(cons? low)
     (append
      (find-widgets-cheaper-than
       (first low)
       maxprice)
      (find-widgets-cheaper-than--low
       (rest low)
       maxprice))]))






(check-expect (find-widgets-cheaper-than Wire 3) null) 
(check-expect (find-widgets-cheaper-than Wire 13)
              (list Wire)) 
(check-expect (find-widgets-cheaper-than Telephone 15) 
              (list Receiver Buttons Numbers Cord Wire))
(define (find-widgets-cheaper-than w maxprice)
  (cond
    [(< (widget-price w) maxprice)
     (cons w
           (find-widgets-cheaper-than--low
            (widget-parts w)
            maxprice))]
    [else (find-widgets-cheaper-than--low
           (widget-parts w)
           maxprice)]))







(check-expect (widget-is-hard? Wire 10 5) #t) 
(check-expect (widget-is-hard? Telephone 0 10000) #f) 
(define (widget-is-hard? w minquantity maxcost)
  (or (< (widget-quantity w) minquantity)
         (> (widget-price w) maxcost)))









(check-expect
 (find-widget-hard-make--low (widget-parts Telephone) 10 5)
 (list Receiver Buttons Numbers Cord Wire))

(check-expect
 (find-widget-hard-make--low (widget-parts Telephone) 0 10000)
 null)

(check-expect
 (find-widget-hard-make--low (widget-parts Wire) 10 5)
 null)
(define (find-widget-hard-make--low low minquantity maxcost)
  (cond
    [(empty? low) empty]
    [(cons? low)
     (append (find-widget-hard-make (first low) minquantity maxcost)
             (find-widget-hard-make--low (rest low) minquantity maxcost))]))







(check-expect (find-widget-hard-make Telephone 10 5)
              (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widget-hard-make Telephone 0 10000) 
              null)
(check-expect (find-widget-hard-make Wire 10 5) 
              (list Wire))
(define (find-widget-hard-make w maxquantity mincost)
  (cond
    [(widget-is-hard? w maxquantity mincost)
     (cons w (find-widget-hard-make--low (widget-parts w) maxquantity mincost))]
    [else (find-widget-hard-make--low (widget-parts w) maxquantity mincost)]))