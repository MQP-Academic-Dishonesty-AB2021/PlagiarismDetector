

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


 
 






(check-expect
 (find-widget-name-longer-than Jewelry 100) empty) 

(check-expect
 (find-widget-name-longer-than Jewelry 0)
 (list Jewelry Rings Necklace Chain Pendant 
       Bracelet Beads Glass))

(check-expect
 (find-widget-name-longer-than Telephone 7)
 (list Telephone Receiver))




(define (find-widget-name-longer-than wid length)
  (if (> (string-length (widget-name wid)) length)
      (cons wid (find-widget-name--list (widget-parts wid) length))
      (find-widget-name--list (widget-parts wid) length)))


(define (find-widget-name--list low length)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) length)
                 (find-widget-name--list (rest low) length))]))







(check-expect
 (find-widget-quantity-over Jewelry 30) empty) 

(check-expect
 (find-widget-quantity-over Telephone 6)
 (list Receiver Buttons Numbers Cord))

(check-expect
 (find-widget-quantity-over Jewelry 0)
 (list Jewelry Rings Necklace Chain
       Pendant Bracelet Beads Glass))




(define (find-widget-quantity-over wid quantity)
  (if (> (widget-quantity wid) quantity)
      (cons wid (find-widget-stock-list
                 (widget-parts wid) quantity))
      (find-widget-stock-list
       (widget-parts wid) quantity)))



(define (find-widget-stock-list low quantity)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over
                  (first low) quantity)
                 (find-widget-stock-list
                  (rest low) quantity))]))







(check-expect
 (find-widget-cheaper-than Jewelry 0) empty) 

(check-expect
 (find-widget-cheaper-than Jewelry 6)
 (list Necklace Chain Pendant Bracelet Glass))

(check-expect
 (find-widget-cheaper-than Telephone 6)
 (list Buttons Numbers Cord Wire))




(define (find-widget-cheaper-than wid price)
  (if (< (widget-price wid) price)
      (cons wid (find-widget-cheaper-than--list
                 (widget-parts wid) price))
      (find-widget-cheaper-than--list
       (widget-parts wid) price)))


(define (find-widget-cheaper-than--list low price)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-cheaper-than
                  (first low) price)
                 (find-widget-cheaper-than--list
                  (rest low) price))]))








(check-expect (find-widget-hard-make Telephone 0 50) empty) 

(check-expect
 (find-widget-hard-make Telephone 6 6)
 (list Telephone Receiver Wire))

(check-expect
 (find-widget-hard-make Jewelry 6 6)
 (list Jewelry Rings Pendant Bracelet Beads))




(define (find-widget-hard-make wid stock price)
  (if (is-hard? wid stock price)  
      (cons wid (find-widget-hard-make--list
                 (widget-parts wid) stock price))
      (find-widget-hard-make--list
       (widget-parts wid) stock price))) 



(define (find-widget-hard-make--list low stock price)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-hard-make
                  (first low) stock price)
                 (find-widget-hard-make--list
                  (rest low) stock price))]))








(check-expect (is-hard? Telephone 6 6) true)
(check-expect (is-hard? Cord 6 6) false)

(define (is-hard? wid stock price)
  (or (> (widget-price wid) price)
      (< (widget-quantity wid) stock)))