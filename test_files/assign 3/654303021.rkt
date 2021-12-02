

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))









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





 
 






(check-expect (find-widget-name-longer-than Glass 2) (list Glass)) 
(check-expect (find-widget-name-longer-than Glass 6) empty) 
(check-expect (find-widget-name-longer-than Cord 3) (list Cord Wire)) 
(check-expect (find-widget-name-longer-than Jewelry 12) empty) 

(define (find-widget-name-longer-than widget num)
  (if (> (string-length (widget-name widget)) num)
      (cons widget (examine-children-names (widget-parts widget) num))
      (examine-children-names (widget-parts widget) num)))

(define (examine-children-names low num)
  (cond [(empty? low) empty]
        [else (append (find-widget-name-longer-than (first low) num)
                   (examine-children-names (rest low) num))]))



(check-expect (find-widget-quantity-over Wire 3) empty) 
(check-expect (find-widget-quantity-over Wire 2) (list Wire)) 
(check-expect (find-widget-quantity-over Cord 5) (list Cord)) 
(check-expect (find-widget-quantity-over Cord 2) (list Cord Wire)) 

(define (find-widget-quantity-over widget num)
  (if (> (widget-quantity widget) num)
      (cons widget (examine-children-quantity (widget-parts widget) num))
      (examine-children-quantity (widget-parts widget) num)))

(define (examine-children-quantity low num)
  (cond [(empty? low) empty]
        [else (append (find-widget-quantity-over (first low) num)
                   (examine-children-quantity (rest low) num))]))



(check-expect (find-widgets-cheaper-than Wire 5) empty) 
(check-expect (find-widgets-cheaper-than Wire 6) (list Wire)) 
(check-expect (find-widgets-cheaper-than Bracelet 8) (list Bracelet Beads Glass)) 
(check-expect (find-widgets-cheaper-than Bracelet 6) (list Bracelet Glass)) 

(define (find-widgets-cheaper-than widget num)
  (if (< (widget-price widget) num)
       (cons widget (examine-children-price (widget-parts widget) num))
       (examine-children-price (widget-parts widget) num)))

(define (examine-children-price low num)
  (cond [(empty? low) empty]
        [else (append (find-widgets-cheaper-than (first low) num)
                   (examine-children-price (rest low) num))]))



(check-expect (find-widget-hard-make Wire 3 5) empty) 
(check-expect (find-widget-hard-make Wire 6 5) (list Wire)) 
(check-expect (find-widget-hard-make Wire 3 4) (list Wire)) 
(check-expect (find-widget-hard-make Wire 6 4) (list Wire)) 
(check-expect (find-widget-hard-make Beads 6 7) empty) 
(check-expect (find-widget-hard-make Beads 6 6) (list Beads)) 
(check-expect (find-widget-hard-make Beads 7 10) (list Glass)) 
(check-expect (find-widget-hard-make Beads 26 5) (list Beads Glass)) 

(define (find-widget-hard-make widget nat num)
  (if (or (< (widget-quantity widget) nat) (> (widget-price widget) num))
      (cons widget (examine-children-quantity-cost (widget-parts widget) nat num))
      (examine-children-quantity-cost (widget-parts widget) nat num)))

(define (examine-children-quantity-cost low nat num)
  (cond [(empty? low) empty]
        [else (append (find-widget-hard-make (first low) nat num)
                   (examine-children-quantity-cost (rest low) nat num))]))