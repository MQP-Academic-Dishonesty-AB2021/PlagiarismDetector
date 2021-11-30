

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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







(check-expect (find-widget-name-longer-than Wire 20) empty)


(check-expect (find-widget-name-longer-than Telephone 100) empty)


(check-expect (find-widget-name-longer-than Telephone 6)
              (list Telephone Receiver Buttons Numbers))


(check-expect (find-widget-name-longer-than Telephone 0)
              (list Telephone Receiver Buttons Numbers Cord Wire))


(check-expect (find-widget-name-longer-than Cord 3)(list Cord Wire))



(define (find-widget-name-longer-than root min-len)
  (if (> (string-length (widget-name root)) min-len)
      (cons root (find-widget-name-longer-than-low (widget-parts root) min-len))
      (find-widget-name-longer-than-low (widget-parts root) min-len)))







(check-expect (find-widget-name-longer-than-low (list Receiver Buttons Cord) 6)
              (list Receiver Buttons Numbers))


(check-expect (find-widget-name-longer-than-low empty 20) empty) 


(check-expect (find-widget-name-longer-than-low (list Receiver) 2) (list Receiver)) 


(check-expect (find-widget-name-longer-than-low (list Buttons) 1) (list Buttons Numbers)) 
               


(define (find-widget-name-longer-than-low low min-len)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) min-len)
                 (find-widget-name-longer-than-low (rest low) min-len))]))









(check-expect (find-widget-quantity-over Glass 20) empty) 


(check-expect (find-widget-quantity-over Bracelet 5)(list Beads Glass))

 
(check-expect (find-widget-quantity-over Jewelry 9)(list Rings Necklace Beads))

(define (find-widget-quantity-over root min-stock)
  (if (> (widget-quantity root) min-stock)
      (cons root (find-widget-quantity-over-low (widget-parts root) min-stock))
      (find-widget-quantity-over-low (widget-parts root) min-stock)))









(check-expect (find-widget-quantity-over-low empty 10) empty) 


(check-expect (find-widget-quantity-over-low (list Beads Chain) 6) (list Beads Chain)) 


(check-expect (find-widget-quantity-over-low (list Rings Necklace Chain) 5)
              (list Rings Necklace Chain Chain))


(define (find-widget-quantity-over-low low min-stock)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over (first low) min-stock)
                 (find-widget-quantity-over-low (rest low) min-stock))]))










(check-expect (find-widgets-cheaper-than Wire 4) empty) 


(check-expect (find-widgets-cheaper-than Cord 9)(list Wire))


(check-expect (find-widgets-cheaper-than Telephone 10)(list Receiver Buttons Numbers Wire)) 

(define (find-widgets-cheaper-than root max-price)
  (if (< (widget-price root) max-price)
      (cons root (find-widgets-cheaper-than-low (widget-parts root) max-price))
      (find-widgets-cheaper-than-low (widget-parts root) max-price)))










(check-expect (find-widgets-cheaper-than-low empty 10) empty) 


(check-expect (find-widgets-cheaper-than-low (list Buttons Cord) 10) (list Buttons Numbers Wire)) 

 
(check-expect (find-widgets-cheaper-than-low (list Receiver Cord Wire) 8) (list Receiver Wire Wire))

 
(check-expect (find-widgets-cheaper-than-low (list Buttons Bracelet) 8)
              (list Buttons Bracelet Beads Glass))

(define (find-widgets-cheaper-than-low low max-price)
  (cond [(empty? low) empty]
        [else
         (append (find-widgets-cheaper-than (first low) max-price)
                 (find-widgets-cheaper-than-low (rest low) max-price))]))









(check-expect (find-widget-hard-make Rings 0 120) empty) 


(check-expect (find-widget-hard-make Necklace 5 2) (list Necklace Pendant)) 


(check-expect (find-widget-hard-make Jewelry 10 10)
              (list Jewelry Rings Chain Pendant Bracelet Glass))


(define (find-widget-hard-make root max-stock min-price)
  (if (or (< (widget-quantity root) max-stock) (> (widget-price root) min-price))
      (cons root (find-widget-hard-make-low (widget-parts root) max-stock min-price))
      (find-widget-hard-make-low (widget-parts root) max-stock min-price)))








(check-expect ( find-widget-hard-make-low empty 12 12) empty)


(check-expect ( find-widget-hard-make-low (list Necklace) 5 2) (list Necklace Pendant)) 


(check-expect (find-widget-hard-make-low (list Jewelry) 10 10)
              (list Jewelry Rings Chain Pendant Bracelet Glass))


(define (find-widget-hard-make-low low max-stock min-price)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-hard-make (first low) max-stock min-price)
                 (find-widget-hard-make-low (rest low) max-stock min-price))]))