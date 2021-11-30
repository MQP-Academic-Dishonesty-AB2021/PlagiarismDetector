

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(define-struct widget (name quantity time price parts))












 

 




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









(check-expect (find-widget-name-longer-than Receiver 3) 
              (list (make-widget "Receiver" 10 5 7 empty))) 

(check-expect (find-widget-name-longer-than Rings 10) 
              empty)                                  
 
(check-expect (find-widget-name-longer-than Telephone 5) 
              (list Telephone Receiver Buttons Numbers))

(check-expect (find-widget-name-longer-than Jewelry 6) 
              (list Jewelry Necklace Pendant Bracelet))

(define (find-widget-name-longer-than wid len)
  (if (> (string-length (widget-name wid)) len)
       (cons wid (find-widget-name-longer-than--low (widget-parts wid) len))
       (find-widget-name-longer-than--low (widget-parts wid) len)))







(check-expect (find-widget-name-longer-than--low empty 0)
                empty)                               
(check-expect (find-widget-name-longer-than--low
                (widget-parts Telephone) 5) (list Receiver Buttons Numbers)) 
(check-expect (find-widget-name-longer-than--low (widget-parts Jewelry) 6)
                (list Necklace Pendant Bracelet)) 

(define (find-widget-name-longer-than--low low len)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-name-longer-than (first low) len)
           (find-widget-name-longer-than--low (rest low) len))]))
  







(check-expect (find-widget-quantity-over Wire 0)
                (list Wire))   
(check-expect (find-widget-quantity-over Wire 10)
                empty)         
(check-expect (find-widget-quantity-over Telephone 8)
                (list Receiver Numbers)) 
(check-expect (find-widget-quantity-over Telephone 0)
                (list Telephone Receiver Buttons Numbers Cord Wire))


(define (find-widget-quantity-over widget quant)
  (if (> (widget-quantity widget) quant)
      (cons widget (find-widget-quantity-over--low (widget-parts widget) quant))
      (find-widget-quantity-over--low (widget-parts widget) quant)))






(check-expect (find-widget-quantity-over--low empty 0)
                empty) 
(check-expect (find-widget-quantity-over--low
                (widget-parts Buttons) 0) (list Numbers)) 
(check-expect (find-widget-quantity-over--low (widget-parts Telephone) 0)
                (list Receiver Buttons Numbers Cord Wire)) 

(define (find-widget-quantity-over--low low quant)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-quantity-over (first low) quant)
             (find-widget-quantity-over--low (rest low) quant))]))







(check-expect (find-widgets-cheaper-than Beads 1) 
              empty)

(check-expect (find-widgets-cheaper-than Telephone 10) 
              (list Receiver Buttons Numbers Cord Wire))

(check-expect (find-widgets-cheaper-than Jewelry 8) 
              (list Necklace Chain Pendant Bracelet Beads Glass))

(define (find-widgets-cheaper-than wid price)
  (if (< (widget-price wid) price)
       (cons wid (find-widgets-cheaper-than--low (widget-parts wid) price))
       (find-widgets-cheaper-than--low (widget-parts wid) price)))






(check-expect (find-widgets-cheaper-than--low empty 15)
                empty)                               
(check-expect (find-widgets-cheaper-than--low
                (widget-parts Telephone) 6) (list Buttons Numbers Cord Wire)) 
(check-expect (find-widgets-cheaper-than--low (widget-parts Jewelry) 8)
                (list Necklace Chain Pendant Bracelet Beads Glass)) 


(define (find-widgets-cheaper-than--low low price)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widgets-cheaper-than (first low) price)
           (find-widgets-cheaper-than--low (rest low) price))]))










(check-expect (find-widget-hard-make Jewelry 0 100) empty) 
(check-expect (find-widget-hard-make Jewelry 30 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect (find-widget-hard-make Telephone 5 6)
              (list Telephone Receiver Wire)) 

(define (find-widget-hard-make widget quant cost)
  (if (isHardMake? widget quant cost)
      (cons widget (find-widget-hard-make--low (widget-parts widget) quant cost))
      (find-widget-hard-make--low (widget-parts widget) quant cost)))








(check-expect (isHardMake? Cord 10 0) true)
(check-expect (isHardMake? Numbers 0 0) true)
(check-expect (isHardMake? Receiver 5 10) false)

(define (isHardMake? widget quant cost)
  (or (< (widget-quantity widget) quant)
          (> (widget-price widget) cost)))






(check-expect (find-widget-hard-make--low empty 0 0)
              empty) 
(check-expect (find-widget-hard-make--low (widget-parts Cord) 5 0)
              (list Wire)) 
(check-expect (find-widget-hard-make--low (widget-parts Cord) 0 5)
              empty) 

(define (find-widget-hard-make--low low quant cost)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-hard-make (first low) quant cost)
             (find-widget-hard-make--low (rest low) quant cost))]))