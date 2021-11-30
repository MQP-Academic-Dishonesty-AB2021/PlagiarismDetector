

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))







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










(check-expect (find-widget-name-longer-than Necklace 5)
              (list Necklace Pendant)) 
(check-expect (find-widget-name-longer-than Glass 5)
              empty) 
(check-expect (find-widget-name-longer-than Telephone 8)
              (list Telephone)) 



(define (find-widget-name-longer-than widget length)
  (if (> (string-length (widget-name widget)) length)
      (cons widget
            (find-widget-name-longer-than--low (widget-parts widget) length))
      (find-widget-name-longer-than--low (widget-parts widget) length)))




(check-expect (find-widget-name-longer-than--low (list Telephone) 9)
              empty)
(check-expect (find-widget-name-longer-than--low (list Telephone) 5)
              (list Telephone Receiver Buttons Numbers)) 
(check-expect (find-widget-name-longer-than--low empty 999)
              empty) 


              
(define (find-widget-name-longer-than--low low length)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) length)
                 (find-widget-name-longer-than--low (rest low) length))]))








(check-expect (find-widget-quantity-over Necklace 5)
              (list Necklace Chain)) 
(check-expect (find-widget-quantity-over Glass 6)
              empty) 
(check-expect (find-widget-quantity-over Telephone 8)
              (list Receiver Numbers)) 



(define (find-widget-quantity-over widget stock)
  (if (> (widget-quantity widget) stock)
      (cons widget (find-widget-quantity-over--low (widget-parts widget) stock))
      (find-widget-quantity-over--low (widget-parts widget) stock)))




(check-expect (find-widget-quantity-over--low empty 2) empty) 

(check-expect (find-widget-quantity-over--low (list Telephone Bracelet) 6)
              (list Receiver Buttons Numbers Cord Beads)) 
(check-expect (find-widget-quantity-over--low (list Receiver) 10)
              empty) 



(define (find-widget-quantity-over--low low stock)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over (first low) stock)
                 (find-widget-quantity-over--low (rest low) stock))]))










(check-expect (find-widget-cheaper-than Necklace 5)
              (list Necklace Chain Pendant)) 
(check-expect (find-widget-cheaper-than Glass 1)
              empty) 
(check-expect (find-widget-cheaper-than Telephone 8)
              (list Receiver Buttons Numbers Cord Wire)) 



(define (find-widget-cheaper-than widget price)
  (if (< (widget-price widget) price)
      (cons widget (find-widget-cheaper-than--low (widget-parts widget) price))
      (find-widget-cheaper-than--low (widget-parts widget) price)))





(check-expect (find-widget-cheaper-than--low empty 2) empty) 
(check-expect (find-widget-cheaper-than--low (list Telephone) 8)
              (list Receiver Buttons Numbers Cord Wire)) 



(define (find-widget-cheaper-than--low low price)
  (cond [(empty? low) empty]                   
        [else
         (append (find-widget-cheaper-than (first low) price)
                 (find-widget-cheaper-than--low (rest low) price))]))











(check-expect (find-widget-hard-make Necklace 5 4)
              (list Pendant)) 
(check-expect (find-widget-hard-make Glass 5 5)
              empty) 
(check-expect (find-widget-hard-make Telephone 4 5)
              (list Telephone Receiver Wire)) 



(define (find-widget-hard-make widget stock price)
  (if (hard-make? widget stock price)
      (cons widget (find-widget-hard-make--low (widget-parts widget) stock price))
      (find-widget-hard-make--low (widget-parts widget) stock price)))




(check-expect (hard-make? Telephone 6 20) true) 
(check-expect (hard-make? Telephone 5 14) true) 
(check-expect (hard-make? Telephone 4 20) false) 



(define (hard-make? widget stock price)
  (or (< (widget-quantity widget) stock)
      (> (widget-price widget) price)))





(check-expect (find-widget-hard-make--low (list Telephone Bracelet Jewelry) 7 20)
              (list Telephone Wire Bracelet Glass Jewelry Pendant Bracelet Glass))
              

(check-expect (find-widget-hard-make--low (list Telephone) 1 200)
              empty) 
(check-expecct (find-widget-hard-make--low (list Telephone) 5 7)
               (list Telephone Wire))



(define (find-widget-hard-make--low low stock price)
  (cond [(empty? low) empty]                   
        [else
         (append (find-widget-hard-make (first low) stock price)
                 (find-widget-hard-make--low (rest low) stock price))]))