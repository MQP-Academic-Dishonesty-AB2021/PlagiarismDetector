

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

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
(define Jewel (make-widget "Jewel" 0 17 300
                           empty))
(define Rings (make-widget "Rings" 15 8 11 empty))
(define Jewelry (make-widget "Jewelry set" 4 17 30
                             (list Rings Necklace Bracelet)))

 
 



 
(check-expect (find-widget-name-longer-than Rings 2) 
              (list Rings))
(check-expect (find-widget-name-longer-than Rings 7) 
              empty)
(check-expect (find-widget-name-longer-than Cord 3) 
              (list Cord Wire))
(check-expect (find-widget-name-longer-than Cord 4) 
              empty)
(define (find-widget-name-longer-than widget length)
  (if (< length (string-length (widget-name widget)))
      (cons widget
            (find-widget-name-longer-than--low (widget-parts widget) length))
      (find-widget-name-longer-than--low (widget-parts widget) length)))



(check-expect (find-widget-name-longer-than--low empty 0) empty)  
(check-expect (find-widget-name-longer-than--low 
               (list Cord Numbers) 2)
              (list Cord Wire Numbers))
(check-expect (find-widget-name-longer-than--low 
               (list Numbers Cord) 4)
              (list Numbers))
 

(define (find-widget-name-longer-than--low low length)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) length)
                 (find-widget-name-longer-than--low (rest low) length))]))




(check-expect (find-widget-quantity-over Rings 0) (list Rings)) 
(check-expect (find-widget-quantity-over Rings 15) empty) 
(check-expect (find-widget-quantity-over Rings 14) (list Rings)) 
(check-expect (find-widget-quantity-over Cord 5) (list Cord)) 
(check-expect (find-widget-quantity-over Cord 2) (list Cord Wire)) 
(check-expect (find-widget-quantity-over Buttons 8) 
              (list Numbers))
 

(define (find-widget-quantity-over widget quantity)
  (if (< quantity (widget-quantity widget))
      (cons widget (find-widget-quantity-over--low (widget-parts widget)
                                                   quantity))
      (find-widget-quantity-over--low (widget-parts widget) quantity)))




(check-expect (find-widget-quantity-over--low (list Rings) 0) 
              (list Rings))
(check-expect (find-widget-quantity-over--low empty 15) empty) 
(check-expect (find-widget-quantity-over--low (list Cord Rings) 2) 
              (list Cord Wire Rings))
(check-expect (find-widget-quantity-over--low (list Buttons) 8) 
              (list Numbers))
 

(define (find-widget-quantity-over--low low quantity)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over (first low) quantity)
                 (find-widget-quantity-over--low (rest low) quantity))]))




(check-expect (find-widgets-cheaper-than Telephone 0) empty) 
(check-expect (find-widgets-cheaper-than Cord 7)(list Cord Wire)) 
(check-expect (find-widgets-cheaper-than Cord 5) empty) 
 

(define (find-widgets-cheaper-than widget price)
  (if (< (widget-price widget) price)
      (cons widget (find-widgets-cheaper-than--low (widget-parts widget) price))
      (find-widgets-cheaper-than--low (widget-parts widget) price)))




 
(check-expect (find-widgets-cheaper-than--low empty 0) empty) 
(check-expect (find-widgets-cheaper-than--low (list Telephone) 16) 
              (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widgets-cheaper-than--low (list Telephone) 7) 
              (list Buttons Numbers Cord Wire))
(check-expect (find-widgets-cheaper-than--low (list Cord Wire) 6) 
              (list Cord Wire Wire))
(check-expect (find-widgets-cheaper-than--low (list Cord Receiver) 6) 
              (list Cord Wire))

(define (find-widgets-cheaper-than--low low price)
  (cond [(empty? low) empty]
        [else
         (append (find-widgets-cheaper-than (first low) price)
                 (find-widgets-cheaper-than--low (rest low) price))]))




 
(check-expect (find-widget-hard-make Telephone 4 19) (list Wire)) 
(check-expect (find-widget-hard-make Telephone 1 20) empty) 
(check-expect (find-widget-hard-make Jewelry 5 15) 
              (list Jewelry Pendant))

(define (find-widget-hard-make widget quantity price)
  (if (or (< (widget-quantity widget) quantity)
          (< price (widget-price widget)))
      (cons widget (find-widget-hard-make--low (widget-parts widget)
                                               quantity price))
      (find-widget-hard-make--low (widget-parts widget) quantity price)))





 
(check-expect (find-widget-hard-make--low empty 10 10) empty) 
(check-expect (find-widget-hard-make--low (list Telephone) 4 19) 
              (list Wire))
(check-expect (find-widget-hard-make--low (list Jewelry Telephone) 100 1) 
              (list Jewelry Rings Necklace Chain Pendant
                    Bracelet Beads Glass
                    Telephone Receiver Buttons Numbers
                    Cord Wire))
(check-expect (find-widget-hard-make--low (list Glass) 1 10) empty) 

(define (find-widget-hard-make--low low quantity price)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-hard-make (first low) quantity price)
                 (find-widget-hard-make--low (rest low) quantity price))]))