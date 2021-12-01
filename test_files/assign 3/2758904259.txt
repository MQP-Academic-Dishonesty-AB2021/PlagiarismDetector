

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





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





(check-expect (find-widget-name-longer-than Wire 3) (list Wire)) 
(check-expect (find-widget-name-longer-than Buttons 6) (list Buttons Numbers)) 
(check-expect (find-widget-name-longer-than Chain 5) empty) 
(check-expect (find-widget-name-longer-than Telephone 7) (list Telephone Receiver)) 
(check-expect (find-widget-name-longer-than Telephone 10) empty) 





(define (find-widget-name-longer-than widget length)
  (if (> (string-length (widget-name widget)) length) 
      (cons widget (find-widget-name-longer-than--low (widget-parts widget) length))
      (find-widget-name-longer-than--low (widget-parts widget) length))) 



(define (find-widget-name-longer-than--low low length)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-name-longer-than (first low) length)
             (find-widget-name-longer-than--low (rest low) length))]))



(check-expect (find-widget-quantity-over Wire 2) (list Wire)) 
(check-expect (find-widget-quantity-over Buttons 7) (list Buttons Numbers)) 
(check-expect (find-widget-quantity-over Chain 7) empty) 
(check-expect (find-widget-quantity-over Telephone 7) (list Receiver Buttons Numbers)) 





(define (find-widget-quantity-over widget quantity)
  (if (> (widget-quantity widget) quantity) 
      (cons widget (find-widget-quantity-over--low (widget-parts widget) quantity))
      (find-widget-quantity-over--low (widget-parts widget) quantity))) 



(define (find-widget-quantity-over--low low quantity)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-quantity-over (first low) quantity)
             (find-widget-quantity-over--low (rest low) quantity))]))



(check-expect (find-widgets-cheaper-than Wire 7) (list Wire)) 
(check-expect (find-widgets-cheaper-than Buttons 8) (list Buttons Numbers)) 
(check-expect (find-widgets-cheaper-than Chain 1) empty) 
(check-expect (find-widgets-cheaper-than Telephone 6) (list Buttons Numbers Cord Wire)) 





(define (find-widgets-cheaper-than widget price)
  (if (< (widget-price widget) price) 
      (cons widget (find-widgets-cheaper-than--low (widget-parts widget) price))
      (find-widgets-cheaper-than--low (widget-parts widget) price))) 



(define (find-widgets-cheaper-than--low low price)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widgets-cheaper-than (first low) price)
             (find-widgets-cheaper-than--low (rest low) price))]))





(check-expect (find-widget-hard-make Wire 4 4) (list Wire)) 
(check-expect (find-widget-hard-make Wire 4 6) (list Wire)) 
(check-expect (find-widget-hard-make Wire 1 6) empty) 
(check-expect (find-widget-hard-make Buttons 10 4) (list Buttons Numbers)) 
(check-expect (find-widget-hard-make Buttons 9 5) (list Buttons)) 
(check-expect (find-widget-hard-make Chain 7 1) empty) 
(check-expect (find-widget-hard-make Telephone 9 5) (list Telephone Receiver Buttons Cord Wire)) 





(define (find-widget-hard-make widget quantity price)
  (if (hard-make? widget quantity price)
      (cons widget (find-widget-hard-make--low (widget-parts widget) quantity price))
      (find-widget-hard-make--low (widget-parts widget) quantity price)))



(define (find-widget-hard-make--low low quantity price)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-hard-make (first low) quantity price)
             (find-widget-hard-make--low (rest low) quantity price))]))






(check-expect (hard-make? Wire 4 4) true)
(check-expect (hard-make? Wire 1 6) false)



(define (hard-make? widget quantity price)
  (or (< (widget-quantity widget) quantity)
      (> (widget-price widget) price)))
