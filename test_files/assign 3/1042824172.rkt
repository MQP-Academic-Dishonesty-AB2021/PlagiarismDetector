

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




(check-expect (find-widget-name-longer-than (make-widget "Empty" 0 0 0 empty) 5) empty)


(check-expect (find-widget-name-longer-than Wire 3) (list Wire))


(check-expect (find-widget-name-longer-than Cord 5) empty)


(check-expect (find-widget-name-longer-than Telephone 5)
              (list Telephone Receiver Buttons Numbers))

(check-expect (find-widget-name-longer-than Telephone 3)
              (list Telephone Receiver Buttons Numbers Cord Wire))


(check-expect (find-widget-name-longer-than Cord 4) empty)





 




 
(define list-names-longer-than empty)

(define (find-widget-name-longer-than wid number)
  (if (> (string-length (widget-name wid)) number) 
      (cons wid
            (fn-for-low (widget-parts wid) number))
      (fn-for-low (widget-parts wid) number))) 

(define (fn-for-low low number)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) number)
                 (fn-for-low (rest low) number))]))





(check-expect(find-widget-quantity-over
              (make-widget "empty" 0 0 0 empty) 0) empty)


(check-expect(find-widget-quantity-over Wire 5) empty)


(check-expect(find-widget-quantity-over Buttons 8) (list Numbers))


(check-expect(find-widget-quantity-over Telephone 4)
             (list Telephone Receiver Buttons Numbers Cord))


(check-expect(find-widget-quantity-over Telephone 11) empty)

(check-expect(find-widget-quantity-over Telephone 9) (list Receiver))





 




 




(define (find-widget-quantity-over wid number)
  (if (> (widget-quantity wid) number)
      (cons wid (fn-for-low-quantity(widget-parts wid) number))
      (fn-for-low-quantity(widget-parts wid) number)))

(define (fn-for-low-quantity low number)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over (first low) number)
                 (fn-for-low-quantity (rest low) number))]))





(check-expect (find-widgets-cheaper-than (make-widget "Empty" 0 0 0 empty) 0) empty)



(check-expect (find-widgets-cheaper-than Wire 4) empty)


(check-expect (find-widgets-cheaper-than Cord 5) empty)


(check-expect (find-widgets-cheaper-than Telephone 14.50)
              (list Receiver Buttons Numbers Cord Wire))


(check-expect (find-widgets-cheaper-than Jewelry 10)
              (list Necklace Chain Pendant Bracelet Beads Glass))


(check-expect (find-widgets-cheaper-than Bracelet 8)
              (list Bracelet Beads Glass))





 




 

(define (find-widgets-cheaper-than wid num)
  (if (< (widget-price wid) num)
      (cons wid (fn-for-low-price (widget-parts wid) num))
      (fn-for-low-price (widget-parts wid) num)))
 

(define (fn-for-low-price low num)
  (cond [(empty? low)empty]
        [else
         (append (find-widgets-cheaper-than (first low) num)
                 (fn-for-low-price (rest low) num))]))





(check-expect (find-widget-hard-make
               (make-widget "Empty" 0 0 0 empty) 0 0) empty)


(check-expect (find-widget-hard-make Wire 10 4.5)
              (list Wire))

(check-expect (find-widget-hard-make Buttons 10 4.5)
              (list Buttons Numbers))

(check-expect (find-widget-hard-make Telephone 7 6)
              (list Telephone Receiver Wire))




 



 



(define (find-widget-hard-make wid quant price)
  (if (or ( <(widget-quantity wid) quant) (> (widget-price wid) price))
      (cons wid (fn-for-low-hard-make (widget-parts wid) quant price))
      (fn-for-low-hard-make (widget-parts wid) quant price)))
                    

(define (fn-for-low-hard-make low quant price)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-hard-make (first low) quant price)
                 (fn-for-low-hard-make (rest low) quant price))]))








