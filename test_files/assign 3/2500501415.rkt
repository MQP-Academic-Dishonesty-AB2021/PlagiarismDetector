

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


 

 










(check-expect (find-widget-name-longer-than Telephone 5)
              (list Telephone Receiver Buttons Numbers))     
(check-expect (find-widget-name-longer-than Jewelry 6)
              (list Jewelry Necklace Pendant Bracelet))       
(check-expect (find-widget-name-longer-than Telephone 13)
              empty)

(define (find-widget-name-longer-than widget length)
  (cond [(> (string-length (widget-name widget)) length)
         (cons widget (length-low (widget-parts widget) length))]
        [else
         (length-low (widget-parts widget) length)]))



(define (length-low low length)             
  (cond [(empty? low) empty]
        [else
         (append
          (find-widget-name-longer-than (first low) length)
          (length-low (rest low) length))]))

(check-expect (length-low (widget-parts Telephone) 5)
              (list Receiver Buttons Numbers))
(check-expect (length-low (widget-parts Jewelry) 6)
              (list Necklace Pendant Bracelet))
(check-expect (length-low (widget-parts Telephone) 13)
              empty)
 











(check-expect (find-widget-quantity-over Cord 5) (list Cord))
(check-expect (find-widget-quantity-over Telephone 8) (list Receiver Numbers))
(check-expect (find-widget-quantity-over Jewelry 9) (list Rings Necklace Beads))

(define (find-widget-quantity-over widget amount)
  (cond [(> (widget-quantity widget) amount)
         (cons widget (quantity-low (widget-parts widget) amount))]
        [else
         (quantity-low (widget-parts widget) amount)]))




(define (quantity-low low amount)             
  (cond [(empty? low) empty]
        [else
         (append
          (find-widget-quantity-over (first low) amount)
          (quantity-low (rest low) amount))]))

(check-expect (quantity-low (widget-parts Cord) 5)
              empty)
(check-expect (quantity-low (widget-parts Telephone) 8)
              (list Receiver Numbers))
(check-expect (quantity-low (widget-parts Jewelry) 9)
              (list Rings Necklace Beads))












(check-expect (find-widgets-cheaper-than Numbers 4) empty)
(check-expect (find-widgets-cheaper-than Numbers 6) (list Numbers))
(check-expect (find-widgets-cheaper-than Telephone 6) (list Buttons Numbers Cord Wire))

(define (find-widgets-cheaper-than widget price)
  (cond [(< (widget-price widget) price)
         (cons widget (price-low (widget-parts widget) price))]
        [else
         (price-low (widget-parts widget) price)]))




(define (price-low low price)             
  (cond [(empty? low) empty]
        [else
         (append
          (find-widgets-cheaper-than (first low) price)
          (price-low (rest low) price))]))

(check-expect (price-low (widget-parts Numbers) 4)
              empty)
(check-expect (price-low (widget-parts Telephone) 6)
              (list Buttons Numbers Cord Wire))
(check-expect (price-low (widget-parts Jewelry) 3)
              (list Chain Pendant))












(check-expect (find-widget-hard-make Telephone 8 6)
              (list Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget-hard-make Necklace 5 2)
              (list Necklace Chain Pendant))
(check-expect (find-widget-hard-make Bracelet 25 2)
              empty)

(define (find-widget-hard-make widget quantity price)
  (cond [(small-price-or-high-quantity? widget quantity price)
         (cons widget (hard-make-low (widget-parts widget) quantity price))]
        [else
         (hard-make-low (widget-parts widget) quantity price)]))



(define (hard-make-low low quantity price)
  (cond [(empty? low) empty]
        [else
         (append
          (find-widget-hard-make (first low) quantity price)
          (hard-make-low (rest low) quantity price))]))
(check-expect (hard-make-low (widget-parts Telephone) 8 6)
              (list Receiver Buttons Numbers Cord Wire))
(check-expect (hard-make-low (widget-parts Necklace) 5 2)
              (list Chain Pendant))
(check-expect (hard-make-low (widget-parts Bracelet) 25 2)
              empty)





(check-expect (small-price-or-high-quantity? Telephone 8 6) false)
(check-expect (small-price-or-high-quantity? Beads 24 1) true)
(check-expect (small-price-or-high-quantity? Wire 2 2) true)
(check-expect (small-price-or-high-quantity? Wire 3 3) false)

(define (small-price-or-high-quantity? widget quantity price)
  (or (> (widget-quantity widget) quantity)
             (< (widget-price widget) price)))