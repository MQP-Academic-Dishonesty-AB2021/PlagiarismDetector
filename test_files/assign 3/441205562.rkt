

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assignment3_part_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(define-struct widget(name quantity time price parts))



(define (fn-for-widget widget)
  (... (widget-name widget)
       (widget-quantity widget)
       (widget-time widget)
       (widget-price widget)
       (fn-for-low (widget-parts widget))))

(define (fn-for-low low)
  (...
   (cond
     [(empty? low) ...]
     [else
      (... (fn-for-widget (first low))
           (fn-for-low (rest low)))])))
      


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





(check-expect (find-widget-name-longer-than Wire 10) empty)


(check-expect (find-widget-name-longer-than Wire 4) empty)


(check-expect (find-widget-name-longer-than Telephone 5)
              (list Telephone Receiver Buttons Numbers))


(check-expect (find-widget-name-longer-than Jewelry 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

  

(define (find-widget-name-longer-than widget length)
  (if (> (string-length (widget-name widget)) length)
      (cons widget (length-low (widget-parts widget) length))
      (length-low (widget-parts widget) length)))






(check-expect (length-low (widget-parts Wire) 2) empty)


(check-expect (length-low (widget-parts Telephone) 5)
              (list Receiver Buttons Numbers))


(check-expect (length-low (widget-parts Jewelry) 0)
              (list Rings Necklace Chain Pendant Bracelet Beads Glass))
  

(define (length-low low length)
  (if (empty? low) empty
      (append (find-widget-name-longer-than (first low) length)
              (length-low (rest low) length))))










(check-expect (find-widget-quantity-over Wire 2) (list Wire))


(check-expect (find-widget-quantity-over Wire 3) empty)


(check-expect (find-widget-quantity-over Telephone 4)
              (list Telephone Receiver Buttons Numbers Cord))


(check-expect (find-widget-quantity-over Jewelry 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

  

(define (find-widget-quantity-over widget quantity)
  (if (> (widget-quantity widget) quantity)
      (cons widget (quantity-low (widget-parts widget) quantity))
      (quantity-low (widget-parts widget) quantity)))






(check-expect (quantity-low (widget-parts Wire) 2) empty)


(check-expect (quantity-low (list Telephone) 4)
              (list Telephone Receiver Buttons Numbers Cord))


(check-expect (quantity-low (list Jewelry) 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
  

(define (quantity-low low quantity)
  (if (empty? low) empty
      (append (find-widget-quantity-over (first low) quantity)
              (quantity-low (rest low) quantity))))











(check-expect (find-widgets-cheaper-than Wire 2) empty)


(check-expect (find-widgets-cheaper-than Wire 5) empty)


(check-expect (find-widgets-cheaper-than Wire 6) (list Wire))


(check-expect (find-widgets-cheaper-than Telephone 10)
              (list Receiver Buttons Numbers Cord Wire))


(check-expect (find-widgets-cheaper-than Jewelry 50)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

  

(define (find-widgets-cheaper-than widget price)
  (if (< (widget-price widget) price)
      (cons widget (price-low (widget-parts widget) price))
      (price-low (widget-parts widget) price)))






(check-expect (price-low (widget-parts Wire) 2) empty)


(check-expect (price-low (list Telephone) 20)
              (list Telephone Receiver Buttons Numbers Cord Wire))


(check-expect (price-low (list Jewelry) 7)
              (list Necklace Chain Pendant Bracelet Glass))
  

(define (price-low low price)
  (if (empty? low) empty
      (append (find-widgets-cheaper-than (first low) price)
              (price-low (rest low) price))))















(check-expect (find-widget-hard-make Wire 2) (list Wire))


(check-expect (find-widget-hard-make Wire 6) (list Wire))


(check-expect (find-widget-hard-make Telephone 10)
              (list Telephone Buttons Numbers Cord Wire))


(check-expect (find-widget-hard-make Jewelry 50)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))


(check-expect (find-widget-hard-make Jewelry 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

  

(define (find-widget-hard-make widget hard)
  (if (or (< (widget-quantity widget) hard) (> (widget-price widget) hard))
      (cons widget (hard-low (widget-parts widget) hard))
      (hard-low (widget-parts widget) hard)))






(check-expect (hard-low (widget-parts Wire) 2) empty)


(check-expect (hard-low (list Telephone) 20)
              (list Telephone Receiver Buttons Numbers Cord Wire))


(check-expect (hard-low (list Jewelry) 7)
              (list Jewelry Rings Pendant Bracelet Glass))
  

(define (hard-low low hard)
  (if (empty? low) empty
      (append (find-widget-hard-make (first low) hard)
              (hard-low (rest low) hard))))