

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Kai Nakamura and Keaton Mangone Assignment 3 Part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))















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











(check-expect (find-widget-longer-than Wire 2) (list Wire))

(check-expect (find-widget-longer-than Wire 4) empty)

(check-expect (find-widget-longer-than Telephone 4)
              (list Telephone Receiver Buttons Numbers))


(check-expect (find-widget-longer-than--low empty 4) empty)

(check-expect (find-widget-longer-than--low (list Wire) 4) empty)

(check-expect (find-widget-longer-than--low (list Receiver Buttons Cord) 4)
              (list Receiver Buttons Numbers))

(define (find-widget-longer-than widget length)
  (if (> (string-length (widget-name widget)) length)
      (cons widget (find-widget-longer-than--low (widget-parts widget) length))
      (find-widget-longer-than--low (widget-parts widget) length)))

(define (find-widget-longer-than--low low length)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-longer-than (first low) length)
                 (find-widget-longer-than--low (rest low) length))]))








(check-expect (find-widget-quantity-over Wire 2) (list Wire))

(check-expect (find-widget-quantity-over Wire 3) empty)

(check-expect (find-widget-quantity-over Telephone 7)
              (list Receiver Buttons Numbers))

(check-expect (find-widget-quantity-over Telephone 0)
              (list Telephone Receiver Buttons Numbers Cord Wire))

(check-expect (find-widget-quantity-over Jewelry 10) (list Rings Beads))


(check-expect (find-widget-quantity-over--low empty 0) empty)

(check-expect (find-widget-quantity-over--low (list Wire) 3) empty)

(check-expect (find-widget-quantity-over--low (list Receiver Buttons Cord) 0)
              (list Receiver Buttons Numbers Cord Wire))

(check-expect (find-widget-quantity-over--low (list Rings Necklace Bracelet) 10)
              (list Rings Beads))

(define (find-widget-quantity-over widget quantity)
  (if (> (widget-quantity widget) quantity)
      (cons widget
            (find-widget-quantity-over--low (widget-parts widget) quantity))
      (find-widget-quantity-over--low (widget-parts widget) quantity)))

(define (find-widget-quantity-over--low low quantity)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over (first low) quantity)
                 (find-widget-quantity-over--low (rest low) quantity))]))







(check-expect (find-widgets-cheaper-than Wire 100) (list Wire))

(check-expect (find-widgets-cheaper-than Wire 5) empty)

(check-expect (find-widgets-cheaper-than Wire 6) (list Wire))

(check-expect (find-widgets-cheaper-than Jewelry 10)
              (list Necklace Chain Pendant Bracelet Beads Glass))


(check-expect (find-widgets-cheaper-than--low empty 0) empty)

(check-expect (find-widgets-cheaper-than--low (list Wire) 5) empty)

(check-expect (find-widgets-cheaper-than--low (list Wire) 6) (list Wire))

(check-expect (find-widgets-cheaper-than Jewelry 10)
              (list Necklace Chain Pendant Bracelet Beads Glass))

(check-expect (find-widgets-cheaper-than--low (list Rings Necklace Bracelet) 10)
              (list Necklace Chain Pendant Bracelet Beads Glass))

(define (find-widgets-cheaper-than widget price)
  (if (< (widget-price widget) price)
      (cons widget
            (find-widgets-cheaper-than--low (widget-parts widget) price))
      (find-widgets-cheaper-than--low (widget-parts widget) price)))

(define (find-widgets-cheaper-than--low low price)
  (cond [(empty? low) empty]
        [else
         (append (find-widgets-cheaper-than (first low) price)
                 (find-widgets-cheaper-than--low (rest low) price))]))








(check-expect (find-widget-hard-make Wire 4 4) (list Wire))

(check-expect (find-widget-hard-make Wire 3 4) (list Wire))

(check-expect (find-widget-hard-make Wire 4 5) (list Wire))

(check-expect (find-widget-hard-make Wire 3 5) empty)

(check-expect (find-widget-hard-make Telephone 8 5)
              (list Telephone Receiver Cord Wire))

(check-expect (find-widget-hard-make Jewelry 8 10)
              (list Jewelry Rings Chain Pendant Bracelet Glass))


(check-expect (find-widget-hard-make--low empty 8 10) empty)

(check-expect (find-widget-hard-make--low (list Receiver Buttons Cord) 8 5)
              (list Receiver Cord Wire))

(check-expect (find-widget-hard-make--low (list Rings Necklace Bracelet) 8 10)
              (list Rings Chain Pendant Bracelet Glass))

(define (find-widget-hard-make widget quantity price)
  (if (or (< (widget-quantity widget) quantity)
          (> (widget-price widget) price))
      (cons widget
            (find-widget-hard-make--low (widget-parts widget) quantity price))
      (find-widget-hard-make--low (widget-parts widget) quantity price)))

(define (find-widget-hard-make--low low quantity price)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-hard-make (first low) quantity price)
                 (find-widget-hard-make--low (rest low) quantity price))]))
