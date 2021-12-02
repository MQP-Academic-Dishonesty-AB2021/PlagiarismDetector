

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))







(define-struct widget (name quantity time price parts))







(define W1 (make-widget "" 0 0 0 empty))

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

  






(define LOW1 empty)
(define LOW2 (list Telephone Wire Wire Cord))
(define LOW3 (list Jewelry Chain Bracelet Glass))

 









(check-expect (find-widget-name-longer-than--widget W1 0)
              empty) 
(check-expect (find-widget-name-longer-than--widget Telephone 7) 
              (list Telephone Receiver))
(check-expect (find-widget-name-longer-than--widget Jewelry 7) 
              (list Jewelry Necklace Bracelet))

(check-expect (find-widget-name-longer-than--low empty 0)
              empty) 
(check-expect (find-widget-name-longer-than--low (list Telephone Jewelry) 7)
              (list Telephone Receiver Jewelry Necklace Bracelet)) 




(define (find-widget-name-longer-than--widget w len)
  (if (> (string-length (widget-name w)) len)
      (cons w (find-widget-name-longer-than--low (widget-parts w) len))
      (find-widget-name-longer-than--low (widget-parts w) len)))

(define (find-widget-name-longer-than--low low len)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than--widget (first low) len)
                 (find-widget-name-longer-than--low (rest low) len))]))






(check-expect (find-widget-quantity-over--widget W1 0)
              empty) 
(check-expect (find-widget-quantity-over--widget Telephone 5) 
              (list Receiver Buttons Numbers Cord))
(check-expect (find-widget-quantity-over--widget Jewelry 7) 
              (list Rings Necklace Beads))

(check-expect (find-widget-quantity-over--low empty 0)
              empty) 
(check-expect (find-widget-quantity-over--low (list Telephone Jewelry) 7)
              (list Receiver Buttons Numbers Rings Necklace Beads)) 




(define (find-widget-quantity-over--widget w num)
  (if (> (widget-quantity w) num)
      (cons w (find-widget-quantity-over--low (widget-parts w) num))
      (find-widget-quantity-over--low (widget-parts w) num)))

(define (find-widget-quantity-over--low low num)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over--widget (first low) num)
                 (find-widget-quantity-over--low (rest low) num))]))






(check-expect (find-widgets-cheaper-than--widget W1 0)
              empty) 
(check-expect (find-widgets-cheaper-than--widget Telephone 6) 
              (list Buttons Numbers Cord Wire))
(check-expect (find-widgets-cheaper-than--widget Jewelry 10) 
              (list Necklace Chain Pendant Bracelet Beads Glass))

(check-expect (find-widgets-cheaper-than--low empty 0)
              empty) 
(check-expect (find-widgets-cheaper-than--low (list Telephone Jewelry)
                                              6) 
              (list Buttons Numbers Cord Wire Necklace Chain Pendant Bracelet Glass))




(define (find-widgets-cheaper-than--widget w price)
  (if (< (widget-price w) price)
      (cons w (find-widgets-cheaper-than--low (widget-parts w) price))
      (find-widgets-cheaper-than--low (widget-parts w) price)))

(define (find-widgets-cheaper-than--low low price)
  (cond [(empty? low) empty]
        [else
         (append (find-widgets-cheaper-than--widget (first low) price)
                 (find-widgets-cheaper-than--low (rest low) price))]))







(check-expect (find-widget-hard-make--widget W1 0 0)
              empty) 
(check-expect (find-widget-hard-make--widget Telephone 4 6) 
              (list Telephone Receiver Wire))
(check-expect (find-widget-hard-make--widget Jewelry 7 10) 
              (list Jewelry Rings Pendant Bracelet Glass))

(check-expect (find-widget-hard-make--low empty 0 0)
              empty) 
(check-expect (find-widget-hard-make--low (list Telephone Jewelry) 5 7)
              (list Telephone Wire Jewelry Rings Pendant)) 




(define (find-widget-hard-make--widget w num price)
  (if (or (< (widget-quantity w) num) (> (widget-price w) price))
      (cons w (find-widget-hard-make--low (widget-parts w) num price))
      (find-widget-hard-make--low (widget-parts w) num price)))

(define (find-widget-hard-make--low low num price)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-hard-make--widget (first low) num price)
                 (find-widget-hard-make--low (rest low) num price))]))