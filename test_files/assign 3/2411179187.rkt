

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget(name quantity time price parts))


(define (fn-for-widget wid)
  (... (widget-name wid)
       (widget-quantity wid)
       (widget-time wid)
       (widget-price wid)
       (fn-for-low (widget-parts wid))))

(define (fn-for-low low)
  (cond
    [(empty? low) ...]
    [else
     (... (fn-for-widget (first low))
          (fn-for-low (rest low)))]))
        



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







(check-expect (find-widget-name-longer-than Jewelry 5)
              (list Jewelry Necklace Pendant Bracelet))


(check-expect (find-widget-name-longer-than Bracelet 10) empty)

(check-expect (find-widget-name-longer-than Numbers 3) (list Numbers))






(check-expect (name-longer--low empty 0) empty)

(check-expect (name-longer--low (list Rings Necklace Bracelet) 5)
              (list Necklace Pendant Bracelet))

(check-expect (name-longer--low (list Wire) 6) empty)



(define (find-widget-name-longer-than wid len)
  (if (> (string-length (widget-name wid)) len)
      (cons wid
            (name-longer--low (widget-parts wid) len))
      (name-longer--low (widget-parts wid) len)))

(define (name-longer--low low len)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-name-longer-than (first low) len)
          (name-longer--low (rest low) len))]))





(check-expect (find-widget-quantity-over Telephone 5)
              (list Receiver Buttons Numbers Cord))

(check-expect (find-widget-quantity-over Wire 7) empty)

(check-expect (find-widget-quantity-over Necklace 0)
              (list Necklace Chain Pendant))
                



(check-expect (quantity-over--low empty 0) empty) 
(check-expect (quantity-over--low (list Chain Pendant) 5) (list Chain)) 
(check-expect (quantity-over--low (list Glass) 7) empty) 





(define (find-widget-quantity-over wid quan)
  (if (>  (widget-quantity wid) quan)
      (cons wid
            (quantity-over--low (widget-parts wid) quan))
      (quantity-over--low (widget-parts wid) quan)))

(define (quantity-over--low low quan)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-quantity-over (first low) quan)
          (quantity-over--low (rest low) quan))]))





(check-expect (find-widgets-cheaper-than Telephone 8)
              (list Receiver Buttons Numbers Cord Wire))

(check-expect (find-widgets-cheaper-than Wire 2.5) empty)

(check-expect (find-widgets-cheaper-than Necklace 15)
              (list Necklace Chain Pendant))
                



(check-expect (cheaper-than--low empty 0) empty) 
(check-expect (cheaper-than--low (list Chain Pendant) 5)
              (list Chain Pendant)) 
(check-expect (cheaper-than--low (list Glass) 4) empty) 





(define (find-widgets-cheaper-than wid price)
  (if (<  (widget-price wid) price)
      (cons wid
            (cheaper-than--low (widget-parts wid) price))
      (cheaper-than--low (widget-parts wid) price)))

(define (cheaper-than--low low price)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widgets-cheaper-than (first low) price)
          (cheaper-than--low (rest low) price))]))






(check-expect (find-widget-hard-make Telephone 8 4)
              (list Telephone Receiver Buttons Numbers Cord Wire))

(check-expect (find-widget-hard-make Wire 2 5) empty)

(check-expect (find-widget-hard-make Necklace 1 2) (list Necklace)) 
                



(check-expect (hard-make--low empty 0 0) empty) 
(check-expect (hard-make--low (list Chain Pendant) 1 2) empty) 
(check-expect (hard-make--low (list Glass) 7 2) (list Glass)) 





(define (find-widget-hard-make wid quan price)
  (if (or (>  (widget-price wid) price) (< (widget-quantity wid) quan))
      (cons wid
            (hard-make--low (widget-parts wid) quan price))
      (hard-make--low (widget-parts wid) quan price)))

(define (hard-make--low low quan price)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-hard-make (first low) quan price)
          (hard-make--low (rest low) quan price))]))


        
  
  
