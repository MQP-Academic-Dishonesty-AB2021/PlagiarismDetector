

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define-struct widget(name quantity time price parts))














(define (fn-for-widget widget)
  (... 
   (widget-name widget) 
   (widget-quantity widget) 
   (widget-time widget) 
   (widget-price widget) 
   (fn-for-low(widget-parts widget)))) 

(define (fn-for-low low)
  (cond [(empty? low) ...]
        [else 
         (...
          (fn-for-widget (firt low)) 
          (fn-for-low(rest low)))])) 



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
(check-expect (find-widget-name-longer-than Bracelet 6)
              (list Bracelet)) 
(check-expect (find-widget-name-longer-than Wire 5)
              empty) 
(check-expect (find-widget-name-longer-than Jewelry 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass)) 
(check-expect (find-widget-name-longer-than Jewelry 100)
              empty) 

  

(define (find-widget-name-longer-than widget nameLength)
  (if (> (string-length (widget-name widget)) nameLength)
      (cons widget
            (searchWidgetName(widget-parts widget) nameLength))
      (searchWidgetName(widget-parts widget) nameLength)))





(check-expect (searchWidgetName empty 100)
              empty) 
(check-expect (searchWidgetName (list Wire Cord Numbers) 4)
              (list Numbers)) 
(check-expect (searchWidgetName (list Jewelry Telephone) 100)
              empty) 
(check-expect (searchWidgetName (list Jewelry Telephone) 4)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass Telephone Receiver Buttons Numbers)) 

 

(define (searchWidgetName low nameLength)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) nameLength)
                 (searchWidgetName (rest low) nameLength))]))
              






(check-expect (find-widget-quantity-over Beads 7)
              (list Beads)) 
(check-expect (find-widget-quantity-over Jewelry 7)
              (list Rings Necklace Beads)) 
(check-expect (find-widget-quantity-over Glass 9)
              empty) 
(check-expect (find-widget-quantity-over Jewelry 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass)) 

  

(define (find-widget-quantity-over widget quantity)
  (if (> (widget-quantity widget) quantity)
      (cons widget
            (quantitySearchLoW (widget-parts widget) quantity))
      (quantitySearchLoW (widget-parts widget) quantity)))






(check-expect (quantitySearchLoW empty 100)
              empty) 
(check-expect (quantitySearchLoW (list Wire Cord Numbers) 4)
              (list Cord Numbers)) 
(check-expect (quantitySearchLoW (list Jewelry Telephone) 100)
              empty) 
(check-expect (quantitySearchLoW (list Jewelry Telephone) 4)
              (list Rings Necklace Chain Bracelet Beads Glass Telephone Receiver Buttons Numbers Cord)) 



(define (quantitySearchLoW low quantity)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over (first low) quantity)
                 (quantitySearchLoW (rest low) quantity))]))







(check-expect (find-widgets-cheaper-than Jewelry 14)
              (list Rings Necklace Chain Pendant Bracelet Beads Glass)) 
(check-expect (find-widgets-cheaper-than Bracelet 6)
              (list Bracelet Glass)) 
(check-expect (find-widgets-cheaper-than Necklace 0)
              empty) 
(check-expect (find-widgets-cheaper-than Jewelry 50)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass)) 



(define (find-widgets-cheaper-than widget price)
  (if (< (widget-price widget) price)
      (cons widget
            (priceSearch (widget-parts widget) price))
      (priceSearch (widget-parts widget) price)))
      
      



(check-expect (priceSearch empty 100)
              empty) 
(check-expect (priceSearch (list Wire Cord Numbers) 4)
              empty) 
(check-expect (priceSearch (list Jewelry Telephone) 100)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (priceSearch (list Jewelry Telephone) 10)
              (list Necklace Chain Pendant Bracelet Beads Glass Receiver Buttons Numbers Cord Wire)) 



(define (priceSearch low price)
  (cond [(empty? low) empty]
        [else
         (append (find-widgets-cheaper-than (first low) price)
                 (priceSearch (rest low) price))]))







(check-expect (find-widget-hard-make Jewelry 8 6)
              (list Jewelry Rings Chain Pendant Bracelet Beads Glass)) 
(check-expect (find-widget-hard-make Necklace 5 6)
              (list Pendant)) 
(check-expect (find-widget-hard-make Jewelry 1 50)
              empty) 
(check-expect (find-widget-hard-make Jewelry 50 1)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass)) 

  

(define (find-widget-hard-make widget quantity cost)
  (if (isHardToMake? widget quantity cost)
      (cons widget (findListOfHardWidgets (widget-parts widget) quantity cost))
      (findListOfHardWidgets (widget-parts widget) quantity cost) ))




(check-expect (findListOfHardWidgets empty 100 0)
              empty) 
(check-expect (findListOfHardWidgets (list Wire Numbers Receiver) 5 6)
              (list Wire Receiver)) 
(check-expect (findListOfHardWidgets (list Necklace Buttons Cord) 8 10)
              (list Chain Pendant Cord Wire)) 
(check-expect (findListOfHardWidgets (list Necklace Buttons Cord) 20 1)
              (list Necklace Chain Pendant Buttons Numbers Cord Wire)) 

  

(define (findListOfHardWidgets low quantity cost)
  (cond [(empty? low) empty]
        [else 
         (append
          (find-widget-hard-make (first low) quantity cost) 
          (findListOfHardWidgets (rest low) quantity cost))])) 





(check-expect (isHardToMake? Chain 0 0)
              true) 
(check-expect (isHardToMake? Chain 0 100)
              false)
(check-expect (isHardToMake? Chain 100 0)
              true) 
(check-expect (isHardToMake? Chain 100 100)
              true) 
(check-expect (isHardToMake? Rings 10 15)
              alse)
(check-expect (isHardToMake? Rings 10 15)
              false)

  

(define (isHardToMake? widget quantity cost)
  (or (< (widget-quantity widget) quantity) (> (widget-price widget) cost)))

