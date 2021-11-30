

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part1v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))







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











 


 















(check-expect (find-widget-name-longer-than Jewelry 0) 
              (list Jewelry 
                    Rings
                    Necklace
                    Chain
                    Pendant
                    Bracelet
                    Beads
                    Glass))

(check-expect (find-widget-name-longer-than Jewelry 5) 
              (list Jewelry
                    Necklace
                    Pendant
                    Bracelet))

(check-expect (find-widget-name-longer-than Cord 2)
              (append (list Cord) 
                      (find-widget-name-longer-than Wire 2)))

(check-expect (find-widget-name-longer-than Wire 4) empty) 
(check-expect (find-widget-name-longer-than Wire 3) (list Wire)) 
(check-expect (find-widget-name-longer-than Wire 10) empty) 

(check-expect (find-widget-name-longer-than-list (list Cord) 5) 
              empty)

(check-expect (find-widget-name-longer-than-list (list Cord) 4) 
              empty)

(check-expect (find-widget-name-longer-than-list empty 4) 
              empty)
(check-expect (find-widget-name-longer-than-list (list Jewelry) 7) 
              (list Jewelry Necklace Bracelet))







(define (find-widget-name-longer-than widget length)
  (if (> (string-length (widget-name widget)) length)
      (cons widget
            (find-widget-name-longer-than-list (widget-parts widget)
                                               length))
      (find-widget-name-longer-than-list (widget-parts widget) length)))



(define (find-widget-name-longer-than-list low length)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) length)
                 (find-widget-name-longer-than-list (rest low) length))]))













(check-expect (find-widget-quantity-over Wire 0) (list Wire)) 
(check-expect (find-widget-quantity-over Wire 100) empty) 
(check-expect (find-widget-quantity-over Telephone 8)
              (list Receiver 
                    Numbers))

(check-expect (find-widget-quantity-over Telephone 7) 
              (append (list Receiver)
                      (find-widget-quantity-over Buttons 7)))

(check-expect (find-widget-quantity-over-list empty 8) empty) 
              

(check-expect (find-widget-quantity-over-list (widget-parts Jewelry) 2) 
              (list Rings Necklace Chain Pendant Bracelet Beads Glass))

(check-expect (find-widget-quantity-over-list (widget-parts Telephone) 8) 
              (list Receiver Numbers))

(check-expect (find-widget-quantity-over-list (list Bracelet Cord) 2) 
              (list Bracelet Beads Glass Cord Wire))
                                              






(define (find-widget-quantity-over widget num)
  (if(> (widget-quantity widget) num)
     (cons widget (find-widget-quantity-over-list (widget-parts widget) num))
     (find-widget-quantity-over-list (widget-parts widget) num)))



(define (find-widget-quantity-over-list low num)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over (first low) num)
                 (find-widget-quantity-over-list (rest low) num))]))












(check-expect (find-widgets-cheaper-than Wire 100) (list Wire)) 
(check-expect (find-widgets-cheaper-than Wire 0) empty) 
(check-expect (find-widgets-cheaper-than Telephone 15) 
              (list Receiver
                    Buttons
                    Numbers
                    Cord
                    Wire))
(check-expect (find-widgets-cheaper-than Telephone 7)
              (list Buttons 
                    Numbers
                    Cord
                    Wire))
(check-expect (find-widgets-cheaper-than Necklace 0) empty) 
(check-expect (find-widgets-cheaper-than Necklace 100)
              (list Necklace 
                    Chain
                    Pendant))

(check-expect (find-widgets-cheaper-than-list empty 100) 
              empty)

(check-expect (find-widgets-cheaper-than-list (list Cord) 100) 
              (list Cord Wire))

(check-expect (find-widgets-cheaper-than-list (list Telephone) 0) 
              empty)

(check-expect (find-widgets-cheaper-than-list empty 100) 
              empty)






(define (find-widgets-cheaper-than widget num)
  (if(< (widget-price widget) num)
     (cons widget (find-widgets-cheaper-than-list (widget-parts widget) num))
     (find-widgets-cheaper-than-list (widget-parts widget) num)))



(define (find-widgets-cheaper-than-list low num)
  (cond [(empty? low) empty]
        [else
         (append (find-widgets-cheaper-than (first low) num)
                 (find-widgets-cheaper-than-list (rest low) num))]))















(check-expect (find-widget-hard-make Necklace 0 100) empty) 

(check-expect (find-widget-hard-make Necklace 100 0) 
              (list Necklace
                    Chain
                    Pendant))

(check-expect (find-widget-hard-make Necklace 100 100) 
              (list Necklace
                    Chain
                    Pendant))


(check-expect (find-widget-hard-make Necklace 0 0) 
              (list Necklace
                    Chain
                    Pendant))


(check-expect (find-widget-hard-make Cord 3 100) empty) 

(check-expect (find-widget-hard-make Cord 7 100) (list Wire)) 


(check-expect (find-widget-hard-make Cord 0 5) empty) 

(check-expect (find-widget-hard-make-list empty 0 0) empty) 

(check-expect (find-widget-hard-make-list (widget-parts Necklace) 0 0) 
              (list Chain
                    Pendant))

(check-expect (find-widget-hard-make-list (list Necklace Cord) 100 100) 
              (list Necklace
                    Chain
                    Pendant
                    Cord
                    Wire))







(define (find-widget-hard-make widget amount cost)
  (if (hard? widget amount cost)
      (cons widget (find-widget-hard-make-list (widget-parts widget)
                                               amount
                                               cost))
      (find-widget-hard-make-list (widget-parts widget) amount cost)))



(define (find-widget-hard-make-list low amount cost)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-hard-make (first low) amount cost)
                 (find-widget-hard-make-list (rest low) amount cost))]))











(check-expect (hard? Necklace 0 100) false)

(check-expect (hard? Necklace 100 0) true) 

(check-expect (hard? Necklace 100 100) true) 

(check-expect (hard? Necklace 0 0) true) 


(define (hard? widget amount cost)
  (or (< (widget-quantity widget) amount)
      (> (widget-price widget) cost)))


