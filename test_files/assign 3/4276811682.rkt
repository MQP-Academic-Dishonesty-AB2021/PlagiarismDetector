

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(define-struct widget(name quantity time price parts))







(define YUMMY (make-widget "" 0 0 0 empty))


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

   





 











(check-expect (find-widget-name-longer-than--element YUMMY 0)
              empty) 
(check-expect (find-widget-name-longer-than--element Wire 2)
              (list Wire))
(check-expect (find-widget-name-longer-than--element Jewelry 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect (find-widget-name-longer-than--element Jewelry 7)
              (list Jewelry Necklace Bracelet))




 


 

(define (find-widget-name-longer-than--element widget length)
  (if (name-length widget length)
      (cons widget
            (find-widget-name-longer-than--list (widget-parts widget) length))
      (find-widget-name-longer-than--list (widget-parts widget) length)))

(define (find-widget-name-longer-than--list low length)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than--element (first low) length)
                 (find-widget-name-longer-than--list (rest low) length))]))




(check-expect (name-length Wire 4) false)
(check-expect (name-length Wire 3) true)
(check-expect (name-length Wire 0) true)
(check-expect (name-length YUMMY 0) false)
(check-expect (name-length YUMMY -1) true)



(define (name-length widget length)
  (> (string-length (widget-name widget)) length)) 







(check-expect (find-widget-quantity-over--element YUMMY 0)
              empty) 
(check-expect (find-widget-quantity-over--element Wire 2)
              (list Wire))
(check-expect (find-widget-quantity-over--element Jewelry 5)
              (list Rings Necklace Chain Beads Glass))




 


 

(define (find-widget-quantity-over--element widget val)
  (if (> (widget-quantity widget) val)
      (cons widget
            (find-widget-quantity-over--list (widget-parts widget) val))
      (find-widget-quantity-over--list (widget-parts widget) val)))

(define (find-widget-quantity-over--list low val)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over--element (first low) val)
                 (find-widget-quantity-over--list (rest low) val))]))










(check-expect (find-widgets-cheaper-than--element YUMMY 0)
              empty) 

(check-expect (find-widgets-cheaper-than--element Jewelry 0)
              empty)

(check-expect (find-widgets-cheaper-than--element Wire 2)
              empty)

(check-expect (find-widgets-cheaper-than--element Jewelry 5)
              (list Necklace Chain Pendant Glass))

(check-expect (find-widgets-cheaper-than--element Jewelry 4294967295) 
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

(check-expect (find-widgets-cheaper-than--element Telephone 6)
              (list Buttons Numbers Cord Wire))



 


 

(define (find-widgets-cheaper-than--element widget price)
  (if (< (widget-price widget) price)
      (cons widget
            (find-widgets-cheaper-than--list (widget-parts widget) price))
      (find-widgets-cheaper-than--list (widget-parts widget) price)))

(define (find-widgets-cheaper-than--list low price)
  (cond [(empty? low) empty]
        [else
         (append (find-widgets-cheaper-than--element (first low) price)
                 (find-widgets-cheaper-than--list (rest low) price))]))











(check-expect (find-widget-hard-make--element YUMMY 0 0)
              empty) 

(check-expect (find-widget-hard-make--element Wire 2 0)
              (list Wire)) 

(check-expect (find-widget-hard-make--element Jewelry 4294967295 0) 
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

(check-expect (find-widget-hard-make--element Jewelry 5 5) 
              (list Jewelry Rings Pendant Beads))


(check-expect (hard? Wire 5 5) true)
(check-expect (hard? Wire 4294967295 0) true)
(check-expect (hard? Wire 0 4294967295) false)




 


 

(define (find-widget-hard-make--element widget stock cost)
  (if (hard? widget stock cost)
      (cons widget
            (find-widget-hard-make--list (widget-parts widget) stock cost))
      (find-widget-hard-make--list (widget-parts widget) stock cost)))

(define (find-widget-hard-make--list low stock cost)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-hard-make--element (first low) stock cost)
                 (find-widget-hard-make--list (rest low) stock cost))]))






(define (hard? widget stock cost)
  (or (< (widget-quantity widget) stock) (> (widget-price widget) cost)))