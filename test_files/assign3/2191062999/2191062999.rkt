

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Assignment3Part1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

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






 
    
 






(check-expect (find-widget-name-longer-than Rings 5) empty) 
(check-expect (find-widget-name-longer-than Chain 0) (list Chain)) 
(check-expect (find-widget-name-longer-than Buttons 4) (list Buttons Numbers)) 
(check-expect (find-widget-name-longer-than Telephone 5) (list Telephone Receiver Buttons Numbers)) 

(check-expect (find-low-name-longer-than empty 4) empty) 
(check-expect (find-low-name-longer-than (list Glass Beads Bracelet) 4) (list Glass Beads Glass Bracelet Beads Glass)) 
(check-expect (find-low-name-longer-than (list Receiver Buttons Cord) 42) empty) 
(check-expect (find-low-name-longer-than (list Receiver Wire Glass) 6) (list Receiver)) 
 

(define (find-widget-name-longer-than widget length)
  (if (> (string-length (widget-name widget)) length)
      (cons widget (find-low-name-longer-than (widget-parts widget) length))
      (find-low-name-longer-than (widget-parts widget) length)
      ))

(define (find-low-name-longer-than low length)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) length) (find-low-name-longer-than (rest low) length))
         ]))

  







(check-expect (find-widget-quantity-over Rings 25) empty)

(check-expect (find-widget-quantity-over Chain 0) (list Chain))

(check-expect (find-widget-quantity-over Buttons 4) (list Buttons Numbers)) 
(check-expect (find-widget-quantity-over Telephone 8) (list Receiver Numbers)) 

(check-expect (find-low-quantity-over empty 4) empty) 
(check-expect (find-low-quantity-over (list Glass Beads Bracelet) 0)
              (list Glass Beads Glass Bracelet Beads Glass)) 
(check-expect (find-low-quantity-over (list Receiver Buttons Cord) 42) empty)

(check-expect (find-low-quantity-over (list Receiver Buttons Cord) 8)
              (list Receiver Numbers)) 
 



(define (find-widget-quantity-over widget quantity)
  (if (> (widget-quantity widget) quantity)
      (cons widget (find-low-quantity-over (widget-parts widget) quantity))
      (find-low-quantity-over (widget-parts widget) quantity)
      ))

(define (find-low-quantity-over low quantity)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over (first low) quantity)
                 (find-low-quantity-over (rest low) quantity))
         ]))










(check-expect (find-widget-cheaper-than Rings 0) empty)

(check-expect (find-widget-cheaper-than Chain 20) (list Chain)) 
(check-expect (find-widget-cheaper-than Buttons 5) empty)

(check-expect (find-widget-cheaper-than Telephone 20)
              (list Telephone Receiver Buttons Numbers Cord Wire)) 

(check-expect (find-low-cheaper-than empty 4) empty) 
(check-expect (find-low-cheaper-than (list Bracelet) 6) (list Bracelet Glass)) 
(check-expect (find-low-cheaper-than (list Receiver Buttons Cord) 0) empty)

(check-expect (find-low-cheaper-than (list Receiver Buttons Cord) 20)
              (list Receiver Buttons Numbers Cord Wire)) 
 



(define (find-widget-cheaper-than widget price)
  (if (< (widget-price widget) price)
      (cons widget (find-low-cheaper-than (widget-parts widget) price))
      (find-low-cheaper-than (widget-parts widget) price)
      ))

(define (find-low-cheaper-than low price)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-cheaper-than (first low) price) (find-low-cheaper-than (rest low) price))
         ]))







(check-expect (find-widget-hard-make Rings 15 11) empty)

(check-expect (find-widget-hard-make Chain 6 0) (list Chain)) 
(check-expect (find-widget-hard-make Telephone 3 3)
              (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widget-hard-make Bracelet 6 6) (list Bracelet Beads)) 

(check-expect (find-low-hard-make empty 4 7) empty) 
(check-expect (find-low-hard-make (list Receiver Buttons Cord) 3 3)
              (list Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-low-hard-make (list Necklace) 0 2) (list Necklace)) 
(check-expect (find-low-hard-make (list Necklace) 0 4) empty) 
 



(define (find-widget-hard-make widget quantity price)
  (if (hard-make? widget quantity price)
      (cons widget (find-low-hard-make (widget-parts widget) quantity price))
      (find-low-hard-make (widget-parts widget) quantity price)
      ))

(define (find-low-hard-make low quantity price)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-hard-make (first low) quantity price)
                 (find-low-hard-make (rest low) quantity price))
         ]))




(define (hard-make? widget quantity price)
  (or (> (widget-price widget) price)
      (< (widget-quantity widget) quantity)
      ))

(check-expect (hard-make? Rings 15 11) false) 
(check-expect (hard-make? Necklace 0 2) true) 
(check-expect (hard-make? Necklace 0 4) false) 
(check-expect (hard-make? Receiver 9 6) true) 


