

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



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











(check-expect (find-widget-name-longer-than Telephone 0) 
              (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget-name-longer-than Telephone 5) 
              (list Telephone Receiver Buttons Numbers))
(check-expect (find-widget-name-longer-than Jewelry 1000) empty) 
(check-expect (find-widget-name-longer-than Wire 5) empty) 
(check-expect 
 (find-widget-name-longer-than Necklace 6)
 (list Necklace Pendant))


(define (find-widget-name-longer-than widget cutoff)
  (if (> (string-length (widget-name widget)) cutoff)
      (cons widget (find-widgets-name-longer-than--low (widget-parts widget) cutoff))
      (find-widgets-name-longer-than--low (widget-parts widget) cutoff)))








(check-expect (find-widgets-name-longer-than--low empty 5) empty) 
(check-expect (find-widgets-name-longer-than--low 
               (widget-parts Telephone) 100) empty) 
(check-expect (find-widgets-name-longer-than--low 
               (widget-parts Jewelry) 0) 
              (list Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect 
 (find-widgets-name-longer-than--low (widget-parts Cord) 3)
 (list Wire))


(define (find-widgets-name-longer-than--low low cutoff)
  (cond [(empty? low) empty]
        [else (append (find-widget-name-longer-than (first low) cutoff)
                      (find-widgets-name-longer-than--low (rest low) cutoff))]))










(check-expect (find-widget-quantity-over Jewelry 9000) empty) 
(check-expect (find-widget-quantity-over Telephone 2) 
              (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widget-quantity-over Telephone 7)
              (list Receiver Buttons Numbers)) 
(check-expect (find-widget-quantity-over Wire 5) empty) 
(check-expect 
 (find-widget-quantity-over Necklace 6)
 (list Necklace Chain))


(define (find-widget-quantity-over widget cutoff)
  (if (> (widget-quantity widget) cutoff)
      (cons widget (find-widgets-quantity-over--low (widget-parts widget) cutoff))
      (find-widgets-quantity-over--low (widget-parts widget) cutoff)))








(check-expect (find-widgets-quantity-over--low empty 7) empty) 
(check-expect (find-widgets-quantity-over--low (widget-parts Telephone) 7)
              (list Receiver Buttons Numbers)) 
(check-expect (find-widgets-quantity-over--low (list Jewelry) 0) 
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect 
 (find-widgets-quantity-over--low (widget-parts Cord) 1) (list Wire))


(define (find-widgets-quantity-over--low low cutoff)
  (cond [(empty? low) empty]
        [else (append (find-widget-quantity-over (first low) cutoff)
                      (find-widgets-quantity-over--low (rest low) cutoff))]))











(check-expect (find-widgets-cheaper-than Jewelry 0) empty) 
(check-expect (find-widgets-cheaper-than Telephone 1000) 
              (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widgets-cheaper-than Jewelry 7)
              (list Necklace Chain Pendant Bracelet Glass)) 
(check-expect (find-widgets-cheaper-than Wire 5) empty) 
(check-expect 
 (find-widgets-cheaper-than Buttons 6)
 (list Buttons Numbers))


(define (find-widgets-cheaper-than widget cutoff)
  (if (< (widget-price widget) cutoff)
      (cons widget (find-widgets-cheaper-than--low (widget-parts widget) cutoff))
      (find-widgets-cheaper-than--low (widget-parts widget) cutoff)))








(check-expect (find-widgets-cheaper-than--low empty 7) empty) 
(check-expect (find-widgets-cheaper-than--low (widget-parts Telephone) 10)
              (list Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widgets-cheaper-than--low (list Jewelry) 1000) 
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect 
 (find-widgets-cheaper-than--low (widget-parts Cord) 6) (list Wire))


(define (find-widgets-cheaper-than--low low cutoff)
  (cond [(empty? low) empty]
        [else (append (find-widgets-cheaper-than (first low) cutoff)
                      (find-widgets-cheaper-than--low (rest low) cutoff))]))












(check-expect (find-widget-hard-make Jewelry 0 1000) empty) 
(check-expect (find-widget-hard-make Telephone 1000 0)
              (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widget-hard-make Telephone 0 0) 
              (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget-hard-make Jewelry 1000 1000) 
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

(check-expect (find-widget-hard-make Jewelry 7 7) 
              (list Jewelry Rings Pendant Bracelet Glass)) 
(check-expect (find-widget-hard-make Telephone 7 7) 
              (list Telephone Wire)) 
(check-expect (find-widget-hard-make Wire 2 6) 
              empty)


(define (find-widget-hard-make widget q-cutoff p-cutoff)
  (if (or (< (widget-quantity widget) q-cutoff) (> (widget-price widget) p-cutoff))
      (cons widget (find-widgets-hard-make--low (widget-parts widget) q-cutoff p-cutoff))
      (find-widgets-hard-make--low (widget-parts widget) q-cutoff p-cutoff)))









(check-expect (find-widgets-hard-make--low empty 0 1000) empty) 
(check-expect (find-widgets-hard-make--low (widget-parts Telephone) 0 1000) empty) 
(check-expect (find-widgets-hard-make--low (widget-parts Jewelry) 7 7)
              (list Rings Pendant Bracelet Glass)) 
(check-expect (find-widgets-hard-make--low (widget-parts Telephone) 7 7)
              (list Wire)) 
(check-expect (find-widgets-hard-make--low (list Jewelry) 1000 0) 
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect 
 (find-widgets-hard-make--low (widget-parts Cord) 6 3) (list Wire))


(define (find-widgets-hard-make--low low q-cutoff p-cutoff)
  (cond [(empty? low) empty]
        [else (append (find-widget-hard-make (first low) q-cutoff p-cutoff)
                      (find-widgets-hard-make--low (rest low) q-cutoff p-cutoff))]))