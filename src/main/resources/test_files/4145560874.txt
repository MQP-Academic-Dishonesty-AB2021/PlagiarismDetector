

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname assignment3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



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
                             
 

 






(define (find-widget-name-longer-than w num)
  (if 
   (> (string-length (widget-name w)) num)
   (cons w (find-widget-name-longer-than--low (widget-parts w) num))
   (find-widget-name-longer-than--low (widget-parts w) num)))

(define (find-widget-name-longer-than--low low num)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) num)
                 (find-widget-name-longer-than--low (rest low) num))]))


(check-expect (find-widget-name-longer-than Wire 3) (list Wire))

(check-expect (find-widget-name-longer-than Wire 4) empty)

(check-expect (find-widget-name-longer-than Cord 3) (list Cord Wire))

(check-expect (find-widget-name-longer-than Cord 4) empty)

(check-expect (find-widget-name-longer-than Necklace 5) (list Necklace Pendant))

(check-expect (find-widget-name-longer-than Bracelet 4) (list Bracelet Beads Glass))






(define (find-widget-quantity-over--widget w num)
  (if (> (widget-quantity w) num)
      (cons w (find-widget-quantity-over--low (widget-parts w) num))
      (find-widget-quantity-over--low (widget-parts w) num)))

(define (find-widget-quantity-over--low low num)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over--widget (first low) num)
                 (find-widget-quantity-over--low (rest low) num))]))
                
(check-expect (find-widget-quantity-over--widget Wire 0) (list Wire))

(check-expect (find-widget-quantity-over--widget Cord 10) empty)

(check-expect (find-widget-quantity-over--widget Buttons 6) (list Buttons Numbers))

(check-expect (find-widget-quantity-over--widget Chain 10) empty)

(check-expect (find-widget-quantity-over--widget Bracelet 20) (list Beads))

(check-expect (find-widget-quantity-over--widget Jewelry 12) (list Rings Beads))

         





(define (find-widgets-cheaper-than w num)
  (if 
   (< (widget-price w) num)
   (cons w (find-widgets-cheaper-than--low (widget-parts w) num))
   (find-widgets-cheaper-than--low (widget-parts w) num)))

(define (find-widgets-cheaper-than--low low num)
  (cond [(empty? low) empty]
        [else
         (append (find-widgets-cheaper-than (first low) num)
                 (find-widgets-cheaper-than--low (rest low) num))]))
         

(check-expect (find-widgets-cheaper-than Wire 6) (list Wire))

(check-expect (find-widgets-cheaper-than Wire 5) empty)

(check-expect (find-widgets-cheaper-than Cord 6) (list Cord Wire))

(check-expect (find-widgets-cheaper-than Cord 5) empty)

(check-expect (find-widgets-cheaper-than Necklace 2) (list Chain Pendant))

(check-expect (find-widgets-cheaper-than Bracelet 6) (list Bracelet Glass))






(define (find-widget-hard-make w ntl num)
  (if (ishard? w ntl num)
      (cons w (find-widget-hard-make--low (widget-parts w) ntl num))
      (find-widget-hard-make--low (widget-parts w) ntl num)))
  
(define (find-widget-hard-make--low low ntl num)
    (cond [(empty? low) empty]
          [else
          (append (find-widget-hard-make (first low) ntl num)
                  (find-widget-hard-make--low (rest low) ntl num))]))

(check-expect (find-widget-hard-make Wire 0 0) empty)

(check-expect (find-widget-hard-make Cord 5 5) empty)

(check-expect (find-widget-hard-make Telephone 10 4)
              (list Telephone Buttons Numbers Cord Wire))

(check-expect (find-widget-hard-make Glass 0 0) empty)

(check-expect (find-widget-hard-make Necklace 30 30) empty)

              






(define (ishard? w ntl num)
  (and (< (widget-quantity w) ntl)
       (> (widget-price w) num)))

(check-expect (ishard? Wire 0 0) false)

(check-expect (ishard? Cord 5 5) false)

(check-expect (ishard? Buttons 10 10) false)

(check-expect (ishard? Telephone 7 10) true)


  
  