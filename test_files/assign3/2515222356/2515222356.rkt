

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |starter part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


    
 




(check-expect (find-widget-name-longer-than Bracelet 9) empty) 
(check-expect (find-widget-name-longer-than Bracelet 3) (list Bracelet Beads Glass)) 
(check-expect (find-widget-name-longer-than Buttons 6) (list Buttons Numbers)) 


(define (find-widget-name-longer-than widge length)
  (if (> (string-length (widget-name widge)) length)
      (cons widge (find-widget-list-longer-than (widget-parts widge) length))
      (find-widget-list-longer-than (widget-parts widge) length)))   

(define (find-widget-list-longer-than low length)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) length)   
         (find-widget-list-longer-than (rest low) length))]))

  





(check-expect (find-widget-quantity-over Wire 3) empty) 
(check-expect (find-widget-quantity-over Beads 7) (list Beads)) 
(check-expect (find-widget-quantity-over Bracelet 4) (list Bracelet Beads Glass)) 

(define (find-widget-quantity-over widge n)
  (if (> (widget-quantity widge) n)
      (cons widge (list-quantity-over (widget-parts widge) n))
      (list-quantity-over (widget-parts widge) n)))   

(define (list-quantity-over low n)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-quantity-over (first low) n)   
         (list-quantity-over (rest low) n))]))





(check-expect (find-widgets-cheaper-than Bracelet 1) empty) 
(check-expect (find-widgets-cheaper-than Bracelet 10) (list Bracelet Beads Glass)) 
(check-expect (find-widgets-cheaper-than Bracelet 5) (list Glass)) 


(define (find-widgets-cheaper-than w pri)
  (if (< (widget-price w) pri)
      (cons w (widget-loop-cheaper-than (widget-parts w) pri))
      (widget-loop-cheaper-than (widget-parts w) pri)))

(define (widget-loop-cheaper-than low price)
  (cond [(empty? low) empty]
        [else
         (append (find-widgets-cheaper-than (first low) price)   
                 (widget-loop-cheaper-than (rest low) price))]))
                 







(check-expect (find-widget-hard-make Chain 4) empty) 
(check-expect (find-widget-hard-make Bracelet 5)(list Beads)) 
(check-expect (find-widget-hard-make Telephone 5) (list Telephone Receiver Wire)) 

(define (find-widget-hard-make w n)
  (if (or (> n (widget-quantity w))(< n (widget-price w)))
      (cons w (widget-loop-hard-make (widget-parts w) n))
      (widget-loop-hard-make (widget-parts w) n)))

(define (widget-loop-hard-make low n)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-hard-make (first low) n)   
                 (widget-loop-hard-make (rest low) n))]))