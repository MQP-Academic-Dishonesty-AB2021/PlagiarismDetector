

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



 
  







(define (find-widget-name-longer-than wid cutoff)
  (if (> (string-length (widget-name wid)) cutoff) 
  (cons wid (find-widget-name-longer-than-list (widget-parts wid) cutoff))
  (find-widget-name-longer-than-list (widget-parts wid) cutoff)))


(define (find-widget-name-longer-than-list low cutoff)
(if (empty? low) empty
    (append (find-widget-name-longer-than (first low) cutoff)
           (find-widget-name-longer-than-list (rest low) cutoff))))

(check-expect (find-widget-name-longer-than Wire 0) (list Wire))
(check-expect (find-widget-name-longer-than Wire 10) empty)
(check-expect (find-widget-name-longer-than Jewelry 6) (list Jewelry Necklace Pendant Bracelet))







(define (find-widget-quantity-over wid cutoff)
  (if (> (widget-quantity wid) cutoff) 
  (cons wid (find-widget-quantity-over-list (widget-parts wid) cutoff))
  (find-widget-quantity-over-list (widget-parts wid) cutoff)))


(define (find-widget-quantity-over-list low cutoff)
(if (empty? low) empty
    (append (find-widget-quantity-over (first low) cutoff)
           (find-widget-quantity-over-list (rest low) cutoff))))

(check-expect (find-widget-quantity-over Wire 4) empty)
(check-expect (find-widget-quantity-over Wire 2) (list Wire))
(check-expect (find-widget-quantity-over Jewelry 6) (list Rings Necklace Chain Beads))







(define (find-widgets-cheaper-than wid cutoff)
  (if (< (widget-price wid) cutoff) 
  (cons wid (find-widgets-cheaper-than-list (widget-parts wid) cutoff))
  (find-widgets-cheaper-than-list (widget-parts wid) cutoff)))


(define (find-widgets-cheaper-than-list low cutoff)
(if (empty? low) empty
    (append (find-widgets-cheaper-than (first low) cutoff)
           (find-widgets-cheaper-than-list (rest low) cutoff))))

(check-expect (find-widgets-cheaper-than Wire 6) (list Wire))
(check-expect (find-widgets-cheaper-than Wire 4) empty)
(check-expect (find-widgets-cheaper-than Jewelry 10) (list Necklace Chain Pendant Bracelet Beads Glass))






(define ( find-widget-hard-make wid cutoff)
  (if (or (< (widget-quantity wid) cutoff) (> (widget-price wid) cutoff)) 
  (cons wid (find-widget-hard-make-list (widget-parts wid) cutoff))
  (find-widget-hard-make-list (widget-parts wid) cutoff)))


(define (find-widget-hard-make-list low cutoff)
(if (empty? low) empty
    (append (find-widget-hard-make (first low) cutoff)
           (find-widget-hard-make-list (rest low) cutoff))))

(check-expect (find-widget-hard-make Cord 5) (list Wire))
(check-expect (find-widget-hard-make (make-widget "anonymas" 100 5 0 empty) 4) empty)
(check-expect (find-widget-hard-make Jewelry 10) (list Jewelry Rings Chain Pendant Bracelet Glass))