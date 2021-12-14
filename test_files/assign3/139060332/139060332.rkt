

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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


 

 






(define (find-widget-name-longer-than widget limit)
  (if (> (string-length (widget-name widget)) limit)
      (cons
       widget
       (find-widget-name-longer-than--low (widget-parts widget) limit))
      (find-widget-name-longer-than--low  (widget-parts widget) limit)))


(define (find-widget-name-longer-than--low low limit)
  (cond
    [(empty? low) empty]
    [else
     (append
      (find-widget-name-longer-than (first low) limit)
      (find-widget-name-longer-than--low  (rest low) limit))]))



(check-expect (find-widget-name-longer-than Telephone 7) 
              (list Telephone Receiver))


(check-expect (find-widget-name-longer-than Telephone 9) 
              empty)


(check-expect (find-widget-name-longer-than Wire 2) 
              (list Wire))


(check-expect (find-widget-name-longer-than Numbers 10) 
              empty)





(define (find-widget-quantity-over widget limit)
  (if (> (widget-quantity widget) limit)
      (cons
       widget
       (find-widget-quantity-over--low (widget-parts widget) limit))
      (find-widget-quantity-over--low  (widget-parts widget) limit)))


(define (find-widget-quantity-over--low low limit)
  (cond
    [(empty? low) empty]
    [else
     (append
      (find-widget-quantity-over (first low) limit)
      (find-widget-quantity-over--low  (rest low) limit))]))



(check-expect (find-widget-quantity-over Telephone 7) 
              (list Receiver Buttons Numbers))


(check-expect (find-widget-quantity-over Telephone 15) 
              empty)


(check-expect (find-widget-quantity-over Wire 2) 
              (list Wire))


(check-expect (find-widget-quantity-over Numbers 10) 
              empty)





(define (find-widgets-cheaper-than widget limit)
  (if (< (widget-price widget) limit)
      (cons
       widget
       (find-widgets-cheaper-than--low (widget-parts widget) limit))
      (find-widgets-cheaper-than--low (widget-parts widget) limit)))


(define (find-widgets-cheaper-than--low low limit)
  (cond
    [(empty? low) empty]
    [else
     (append
      (find-widgets-cheaper-than (first low) limit)
      (find-widgets-cheaper-than--low (rest low) limit))]))



(check-expect (find-widgets-cheaper-than Telephone 6) 
              (list Buttons Numbers Cord Wire))


(check-expect (find-widgets-cheaper-than Telephone 3) 
              empty)


(check-expect (find-widgets-cheaper-than Wire 6) 
              (list Wire))


(check-expect (find-widgets-cheaper-than Numbers 2) 
              empty)





(define (find-widget-hard-make widget stock cost)
  (if (or (< (widget-quantity widget) stock)
          (> (widget-price widget) cost))
      (cons
       widget
       (find-widget-hard-make--low (widget-parts widget) stock cost))
      (find-widget-hard-make--low (widget-parts widget) stock cost)))


(define (find-widget-hard-make--low low stock cost)
  (cond
    [(empty? low) empty]
    [else
     (append
      (find-widget-hard-make (first low) stock cost)
      (find-widget-hard-make--low (rest low) stock cost))]))




(check-expect (find-widget-hard-make Telephone 6 20)
              (list Telephone Wire))



(check-expect (find-widget-hard-make Telephone 2 20) 
              empty)



(check-expect (find-widget-hard-make Wire 2 2)
              (list Wire))


(check-expect (find-widget-hard-make Numbers 7 10)
              empty)