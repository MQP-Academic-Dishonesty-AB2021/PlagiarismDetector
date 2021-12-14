

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment 3 Part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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


 

 








(check-expect (find-widget-name-longer-than Wire 3) (list Wire))

(check-expect (find-widget-name-longer-than Chain 5) empty)

(check-expect (find-widget-name-longer-than Beads 3) (list Beads Glass))

(check-expect (find-widget-name-longer-than Necklace 6) (list Necklace Pendant))

(check-expect (find-widget-name-longer-than--low (widget-parts Wire) 0) empty)

(check-expect (find-widget-name-longer-than--low (widget-parts Beads) 0)
              (list Glass))

(check-expect (find-widget-name-longer-than--low (widget-parts Necklace) 6)
              (list Pendant))

(define (find-widget-name-longer-than awidget length)
  (if (> (string-length (widget-name awidget)) length)
      (cons awidget
            (find-widget-name-longer-than--low (widget-parts awidget) length))
      (find-widget-name-longer-than--low (widget-parts awidget) length))       
  )


(define (find-widget-name-longer-than--low low length)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-name-longer-than (first low) length)
             (find-widget-name-longer-than--low (rest low) length))]
    )
  )








(check-expect (find-widget-quantity-over Wire 5) empty)

(check-expect (find-widget-quantity-over Chain 5) (list Chain))

(check-expect (find-widget-quantity-over Glass 6) empty)

(check-expect (find-widget-quantity-over Buttons 7) (list Buttons Numbers))

(check-expect (find-widget-quantity-over Buttons 8) (list Numbers)) 


(check-expect (find-widget-quantity-over--low empty 0) empty)

(check-expect (find-widget-quantity-over--low (widget-parts Wire) 1) empty)

(check-expect (find-widget-quantity-over--low (widget-parts Buttons) 8)
              (list Numbers))

(check-expect (find-widget-quantity-over--low (widget-parts Necklace) 6)
              (list Chain))

(define (find-widget-quantity-over awidget min-amount)
  (if (> (widget-quantity awidget) min-amount)
      (cons awidget
            (find-widget-quantity-over--low (widget-parts awidget) min-amount))
      (find-widget-quantity-over--low (widget-parts awidget) min-amount)
      )
  )

(define (find-widget-quantity-over--low low min-amount)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-quantity-over (first low) min-amount)
             (find-widget-quantity-over--low (rest low) min-amount))]
    )
  )







(check-expect (find-widgets-cheaper-than Wire 5) empty)

(check-expect (find-widgets-cheaper-than Beads 6) (list Glass))

(check-expect (find-widgets-cheaper-than Jewelry 3) (list Chain Pendant))


(check-expect (find-widgets-cheaper-than--low empty 0) empty)

(check-expect (find-widgets-cheaper-than--low (widget-parts Wire) 0) empty)

(check-expect (find-widgets-cheaper-than--low (widget-parts Beads) 5)
              (list Glass))

(check-expect (find-widgets-cheaper-than--low (widget-parts Telephone) 7)
              (list Buttons Numbers Cord Wire))
 


(define (find-widgets-cheaper-than awidget price)
  (if (< (widget-price awidget) price)
      (cons awidget (find-widgets-cheaper-than--low
                     (widget-parts awidget) price))
      (find-widgets-cheaper-than--low (widget-parts awidget) price)
      )
  )

(define (find-widgets-cheaper-than--low low price)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widgets-cheaper-than (first low) price)
             (find-widgets-cheaper-than--low (rest low) price))]
    )
  )







(check-expect (find-widget-hard-make Wire 3 5) empty)

(check-expect (find-widget-hard-make Beads 10 30) (list Glass))

(check-expect (find-widget-hard-make Buttons 10 6) (list Buttons Numbers))


(check-expect (find-widget-hard-make--low empty 0 0) empty)
 
(check-expect (find-widget-hard-make--low (widget-parts Wire) 0 0) empty)
 
(check-expect (find-widget-hard-make--low (widget-parts Buttons) 10 10)
              (list Numbers))


(define (find-widget-hard-make awidget amount price)
  (if (or (< (widget-quantity awidget) amount)
          (> (widget-price awidget) price))
      (cons awidget (find-widget-hard-make--low (widget-parts awidget)
                                                amount price))
      (find-widget-hard-make--low (widget-parts awidget) amount price)
      )
  )

(define (find-widget-hard-make--low low amount price)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-hard-make (first low) amount price)
             (find-widget-hard-make--low (rest low) amount price))]
    )
  )

