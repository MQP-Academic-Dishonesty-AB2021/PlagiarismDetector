

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


 

 







(check-expect (find-widget-name-longer-than Glass 7)
              empty)
(check-expect (find-widget-name-longer-than Beads 2)
              (list Beads Glass))
(check-expect (find-widget-name-longer-than Jewelry 5)
              (list Jewelry Necklace Pendant Bracelet ))



(define (find-widget-name-longer-than aWidget lengt)
  (if (> (string-length (widget-name aWidget)) lengt)
      
      (cons aWidget (longer-than-low (widget-parts aWidget) lengt))
      
      (longer-than-low (widget-parts aWidget) lengt)
      )
  )





(check-expect (longer-than-low empty 2)
              empty)
(check-expect (longer-than-low (list Beads ) 3)
              (list Beads Glass)) 
(check-expect (longer-than-low (list Chain Pendant ) 5)
              (list Pendant)) 



(define (longer-than-low low lengt)
  (cond
    [(empty? low) empty]
    [else
     (append
      (find-widget-name-longer-than (first low) lengt)
      (longer-than-low (rest low) lengt))]
    )
  )








(check-expect (find-widget-quantity-over Numbers 10)
              empty)
(check-expect (find-widget-quantity-over Cord 2)
              (list Cord Wire))
(check-expect (find-widget-quantity-over Jewelry 1)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))


(define (find-widget-quantity-over w n)
  (if (> (widget-quantity w) n)
      (cons w (find-quantity-low (widget-parts w) n))
      (find-quantity-low (widget-parts w) n)))





(check-expect (find-quantity-low (widget-parts Numbers) 10) empty)
(check-expect (find-quantity-low (widget-parts Cord) 2)
              (list Wire))
(check-expect (find-quantity-low (widget-parts Jewelry) 1)
              (list Rings Necklace Chain Pendant Bracelet Beads Glass))

               
(define (find-quantity-low t length) 
  (cond
    [(empty? t) empty]
    [else
     (append
      (find-widget-quantity-over (first t) length)
      (find-quantity-low (rest t) length))]))






(check-expect (find-widgets-cheaper-than Glass 3)
              empty)
(check-expect (find-widgets-cheaper-than Beads 12)
              (list Beads Glass))
(check-expect (find-widgets-cheaper-than Jewelry 20)
              (list  Rings Necklace Chain Pendant
                     Bracelet Beads Glass ))


(define (find-widgets-cheaper-than aWidget maxPrice)
   (if (< (widget-price aWidget) maxPrice)
      
      (cons aWidget (cheaper-than-low (widget-parts aWidget) maxPrice))
      
      (cheaper-than-low (widget-parts aWidget) maxPrice)
      )
  )






(check-expect (cheaper-than-low empty 2)
              empty)
(check-expect (cheaper-than-low (list Beads ) 10)
              (list Beads Glass)) 
(check-expect (cheaper-than-low (list Chain Pendant ) 2)
              (list Chain Pendant)) 



(define (cheaper-than-low low maxPrice)
  (cond
    [(empty? low) empty]
    [else
     (append
      (find-widgets-cheaper-than (first low) maxPrice)
      (cheaper-than-low (rest low) maxPrice))]
    )
 )







(check-expect (find-widget-hard-make Wire 1 10) empty) 
(check-expect (find-widget-hard-make Cord 1 1) (list Cord Wire)) 
(check-expect (find-widget-hard-make Necklace 1 1) (list Necklace)) 

(define (find-widget-hard-make w stockN costN)
  (if (or (< (widget-quantity w) stockN) (> (widget-price w) costN))
      (cons w (cheaper-or-stock-less-than-low (widget-parts w) stockN costN))
      (cheaper-or-stock-less-than-low (widget-parts w) stockN costN)))






(check-expect (cheaper-or-stock-less-than-low empty 0 0)empty)
(check-expect (cheaper-or-stock-less-than-low (list Cord) 1 1)
              (list Cord Wire))
(check-expect (cheaper-or-stock-less-than-low (list Necklace) 1 1)
              (list Necklace))

(define (cheaper-or-stock-less-than-low low stockN costN)
  (cond
    [(empty? low) empty]
    [else
     (append
      (find-widget-hard-make (first low) stockN costN) 
      (cheaper-or-stock-less-than-low (rest low) stockN costN))]
    )
 )
