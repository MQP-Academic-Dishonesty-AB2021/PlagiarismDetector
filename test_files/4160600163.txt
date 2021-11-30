

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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


(define (fn-for-widget widg)
  (... (widget-name widg)
       (widget-quanity widg)
       (widget-time widg)
       (widget-price widg)
       (widget-parts widg)))


 










(check-expect (find-widget-name-longer-than Jewelry 6)
              (list Jewelry Necklace Pendant Bracelet))        
(check-expect (find-widget-name-longer-than Wire 5) empty)     
(check-expect (find-widget-name-longer-than Jewelry 15) empty) 


(define (find-widget-name-longer-than widg len)
  (if (> (string-length (widget-name widg)) len)
      (cons widg
            (part-length (widget-parts widg) len))
      (part-length (widget-parts widg) len)))        

(define (part-length low len)
  (cond [(empty? low) empty]
        [else
         (append
          (find-widget-name-longer-than (first low) len)
          (part-length (rest low) len))]))











              



(check-expect (find-widget-quantity-over Jewelry 5)
              (list Rings Necklace Chain Beads Glass))                 
(check-expect (find-widget-quantity-over Jewelry 10)
              (list Rings Beads))                                      
(check-expect (find-widget-quantity-over Numbers 10) empty)            
(check-expect (find-widget-quantity-over Cord 8) empty)                
(check-expect (find-widget-quantity-over Telephone 9) (list Receiver)) 

(define (find-widget-quantity-over widg overstock)
  (if (> (widget-quantity widg) overstock)
      (cons widg
            (stock-list (widget-parts widg) overstock)) 
      (stock-list (widget-parts widg) overstock)))        

(define (stock-list low overstock)
  (cond [(empty? low) empty]
        [else
         (append
          (find-widget-quantity-over (first low) overstock)
          (stock-list (rest low) overstock))]))









(check-expect(find-widget-cheaper-than Jewelry 5)
             (list Necklace Chain Pendant Glass))                      
(check-expect(find-widget-cheaper-than Jewelry 30)
             (list Rings Necklace Chain Pendant Bracelet Beads Glass)) 
(check-expect(find-widget-cheaper-than Wire 1) empty)                  

(define (find-widget-cheaper-than widg cost-thresh)
  (if (< (widget-price widg) cost-thresh)
      (cons widg
            (cost-list (widget-parts widg) cost-thresh)) 
      (cost-list (widget-parts widg) cost-thresh)))        

(define (cost-list low cost-thresh)
  (cond [(empty? low) empty]
        [else
         (append
          (find-widget-cheaper-than (first low) cost-thresh)
          (cost-list (rest low) cost-thresh))]))

                    








(check-expect(find-widget-hard-make Jewelry 7 10)
             (list Jewelry Rings Pendant Bracelet Glass)) 
(check-expect(find-widget-hard-make Cord 2 6) empty)      
(check-expect(find-widget-hard-make Telephone 3 6)
             (list Telephone Receiver))                   


(define (find-widget-hard-make widg stock cost)
  (if (or (> (widget-price widg) cost)
          (< (widget-quantity widg) stock))
      (cons widg
            (difficult-list (widget-parts widg) stock cost)) 
      (difficult-list (widget-parts widg) stock cost)))        

(define (difficult-list low stock cost)
  (cond [(empty? low) empty]
        [else
         (append
          (find-widget-hard-make (first low) stock cost)
          (difficult-list (rest low) stock cost))]))





