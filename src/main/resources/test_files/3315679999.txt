

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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
  (widget-compute widg
                  (λ(widg) (> (string-length (widget-name widg)) len))))





(check-expect (find-widget-quantity-over Jewelry 5)
              (list Rings Necklace Chain Beads Glass))                 
(check-expect (find-widget-quantity-over Jewelry 10)
              (list Rings Beads))                                      
(check-expect (find-widget-quantity-over Numbers 10) empty)            
(check-expect (find-widget-quantity-over Cord 8) empty)                
(check-expect (find-widget-quantity-over Telephone 9) (list Receiver)) 


(define (find-widget-quantity-over widg overstock)
  (widget-compute widg
                  (λ(widg) (> (widget-quantity widg) overstock))))










(check-expect(find-widget-cheaper-than Jewelry 5)
             (list Necklace Chain Pendant Glass))                      
(check-expect(find-widget-cheaper-than Jewelry 30)
             (list Rings Necklace Chain Pendant Bracelet Beads Glass)) 
(check-expect(find-widget-cheaper-than Wire 1) empty)                  

    

(define (find-widget-cheaper-than widg cost-thresh)
  (widget-compute widg
                  (λ(widg) (< (widget-price widg) cost-thresh))))










(check-expect(find-widget-hard-make Jewelry 7 10)
             (list Jewelry Rings Pendant Bracelet Glass)) 
(check-expect(find-widget-hard-make Cord 2 6) empty)      
(check-expect(find-widget-hard-make Telephone 3 6)
             (list Telephone Receiver))                   



(define (find-widget-hard-make widg stock cost)
  (widget-compute widg
                  (λ(widg) (or(> (widget-price widg) cost)
                              (< (widget-quantity widg) stock)))))





 






(define (widget-compute widg func)
  (local
    ([define (compute-asses widg func)
       (if (func widg)
           (cons widg
                 (compute-list (widget-parts widg))) 
           (compute-list (widget-parts widg)))]        

     [define (compute-list low)
       (cond [(empty? low) empty]
             [else
              (append
               (compute-asses (first low) func)
               (compute-list (rest low)))])])
    (compute-asses widg func)))





 
















(check-expect (qsort > (list Jewelry Telephone) widget-price)
              (list Jewelry Telephone))
(check-expect (qsort < (list Necklace Wire Telephone) widget-quantity)
              (list Wire Telephone Necklace))

(define (qsort fn? low fn-for-widget)
  (cond
    [(empty? low) empty]
    [else 
     (local
       [(define PREV (first low))
        (define (smaller? widg)
          (fn? (fn-for-widget widg) (fn-for-widget PREV)))] 
       (append
        (qsort fn? (filter smaller? (rest low)) fn-for-widget)
        (list PREV)
        (qsort fn?
               (filter (λ(widg) 
                         (not (smaller? widg)))
                       (rest low)) fn-for-widget)))]))





(check-expect(full-low Necklace)
             (list Necklace Chain Pendant))
(check-expect(full-low Jewelry)
             (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect(full-low Wire)
             (list Wire))

(define (full-low widg)
  (local
    ([define (make-low widg)
           (cons widg
                 (add-to-low (widget-parts widg)))]        

     [define (add-to-low low)
       (cond [(empty? low) empty]
             [else
              (append
               (make-low (first low))
               (add-to-low (rest low)))])])
    (make-low widg)))





(check-expect (sort-strings Telephone)
              (list Buttons Cord Numbers Receiver Telephone Wire))
(check-expect (sort-strings Jewelry)
              (list Beads Bracelet Chain Glass Jewelry Necklace Pendant Rings))
(check-expect (sort-strings Wire)
              (list Wire))

(define (sort-strings widg)
  (qsort string<? (full-low widg) widget-name))




(check-expect (sort-overstocked Necklace)
              (list Necklace Chain Pendant))
(check-expect (sort-overstocked Telephone)
              (list Receiver Numbers Buttons Cord Telephone Wire))
(check-expect (sort-overstocked Wire)
              (list Wire))

(define (sort-overstocked widg)
  (qsort > (full-low widg) widget-quantity))
         
      


    


