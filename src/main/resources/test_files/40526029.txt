

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define-struct widget(name quantity time price parts))















(define (fn-for-widget widget-0)
  (local [
          (define (fn-for-widget widget)
            (... 
             (widget-name widget) 
             (widget-quantity widget) 
             (widget-time widget) 
             (widget-price widget) 
             (fn-for-low(widget-parts widget)))) 

          (define (fn-for-low low)
            (cond [(empty? low) ...]
                  [else 
                   (...
                    (fn-for-widget (firt low))    
                    (fn-for-low(rest low)))])) ]  
    (fn-for-widget widget-0) ))



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




(define (widgetFold widget-0 whatCheck?)
  (local [
          (define (fn-for-widget widget)
            (if (whatCheck? widget)
                (cons widget
                      (fn-for-low(widget-parts widget)))
                (fn-for-low(widget-parts widget))))

          (define (fn-for-low low)
            (cond [(empty? low) empty]
                  [else 
                   (append
                    (fn-for-widget (first low))    
                    (fn-for-low(rest low)))])) ]  
    (fn-for-widget widget-0) ))














(check-expect (find-widget-name-longer-than Jewelry 5)
              (list Jewelry Necklace Pendant Bracelet)) 
(check-expect (find-widget-name-longer-than Bracelet 6)
              (list Bracelet)) 
(check-expect (find-widget-name-longer-than Wire 5)
              empty) 
(check-expect (find-widget-name-longer-than Jewelry 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass)) 
(check-expect (find-widget-name-longer-than Jewelry 100)
              empty) 

  

 

(define (find-widget-name-longer-than widget nameLength) 
  (widgetFold widget
              (lambda (widget) (> (string-length (widget-name widget)) nameLength)) ))





 

(define (searchWidgetName low nameLength)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) nameLength)
                 (searchWidgetName (rest low) nameLength))]))
              







(check-expect (find-widget-quantity-over Beads 7)
              (list Beads)) 
(check-expect (find-widget-quantity-over Jewelry 7)
              (list Rings Necklace Beads)) 
(check-expect (find-widget-quantity-over Glass 9)
              empty) 
(check-expect (find-widget-quantity-over Jewelry 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass)) 

  

 


(define (find-widget-quantity-over widget quantity) 
  (widgetFold widget (lambda (widget) (> (widget-quantity widget) quantity)) ))







 







(check-expect (find-widgets-cheaper-than Jewelry 14)
              (list Rings Necklace Chain Pendant Bracelet Beads Glass)) 
(check-expect (find-widgets-cheaper-than Bracelet 6)
              (list Bracelet Glass)) 
(check-expect (find-widgets-cheaper-than Necklace 0)
              empty) 
(check-expect (find-widgets-cheaper-than Jewelry 50)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass)) 



 
      
(define (find-widgets-cheaper-than widget price) 
  (widgetFold widget (lambda (widget) (< (widget-price widget) price)) ))
      





 







(check-expect (find-widget-hard-make Jewelry 8 6)
              (list Jewelry Rings Chain Pendant Bracelet Beads Glass)) 
(check-expect (find-widget-hard-make Necklace 5 6)
              (list Pendant)) 
(check-expect (find-widget-hard-make Jewelry 1 50)
              empty) 
(check-expect (find-widget-hard-make Jewelry 50 1)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass)) 

  

 




  

  


(define (find-widget-hard-make widget quantity cost) 
  (widgetFold widget (lambda (widget) (isHardToMake? widget quantity cost) )))






(check-expect (isHardToMake? Chain 0 0)
              true) 
(check-expect (isHardToMake? Chain 0 100)
              false)
(check-expect (isHardToMake? Chain 100 0)
              true) 
(check-expect (isHardToMake? Chain 100 100)
              true) 
(check-expect (isHardToMake? Rings 10 15)
              false)
(check-expect (isHardToMake? Rings 10 15)
              false)

  

(define (isHardToMake? widget quantity cost)
  (or (< (widget-quantity widget) quantity) (> (widget-price widget) cost)))









(check-expect (allComponents Wire)
              (list Wire)) 
(check-expect (allComponents Telephone)
              (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (allComponents Bracelet)
              (list Bracelet Beads Glass)) 

  

(define (allComponents Widget0)
  (local [(define (allComponents Widget)
            (cons Widget
                  (makeList (widget-parts Widget))))

          (define (makeList lox)
            (cond [(empty? lox) empty]
                  [else (append (allComponents (first lox)) (makeList (rest lox)))]   ))]      
    (allComponents Widget0)))
  








  

(define (qsort comparisonFn field)
  (local [(define (qsort-inner widget)
            (local [
                    (define (qsort-inner-inner low)
                      (cond [(empty? low) empty]
                            [else
                             (local [ (define pivot (first low))
                                      (define (lessThan? x) (comparisonFn (field x) (field pivot))) ]
                               (append
                                (qsort-inner-inner (filter lessThan? (rest low)))
                                (list pivot)
                                (qsort-inner-inner (filter (lambda(n) 
                                                             (not (lessThan? n))) (rest low)))
                                                   ))
                           
                             ]))]
              (qsort-inner-inner (allComponents widget))))]
    qsort-inner))


(define sort-strings (qsort string<? widget-name)) 
(define sort-overstocked (qsort > widget-quantity)) 
(define sort-by-price (qsort < widget-price)) 




(check-expect (sort-strings Telephone)
              (list Buttons Cord Numbers Receiver Telephone Wire)) 
(check-expect (sort-overstocked Necklace)
              (list Necklace Chain Pendant)) 
(check-expect (sort-by-price Bracelet)
              (list Glass Bracelet Beads)) 




