

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity time price parts))








 

(define-struct operation (fn? parameter element))






  


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





(define (find-widget widget operations)
  (local [(define (find-widget widget)
            (if (andmap (λ (fn) ((operation-fn? fn)
                                 (local [(define (element-value element)
                                           (cond [(string=? "name" element)
                                                  (string-length (widget-name widget))]
                                                 [(string=? "quantity" element)
                                                  (widget-quantity widget)]
                                                 [(string=? "time" element)
                                                  (widget-time widget)]
                                                 [(string=? "price" element)
                                                  (widget-price widget)]
                                                 [(string=? "parts" element)
                                                  (widget-parts widget)]))]
                                   (element-value (operation-element fn)))
                                 (operation-parameter fn)))
                        operations)
                (cons widget (find-widget-low (widget-parts widget)))
                (find-widget-low (widget-parts widget))))
          (define (find-widget-low low)
            (cond [(empty? low) empty]
                  [else
                   (append (find-widget (first low))
                           (find-widget-low (rest low)))]))]
    (find-widget widget)))

(check-expect (find-widget Glass (list (make-operation > 5 "name")))
              empty) 
(check-expect (find-widget Wire (list (make-operation > 3 "name")))
              (list Wire))
(check-expect (find-widget Cord (list (make-operation > 3 "name")))
              (list Cord Wire))
(check-expect (find-widget Telephone (list (make-operation > 7 "name")))
              (list Telephone Receiver))


(check-expect (find-widget Wire (list (make-operation > 3 "quantity")))
              empty) 
(check-expect (find-widget Buttons (list (make-operation > 7 "quantity")))
              (list Buttons Numbers))
(check-expect (find-widget Jewelry (list (make-operation > 8 "quantity")))
              (list Rings Necklace Beads))


(check-expect (find-widget Numbers (list (make-operation < 5 "price")))
              empty) 
(check-expect (find-widget Buttons (list (make-operation < 6 "price")))
              (list Buttons Numbers))
(check-expect (find-widget Telephone (list (make-operation < 10 "price")))
              (list Receiver Buttons Numbers Cord Wire))


(check-expect (find-widget Glass (list (make-operation < 6 "quantity")
                                       (make-operation > 4 "price")))
              empty) 
                     
(check-expect (find-widget Beads (list (make-operation < 30 "quantity")
                                       (make-operation > 6 "price")))
              (list Beads))
(check-expect (find-widget Jewelry (list (make-operation < 10 "quantity")
                                         (make-operation > 10 "price")))
              (list Jewelry))


(define (qsort fn? element)
  (local
    [(define (get-widgets widget)        
       (cons widget (get-widgets--low    
                     (widget-parts widget)))) 

     (define (get-widgets--low low)
       (cond
         [(empty? low) empty]
         [else
          (append (get-widgets (first low))
                  (get-widgets--low (rest low)))]))
     
     (define (qsort-wrapper widget)  
       (local
         
         [(define low (get-widgets widget)) 

          (define (qsort-algo low)  
            (cond                   
              [(empty? low) empty]
              [else 
               (local
                 [(define pivot (first low))
                  (define (smaller? widget) (fn? (element widget) (element pivot)))]
                 (append
                  (qsort-algo (filter smaller? (rest low)))               
                  (list pivot)
                  (qsort-algo (filter (λ(widget)
                                        (not (smaller? widget)))
                                      (rest low)))))]))]
             
         (qsort-algo low)))]   
     
    qsort-wrapper))




(define sort-strings (qsort string<? widget-name))
(map widget-name (sort-strings Telephone))
(check-expect (map widget-name (sort-strings Telephone))
              (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))




(define sort-overstocked (qsort > widget-quantity))
(map widget-name (sort-overstocked Necklace))
(check-expect (map widget-name (sort-overstocked Necklace))
              (list "Necklace" "Chain" "Pendant"))