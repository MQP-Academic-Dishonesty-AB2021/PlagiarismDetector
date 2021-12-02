

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(define-struct widget (name quantity time price parts))








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


(define (fn-for-widget widget)
  (local [(define (fn-for-widget--widget widget)
            (... (widget-name widget)                            
                 (widget-quantity widget)                        
                 (widget-time widget)                            
                 (widget-price widget)                           
                 (fn-for-widget--low (widget-parts widget))))   
          (define (fn-for-widget--low low)
            (cond
              [(empty? low) (...)]
              [else (... (fn-for-widget--widget (first low))
                         (fn-for-widget--low (rest low)))]))]
    (fn-for-widget--widget widget))) 
 








(define (find-widgets fn? widget)
  (local [(define (find-widgets--widget widget)
            (if (fn? widget)
                (cons widget
                      (find-widgets--low (widget-parts widget)))
                (find-widgets--low (widget-parts widget))))
          
          (define (find-widgets--low low)
            (cond [(empty? low) empty]
                  [else (append (find-widgets--widget (first low))
                                (find-widgets--low (rest low)))]))]
    (find-widgets--widget widget))) 


(define (find-widget-name-longer-than widget cutoff)
  (find-widgets (λ (widget)
                 (> (string-length (widget-name widget)) cutoff))
               widget))

(define (find-widget-quantity-over widget cutoff)
  (find-widgets (λ (widget)
                 (> (widget-quantity widget) cutoff))
               widget))

(define (find-widgets-cheaper-than widget cutoff)
  (find-widgets (λ (widget)
                 (< (widget-price widget) cutoff))
               widget))

(define (find-widget-hard-make widget q-cutoff p-cutoff)
  (find-widgets (λ (widget)
                 (or (< (widget-quantity widget) q-cutoff)
                     (> (widget-price widget) p-cutoff)))
               widget))



(check-expect (find-widget-name-longer-than Telephone 0) 
              (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget-name-longer-than Telephone 5) 
              (list Telephone Receiver Buttons Numbers))
(check-expect (find-widget-name-longer-than Jewelry 1000) empty) 
(check-expect (find-widget-name-longer-than Wire 5) empty) 
(check-expect 
 (find-widget-name-longer-than Necklace 6)
 (list Necklace Pendant))


(check-expect (find-widget-quantity-over Jewelry 9000) empty) 
(check-expect (find-widget-quantity-over Telephone 2) 
              (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widget-quantity-over Telephone 7)
              (list Receiver Buttons Numbers)) 
(check-expect (find-widget-quantity-over Wire 5) empty) 
(check-expect 
 (find-widget-quantity-over Necklace 6)
 (list Necklace Chain))


(check-expect (find-widgets-cheaper-than Jewelry 0) empty) 
(check-expect (find-widgets-cheaper-than Telephone 1000) 
              (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widgets-cheaper-than Jewelry 7)
              (list Necklace Chain Pendant Bracelet Glass)) 
(check-expect (find-widgets-cheaper-than Wire 5) empty) 
(check-expect 
 (find-widgets-cheaper-than Buttons 6)
 (list Buttons Numbers))


(check-expect (find-widget-hard-make Jewelry 0 1000) empty) 
(check-expect (find-widget-hard-make Telephone 1000 0)
              (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widget-hard-make Telephone 0 0) 
              (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget-hard-make Jewelry 1000 1000) 
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

(check-expect (find-widget-hard-make Jewelry 7 7) 
              (list Jewelry Rings Pendant Bracelet Glass)) 
(check-expect (find-widget-hard-make Telephone 7 7) 
              (list Telephone Wire)) 
(check-expect (find-widget-hard-make Wire 2 6) 
              empty)

















(check-expect (map widget-name (sort-strings Telephone))
              (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))
(check-expect (map widget-name (sort-overstocked Necklace))
              (list "Necklace" "Chain" "Pendant"))


(check-expect (map widget-name (sort-overstocked Wire)) 
              (list "Wire"))


(check-expect (map widget-name (sort-prices Buttons))
              (list "Buttons" "Numbers"))


(check-expect (map widget-name (sort-prices Necklace))
              (list "Chain" "Pendant" "Necklace"))


(define (qsort compare-fn? selector-fn)
  (local
    
    
    [(define (compile-widgets widget)
       (local [(define (compile-widgets--widget widget)
                 (cons widget
                       (compile-widgets--low (widget-parts widget))))
               (define (compile-widgets--low low)
                 (cond [(empty? low) empty]
                       [else (append (compile-widgets--widget (first low))
                                     (compile-widgets--low (rest low)))]))]
         (compile-widgets--widget widget))) 

     
     (define (qsort-curry widget)
       
       (local [(define low (compile-widgets widget))
               (define (qsort-curry-inner low)
                 (cond [(empty? low) empty]
                       [else (local
                               [(define pivot (first low))
                                (define (smaller? current-widget)
                                  (compare-fn? (selector-fn current-widget) 
                                               (selector-fn pivot)))]
                               
                               
                               (append (qsort-curry-inner (filter smaller? (rest low)))
                                       (list pivot)
                                       (qsort-curry-inner (filter (λ (widget)
                                                                    (not (smaller? widget)))
                                                                  (rest low)))))]))]
         (qsort-curry-inner low)))] 
    qsort-curry)) 




(define sort-strings (qsort string<? widget-name))

(define sort-overstocked (qsort > widget-quantity))
(define sort-prices (qsort < widget-price))