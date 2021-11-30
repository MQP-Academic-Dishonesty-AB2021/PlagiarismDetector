

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





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







 












(check-expect (main-fn-for-widget  (λ (wdg) (> (string-length (widget-name wdg)) 5)) Wire) empty)                      
(check-expect (main-fn-for-widget  (λ (wdg) (> (string-length (widget-name wdg)) 2)) Wire) (list Wire))                
(check-expect (main-fn-for-widget  (λ (wdg) (> (widget-quantity wdg) 2)) Wire) (list Wire))                            
(check-expect (main-fn-for-widget  (λ (wdg) (< (widget-price wdg)    6)) Telephone) (list Buttons Numbers Cord Wire))  
(check-expect (main-fn-for-widget  (λ (wdg) (or (< (widget-quantity wdg) 0)
                                                (> (widget-price wdg) 6))) Telephone) (list Telephone Receiver))       


(define (main-fn-for-widget fn? wdg)                  
  (local[
         (define (fn-for-widget wdg)
           (if (fn? wdg)
               (cons wdg (fn-for--LOW (widget-parts wdg))) 
               (fn-for--LOW (widget-parts wdg))))        
         
         (define (fn-for--LOW LOW)
           (cond [(empty? LOW) empty]
                 [else
                  (append (fn-for-widget (first LOW))   
                          (fn-for--LOW (rest LOW)))]))]  
  
    (fn-for-widget wdg)))










(check-expect (find-widget-name-longer-than Wire 2) (list Wire))                                              
(check-expect (find-widget-name-longer-than Wire 4) empty)                                                    
(check-expect (find-widget-name-longer-than Wire 10) empty)                                                   
(check-expect (find-widget-name-longer-than Cord 2) (list Cord Wire))                                         
(check-expect (find-widget-name-longer-than Telephone 2) (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widget-name-longer-than Telephone 5) (list Telephone Receiver Buttons Numbers))           

(define (find-widget-name-longer-than wdg num)
  (main-fn-for-widget  (λ (wdg) (> (string-length (widget-name wdg)) num)) wdg))







(check-expect (find-widget-quantity-over Wire 1) (list Wire))                                                  
(check-expect (find-widget-quantity-over Wire 3) empty)                                                        
(check-expect (find-widget-quantity-over Wire 10) empty)                                                       
(check-expect (find-widget-quantity-over Cord 6) (list Cord))                                                  
(check-expect (find-widget-quantity-over Cord 1) (list Cord Wire))                                             
(check-expect (find-widget-quantity-over Telephone 8) (list Receiver Numbers))                                 

(define (find-widget-quantity-over wdg num)
  (main-fn-for-widget  (λ (wdg) (> (widget-quantity wdg) num)) wdg))







(check-expect (find-widgets-cheaper-than Wire 4) empty)                                                                 
(check-expect (find-widgets-cheaper-than Wire 5) empty)                                                                 
(check-expect (find-widgets-cheaper-than Wire 6) (list Wire))                                                           
(check-expect (find-widgets-cheaper-than Cord 6) (list Cord Wire))                                                      
(check-expect (find-widgets-cheaper-than Telephone 6) (list Buttons Numbers Cord Wire))                                 
(check-expect (find-widgets-cheaper-than Telephone 4) empty)                                                            

(define (find-widgets-cheaper-than wdg num)
  (main-fn-for-widget  (λ (wdg) (< (widget-price wdg)    num)) wdg))










(check-expect (find-widget-hard-make Wire 0 0) (list Wire))                    
(check-expect (find-widget-hard-make Wire 5 0) (list Wire))                    
(check-expect (find-widget-hard-make Wire 0 10) empty)                         
(check-expect (find-widget-hard-make Telephone 0 100) empty)                   


(check-expect (find-widget-hard-make Wire 5 10) (list Wire))                   
(check-expect (find-widget-hard-make Telephone 4 100) (list Wire))             
(check-expect (find-widget-hard-make Telephone 0 6) (list Telephone Receiver)) 

(define (find-widget-hard-make wdg qty cost)
  (main-fn-for-widget  (λ (wdg) (or (< (widget-quantity wdg) qty)
                                    (> (widget-price wdg) cost))) wdg))







 








(define (qsort fn? field?)
  (local[
         (define (flatten wdg)
           (local [(define (fn-for-one wdg)            
                     (cons wdg (fn-for-one-LOW (widget-parts wdg))))                

                   (define (fn-for-one-LOW LOW)
                     (cond [(empty? LOW) empty]
                           [else
                            (append (fn-for-one (first LOW))
                                    (fn-for-one-LOW (rest LOW)))]))]          
             (fn-for-one wdg)))
         
         (define (qsort lox)
           (local [
                   (define (qsort lox)
                     (cond
                       [(empty? lox) empty]
                       [else 
                        (local
                          [(define pivot (first lox))
                           (define (smaller? n) (fn? (field? n) (field? pivot)))]
                          (append
                           (qsort (filter smaller? (rest lox)))               
                           (list pivot)
                           (qsort
                            (filter (λ(n) (not (smaller? n))) (rest lox)))))]))]
             (qsort (flatten lox))))]
    qsort))




(define sort-strings (qsort string<? widget-name))




(define sort-overstocked (qsort > widget-quantity))


(check-expect (sort-strings Wire) (list Wire))                                                                               
(check-expect (sort-strings Cord) (list Cord Wire))                                                                          
(check-expect (map widget-name (sort-strings Telephone)) (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))    
(check-expect (map widget-name (sort-overstocked Necklace)) (list "Necklace" "Chain" "Pendant"))                             
(check-expect (map widget-name (sort-overstocked Wire)) (list "Wire"))                                                       


  
