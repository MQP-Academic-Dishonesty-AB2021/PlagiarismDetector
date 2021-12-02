

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






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









(check-expect (find-widget-name-longer-than Wire 2) (list Wire))                                              
(check-expect (find-widget-name-longer-than Wire 4) empty)                                                    
(check-expect (find-widget-name-longer-than Wire 10) empty)                                                   
(check-expect (find-widget-name-longer-than Cord 2) (list Cord Wire))                                         
(check-expect (find-widget-name-longer-than Telephone 2) (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widget-name-longer-than Telephone 5) (list Telephone Receiver Buttons Numbers))           

(check-expect (find-widget-name-longer-than-LOW empty 2) empty)                                               
(check-expect (find-widget-name-longer-than-LOW (widget-parts Wire) 2) empty)                                 
(check-expect (find-widget-name-longer-than-LOW (widget-parts Wire) 10) empty)                                
(check-expect (find-widget-name-longer-than-LOW (widget-parts Cord) 2) (list Wire))                           
(check-expect (find-widget-name-longer-than-LOW (widget-parts Cord) 10) empty)                                
(check-expect (find-widget-name-longer-than-LOW (widget-parts Telephone) 5) (list Receiver Buttons Numbers))  


(define (find-widget-name-longer-than wdg num)
  (if (> (string-length (widget-name wdg)) num)
      (cons wdg (find-widget-name-longer-than-LOW (widget-parts wdg) num))
      (find-widget-name-longer-than-LOW (widget-parts wdg) num)))





(define (find-widget-name-longer-than-LOW LOW num)
  (cond [(empty? LOW) empty]
        [else
         (append (find-widget-name-longer-than (first LOW) num)
                 (find-widget-name-longer-than-LOW (rest LOW) num))]))







(check-expect (find-widget-quantity-over Wire 1) (list Wire))                                                  
(check-expect (find-widget-quantity-over Wire 3) empty)                                                        
(check-expect (find-widget-quantity-over Wire 10) empty)                                                       
(check-expect (find-widget-quantity-over Cord 6) (list Cord))                                                  
(check-expect (find-widget-quantity-over Cord 1) (list Cord Wire))                                             
(check-expect (find-widget-quantity-over Telephone 8) (list Receiver Numbers))                                 

(check-expect (find-widget-quantity-over-LOW empty 2) empty)                                                   
(check-expect (find-widget-quantity-over-LOW (widget-parts Wire) 3) empty)                                     
(check-expect (find-widget-quantity-over-LOW (widget-parts Wire) 10) empty)                                    
(check-expect (find-widget-quantity-over-LOW (widget-parts Cord) 2) (list Wire))                               
(check-expect (find-widget-quantity-over-LOW (widget-parts Cord) 10) empty)                                    
(check-expect (find-widget-quantity-over-LOW (widget-parts Telephone) 5) (list Receiver Buttons Numbers Cord)) 


(define (find-widget-quantity-over wdg num)
  (if (> (widget-quantity wdg) num)
      (cons wdg (find-widget-quantity-over-LOW (widget-parts wdg) num))
      (find-widget-quantity-over-LOW (widget-parts wdg) num)))





(define (find-widget-quantity-over-LOW LOW num)
  (cond [(empty? LOW) empty]
        [else
         (append (find-widget-quantity-over (first LOW) num)
                 (find-widget-quantity-over-LOW (rest LOW) num))]))








(check-expect (find-widgets-cheaper-than Wire 4) empty)                                                                 
(check-expect (find-widgets-cheaper-than Wire 5) empty)                                                                 
(check-expect (find-widgets-cheaper-than Wire 6) (list Wire))                                                           
(check-expect (find-widgets-cheaper-than Cord 6) (list Cord Wire))                                                      
(check-expect (find-widgets-cheaper-than Telephone 6) (list Buttons Numbers Cord Wire))                                 
(check-expect (find-widgets-cheaper-than Telephone 4) empty)                                                            

(check-expect (find-widgets-cheaper-than-LOW empty 4) empty)                                                            
(check-expect (find-widgets-cheaper-than-LOW (list Wire) 4) empty)                                                      
(check-expect (find-widgets-cheaper-than-LOW  (list Telephone) 16) (list Telephone Receiver Buttons Numbers Cord Wire)) 

(define (find-widgets-cheaper-than wdg num)
  (if (< (widget-price wdg) num)
      (cons wdg (find-widgets-cheaper-than-LOW (widget-parts wdg) num))
      (find-widgets-cheaper-than-LOW (widget-parts wdg) num)))





(define (find-widgets-cheaper-than-LOW LOW num)
  (cond [(empty? LOW) empty]
        [else
         (append (find-widgets-cheaper-than (first LOW) num)
                 (find-widgets-cheaper-than-LOW (rest LOW) num))]))











(check-expect (find-widget-hard-make Wire 0 0) (list Wire))                    
(check-expect (find-widget-hard-make Wire 5 0) (list Wire))                    
(check-expect (find-widget-hard-make Wire 0 10) empty)                         
(check-expect (find-widget-hard-make Telephone 0 100) empty)                   


(check-expect (find-widget-hard-make Wire 5 10) (list Wire))                   
(check-expect (find-widget-hard-make Telephone 4 100) (list Wire))             
(check-expect (find-widget-hard-make Telephone 0 6) (list Telephone Receiver)) 


(check-expect (find-widget-hard-make-LOW (list Wire) 4 6) (list Wire))                                                 
(check-expect (find-widget-hard-make-LOW (list Telephone) 0 6) (list Telephone Receiver))                              
(check-expect (find-widget-hard-make-LOW empty 0 0) empty)                                                             
(check-expect (find-widget-hard-make-LOW (list Telephone) 100 0) (list Telephone Receiver Buttons Numbers Cord Wire))  

(define (find-widget-hard-make wdg qty cost)
  (if (deciding-factor wdg qty cost)
      (cons wdg (find-widget-hard-make-LOW (widget-parts wdg) qty cost))
      (find-widget-hard-make-LOW (widget-parts wdg) qty cost)))





(define (find-widget-hard-make-LOW LOW qty cost)
  (cond [(empty? LOW) empty]
        [else
         (append (find-widget-hard-make (first LOW) qty cost)
                 (find-widget-hard-make-LOW (rest LOW) qty cost))]))







(check-expect (deciding-factor Wire 8 10) true)       
(check-expect (deciding-factor Wire 2 6) false)       
(check-expect (deciding-factor Wire 2 4) true)        
(check-expect (deciding-factor Telephone 4 16) false) 
(check-expect (deciding-factor Telephone 6 14) true)  
(check-expect (deciding-factor Wire 3 5) false)       
(check-expect (deciding-factor Wire 3 4) true)        
(check-expect (deciding-factor Wire 4 5) true)        


(define (deciding-factor wdg qty cost)
  (or (< (widget-quantity wdg) qty)
      (> (widget-price wdg) cost)))