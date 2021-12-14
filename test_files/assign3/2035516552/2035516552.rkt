

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


(define 1our (make-widget "1our" 3 5 3 empty))
(define 2our (make-widget "2our" 7 5 5 (list 1our)))
(define 3our (make-widget "3our" 9 5 5 empty))
(define Four (make-widget "Four" 8 5 5 (list 3our)))
(define 5our (make-widget "5our" 10 5 7 empty))
(define 6our (make-widget "6our" 5 20 15
                               (list 5our Four 2our)))
(define 7our (make-widget "7our" 5 20 10
                               (list 1our 3our)))
(define 8our (make-widget "8our" 7 20 10
                               (list 7our)))

(define eee (make-widget "eee" 5 20 3
                               (list 1our)))
(define thr (make-widget "thr" 5 20 3
                               (list eee)))


(define BASE (make-widget "" 0 0 0 empty))



  


  
         
         
         


         


(define (find-widget-name-longer-than wid len) 
  (if  (> (string-length (widget-name wid)) len) 
       (cons wid (find-widget-name-longer-than--low (widget-parts wid) len))
       (find-widget-name-longer-than--low (widget-parts wid) len)))
  


(define (find-widget-name-longer-than--low low len) 
  (cond [(empty? low) empty]
        [else 
         (append(find-widget-name-longer-than (first low) len) 
                (find-widget-name-longer-than--low (rest low) len))]))
 




(check-expect (find-widget-name-longer-than eee 3) (list 1our))



(check-expect (find-widget-name-longer-than thr 3) (list 1our))


(check-expect (find-widget-name-longer-than Wire 5) empty)



(check-expect (find-widget-name-longer-than 7our 5) empty)
  


(check-expect (find-widget-name-longer-than 6our 5) empty)
  

(check-expect (find-widget-name-longer-than Wire 3) 
              (list Wire))
  

(check-expect (find-widget-name-longer-than 7our 3) (list 7our 1our 3our))



(check-expect (find-widget-name-longer-than Telephone 3) 
              (list Telephone Receiver Buttons Numbers Cord Wire))
  

(check-expect (find-widget-name-longer-than Wire 4) empty)



(check-expect (find-widget-name-longer-than Cord 4) empty)
  


(check-expect (find-widget-name-longer-than 6our 4) empty)


(check-expect (find-widget-name-longer-than BASE 1) empty)




(check-expect (find-widget-name-longer-than--low empty 7) empty)


(check-expect (find-widget-name-longer-than--low (widget-parts 2our) 5) empty)


(check-expect (find-widget-name-longer-than--low (widget-parts 2our) 3) (list 1our))


(check-expect (find-widget-name-longer-than--low (widget-parts 2our) 4) empty)
  

(check-expect (find-widget-name-longer-than--low (list 1our 3our) 5) empty)


(check-expect (find-widget-name-longer-than--low(list 1our 3our) 3)(list 1our 3our))


(check-expect (find-widget-name-longer-than--low (list 1our 3our) 4) empty)
              

              


(define (find-widget-quantity-over wid stock)
  (if (> (widget-quantity wid) stock)
      (cons wid 
            (find-widget-quantity-over--low (widget-parts wid) stock))
      (find-widget-quantity-over--low (widget-parts wid) stock))) 



(define (find-widget-quantity-over--low low stock)
  (cond [(empty? low) empty]
        [else 
        	(append 
           (find-widget-quantity-over (first low) stock) 
           (find-widget-quantity-over--low (rest low) stock))]))
              



(check-expect (find-widget-quantity-over Wire 100) empty)
  


(check-expect (find-widget-quantity-over Cord 100) empty)



(check-expect (find-widget-quantity-over Telephone 100) empty)
  

(check-expect (find-widget-quantity-over Wire 1) 
                (list Wire))



(check-expect (find-widget-quantity-over Cord 1) 
                (list Cord Wire))
  


(check-expect (find-widget-quantity-over Telephone 1) 
                (list Telephone Receiver Buttons Numbers Cord Wire))
  



(check-expect (find-widget-quantity-over Telephone 5) 
        (list Receiver Buttons Numbers Cord))
  



(check-expect (find-widget-quantity-over Necklace 7)
                (list Necklace)) 
  
    

    

(check-expect (find-widget-quantity-over--low empty 1) empty)
    

(check-expect (find-widget-quantity-over--low (list Wire) 100) empty)
    


(check-expect (find-widget-quantity-over--low (list Telephone) 100) empty)
    

(check-expect (find-widget-quantity-over--low (list Wire) 1) (list Wire))
    


(check-expect (find-widget-quantity-over--low (list Telephone) 1) 
                  (list Telephone Receiver Buttons Numbers Cord Wire))
    
    
    
    
    
    (check-expect (find-widget-quantity-over--low (list Beads) 6) 
                  (list Beads)) 
    
    
    
    
    
    (check-expect (find-widget-quantity-over--low (list Necklace) 4)
                  (list Necklace Chain)) 

    
    (check-expect (find-widget-quantity-over--low empty 0) empty)
    




(define (find-widgets-cheaper-than wid p)
  (if  (< (widget-price wid) p)
       (cons wid (find-widgets-cheaper-than--low (widget-parts wid) p))
       (find-widgets-cheaper-than--low (widget-parts wid) p))) 



(define (find-widgets-cheaper-than--low low p)
  (cond [(empty? low) empty]
        [else 
         (append (find-widgets-cheaper-than (first low) p) 
         (find-widgets-cheaper-than--low (rest low) p))]))




(check-expect (find-widgets-cheaper-than BASE 0) empty)


(check-expect (find-widgets-cheaper-than Four 5) empty)



(check-expect (find-widgets-cheaper-than thr 3) empty)



(check-expect (find-widgets-cheaper-than 7our 4) (list 1our))



(check-expect (find-widgets-cheaper-than 8our 4) (list 1our))




(check-expect (find-widgets-cheaper-than--low empty 0) empty)


(check-expect (find-widgets-cheaper-than--low (list thr) 3) empty)


(check-expect (find-widgets-cheaper-than--low (list thr eee) 3) empty)


(check-expect (find-widgets-cheaper-than--low (list thr) 2) empty)




(check-expect (find-widgets-cheaper-than--low (list thr 5our eee) 4)
              (list thr eee 1our eee 1our))






(define (find-widget-hard-make wid stock cost)
  (if 
   (hard-make? wid stock cost)
   (cons wid
       (find-widget-hard-make--low 
        (widget-parts wid) stock cost))
   (find-widget-hard-make--low 
    (widget-parts wid) stock cost)))




(define (find-widget-hard-make--low low stock cost)
  (cond [(empty? low) empty]
        [else 
         (append 
          (find-widget-hard-make 
           (first low) stock cost) 
          (find-widget-hard-make--low 
           (rest low) stock cost))])) 





(define (hard-make? wid stock cost)
  (or 
   	(< (widget-quantity wid) stock)
    (> (widget-price wid) cost)))




(check-expect (hard-make? (make-widget "n/a" 1 2 1 empty) 2 2) true)
 


(check-expect (hard-make? (make-widget "n/a" 3 2 1 empty) 2 2) false)


(check-expect (hard-make? (make-widget "n/a" 2 2 2 empty) 2 2) false)






(check-expect (find-widget-hard-make BASE 0 0) empty)



(check-expect (find-widget-hard-make
               (make-widget "lv0" 5 5 5
                            (list (make-widget "lv1" 5 5 5 empty))) 5 5) empty)



(check-expect (find-widget-hard-make
               (make-widget "lv0" 5 5 5
                     (list (make-widget "lv1" 5 5 5
                          (list (make-widget "lv2" 5 5 5 empty))))) 5 5) empty)
                                          


(check-expect (find-widget-hard-make
               (make-widget "lv0" 5 5 5
                            (list (make-widget "lv1" 3 5 7 empty))) 5 5)
              (list (make-widget "lv1" 3 5 7 empty)))
                                                       


(check-expect (find-widget-hard-make
               (make-widget "lv0" 5 5 5 (list (make-widget "lv1" 5 5 5
                                (list (make-widget "lv2" 3 5 7 empty))))) 5 5)
              (list (make-widget "lv2" 3 5 7 empty)))




(check-expect (find-widget-hard-make--low empty 0 0) empty)


(check-expect (find-widget-hard-make--low (list Wire) 3 5) empty)



(check-expect (find-widget-hard-make--low
               (list Wire (make-widget "n/a" 3 4 5 empty)) 3 5) empty)                                       



(check-expect (find-widget-hard-make--low (list Wire) 10 0) (list Wire))                                  



(check-expect (find-widget-hard-make--low (list (make-widget "n/a" 5 4 5 empty)
                                                Buttons
                                                Receiver) 4 6) (list Receiver))