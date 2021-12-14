

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab3Part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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






































(define (fold-widget c1 c2 widget num)
  (local [(define (fn-for-widget widge n)
            (if (c1 (c2 widge) n)
                (cons widge (fn-for-low (widget-parts widge) n))
                (fn-for-low (widget-parts widge) n)))
         
          (define (fn-for-low low n)
            (cond [(empty? low) empty]
                  [else
                   (append (fn-for-widget (first low) n)   
                           (fn-for-low (rest low) n))]))]
    
    (fn-for-widget widget num)))









(check-expect (find-widget-name-longer-than Bracelet 9) empty) 
(check-expect (find-widget-name-longer-than Bracelet 3) (list Bracelet Beads Glass)) 
(check-expect (find-widget-name-longer-than Buttons 6) (list Buttons Numbers)) 
(check-expect (find-widget-name-longer-than Jewelry 5) (list Jewelry Necklace Pendant Bracelet)) 

(define (find-widget-name-longer-than widge n)
  (local [(define (flip w)
            (string-length (widget-name w)))]
    (fold-widget > flip widge n)))






(check-expect (find-widget-quantity-over Wire 3) empty) 
(check-expect (find-widget-quantity-over Beads 8) (list Beads)) 
(check-expect (find-widget-quantity-over Bracelet 4) (list Bracelet Beads Glass)) 

(define (find-widget-quantity-over widge num)
  (fold-widget > widget-quantity widge num)) 






(check-expect (find-widgets-cheaper-than Bracelet 1) empty) 
(check-expect (find-widgets-cheaper-than Bracelet 10) (list Bracelet Beads Glass)) 
(check-expect (find-widgets-cheaper-than Bracelet 5) (list Glass)) 


(define (find-widgets-cheaper-than widge num)
  (fold-widget < widget-price widge num)) 















(check-expect (find-widget-hard-make Chain 4) empty) 
(check-expect (find-widget-hard-make Bracelet 5)(list Beads)) 
(check-expect (find-widget-hard-make Telephone 5) (list Wire Telephone Receiver)) 

(define (find-widget-hard-make widge num)  
  (append
   (fold-widget < widget-quantity widge num)  
   (fold-widget > widget-price widge num)))






(define (qsort fn1 parm1)                                                        
  (local
    [(define (qsort-inner widge)
       (fold-widget fn1 parm1 widge (parm1 widge)))]
    qsort-inner))
     


























(define sort-strings (qsort string<? widget-name))
(check-expect(map widget-name (sort-strings Telephone))
             (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))

(define sort-overstocked (qsort > widget-quantity))
(check-expect(map widget-name (sort-overstocked Necklace))
             (list "Necklace" "Chain" "Pendant"))
