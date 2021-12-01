

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



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















(define (widop fn? part nat num wid)
  (local [(define (fn-for-wid fn? part nat num wid)
            
            (local [(define (needs-number? fn?)     
                      (if (false? num)              
                          (fn? (part wid) nat)      
                          (fn? wid nat num)))]
              
            (if (needs-number? fn?)
                (cons wid (fn-for-low (widget-parts wid)))
                (fn-for-low (widget-parts wid)))))
        
          (define (fn-for-low low)
            (cond
              [(empty? low) empty]
              [else
               (append (fn-for-wid  fn? part nat num (first low))
                       (fn-for-low (rest low)))]))]
    
    (fn-for-wid fn? part nat num wid)))






(check-expect (find-widget-name-longer-than Receiver 3) 
              (list (make-widget "Receiver" 10 5 7 empty))) 

(check-expect (find-widget-name-longer-than Rings 10) 
              empty)                                  
 
(check-expect (find-widget-name-longer-than Telephone 5) 
              (list Telephone Receiver Buttons Numbers))

(check-expect (find-widget-name-longer-than Jewelry 6) 
              (list Jewelry Necklace Pendant Bracelet))

(define (find-widget-name-longer-than widget length)
  (local [(define (longer-than? str-widName len)
                  (> (string-length str-widName) len))]
    (widop longer-than? widget-name length false widget)))







(check-expect (find-widget-quantity-over Wire 0)
                (list Wire))   
(check-expect (find-widget-quantity-over Wire 10)
                empty)         
(check-expect (find-widget-quantity-over Telephone 8)
                (list Receiver Numbers)) 
(check-expect (find-widget-quantity-over Telephone 0)
                (list Telephone Receiver Buttons Numbers Cord Wire))


(define (find-widget-quantity-over widget quant)
  (widop > widget-quantity quant false widget))







(check-expect (find-widgets-cheaper-than Beads 1) 
              empty)

(check-expect (find-widgets-cheaper-than Telephone 10) 
              (list Receiver Buttons Numbers Cord Wire))

(check-expect (find-widgets-cheaper-than Jewelry 8) 
              (list Necklace Chain Pendant Bracelet Beads Glass))

(define (find-widgets-cheaper-than widget cost)
  (widop < widget-price cost false widget))








(check-expect (find-widget-hard-make Jewelry 0 100) empty) 
(check-expect (find-widget-hard-make Jewelry 30 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect (find-widget-hard-make Telephone 5 6)
              (list Telephone Receiver Wire)) 

(define (find-widget-hard-make widget quant cost)
  (local [(define (isHardMake? widget quant cost)
                     (or (< (widget-quantity widget) quant)
                         (> (widget-price widget) cost)))]
    (widop isHardMake? widget-time quant cost widget)))






(check-expect (map widget-name (sort-strings Receiver))
              (list "Receiver"))

(check-expect (map widget-name (sort-overstocked Wire))
              (list "Wire"))

(check-expect (map widget-name (sort-times Glass))
             (list "Glass"))

(check-expect (map widget-name (sort-prices Chain))
              (list "Chain"))




(check-expect (map widget-name (sort-strings Telephone))
              (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))
(check-expect (map widget-name (sort-strings Bracelet))
              (list "Beads" "Bracelet" "Glass"))

(check-expect (map widget-name (sort-overstocked Necklace))
              (list "Necklace" "Chain" "Pendant"))
(check-expect (map widget-name (sort-overstocked Buttons))
              (list "Numbers" "Buttons"))

(check-expect (map widget-name (sort-times Necklace))
             (list "Chain" "Pendant" "Necklace"))
(check-expect (map widget-name (sort-times Telephone))
              (list "Receiver" "Buttons" "Numbers" "Cord" "Wire" "Telephone"))

(check-expect (map widget-name (sort-prices Jewelry))
              (list "Jewelry set" "Rings" "Beads" "Bracelet" "Glass" "Necklace" "Chain" "Pendant"))
(check-expect (map widget-name (sort-prices Cord))
              (list "Cord" "Wire"))









(define (qsort fn? part)
  (λ (wid) 
    (local [
          (define unsorted (widop > widget-quantity -1 false wid))
          
          (define (qsort-wid fn? part low)
            (cond
              [(empty? low) empty]
              [else
               (local [
                       (define pivot (first low))
                       (define lessthan (filter
                                         (λ (x)
                                           (fn? (part x) (part pivot)))
                                         (rest low)))
                       (define morethan (filter
                                         (λ (x)
                                           (not (fn? (part x) (part pivot))))
                                         (rest low)))]
               (append
                (qsort-wid fn? part lessthan)
                (list pivot)
                (qsort-wid fn? part morethan)))]))]
    (qsort-wid fn? part unsorted))))








(define sort-strings (qsort string<? widget-name))

(define sort-overstocked (qsort > widget-quantity))
(define sort-times (qsort < widget-time))
(define sort-prices (qsort > widget-price))