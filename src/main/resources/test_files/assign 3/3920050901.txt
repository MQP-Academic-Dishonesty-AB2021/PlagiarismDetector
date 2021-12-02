

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part2v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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






 








(define (fn-widget fn? widget)
  (local [(define (fn-for-widget widget)
            (if (fn? widget)
                (cons widget (fn-for-low (widget-parts widget)))
                (fn-for-low (widget-parts widget))))
          (define (fn-for-low low)
            (cond [(empty? low) empty]
                  [else
                   (append (fn-for-widget (first low))
                           (fn-for-low (rest low)))]))]
    (fn-for-widget widget)))










(define (find-widget-name-longer-than widget num)
  (fn-widget (λ (n) (> (string-length (widget-name n)) num))
             widget))


(check-expect (find-widget-name-longer-than Jewelry 0) (list Jewelry 
                                                             Rings
                                                             Necklace
                                                             Chain
                                                             Pendant
                                                             Bracelet
                                                             Beads
                                                             Glass))
(check-expect (find-widget-name-longer-than Jewelry 5) (list Jewelry 
                                                             Necklace
                                                             Pendant
                                                             Bracelet))
(check-expect (find-widget-name-longer-than Cord 2) (append (list Cord) 
                                                            (find-widget-name-longer-than Wire 2)))
(check-expect (find-widget-name-longer-than Wire 4) empty) 
(check-expect (find-widget-name-longer-than Wire 3) (list Wire)) 
(check-expect (find-widget-name-longer-than Wire 10) empty) 







(define (find-widget-quantity-over widget amount)
  (fn-widget (λ (n) (> (widget-quantity n) amount))
             widget))




(check-expect (find-widget-quantity-over Wire 0) (list Wire)) 
(check-expect (find-widget-quantity-over Wire 100) empty) 
(check-expect (find-widget-quantity-over Telephone 8) (list Receiver 
                                                            Numbers))
(check-expect (find-widget-quantity-over Telephone 7) (append (list Receiver) 
                                                              (find-widget-quantity-over Buttons 7)))











(check-expect (find-widgets-cheaper-than Wire 100) (list Wire)) 
(check-expect (find-widgets-cheaper-than Wire 0) empty) 
(check-expect (find-widgets-cheaper-than Telephone 15) (list Receiver 
                                                             Buttons
                                                             Numbers
                                                             Cord
                                                             Wire))
(check-expect (find-widgets-cheaper-than Telephone 7) (list Buttons 
                                                            Numbers
                                                            Cord
                                                            Wire))
(check-expect (find-widgets-cheaper-than Necklace 0) empty) 
(check-expect (find-widgets-cheaper-than Necklace 100) (list Necklace 
                                                             Chain
                                                             Pendant))



(define (find-widgets-cheaper-than widget cost)
  (fn-widget (λ (n) (< (widget-price n) cost))
             widget))
















(check-expect (find-widget-hard-make Necklace 0 100) empty) 

(check-expect (find-widget-hard-make Necklace 100 0) (list Necklace 
                                                           Chain
                                                           Pendant))

(check-expect (find-widget-hard-make Necklace 100 100) (list Necklace 
                                                             Chain
                                                             Pendant))

(check-expect (find-widget-hard-make Necklace 0 0) (list Necklace 
                                                         Chain
                                                         Pendant))


(check-expect (find-widget-hard-make Cord 3 100) empty) 

(check-expect (find-widget-hard-make Cord 7 100) (list Wire)) 


(check-expect (find-widget-hard-make Cord 0 5) empty) 




(define (find-widget-hard-make widget amount cost)
  (fn-widget (λ (n) (or (< (widget-quantity n) amount)
                        (> (widget-price n) cost)))
             widget))







(define (qsort comp? property)
  (local [(define (qsort-in widget)
            (local [(define low (if (widget? widget)
                                    (fn-widget widget? widget)
                                    widget))]
              (cond [(empty? low) empty]
                    [else
                     (local [(define pivot (first low))
                             (define (smaller? n)
                               (comp? (property n) (property pivot)))]
                       (append
                        (qsort-in (filter smaller? (rest low)))
                        (list pivot)
                        (qsort-in (filter (λ (n) (not (smaller? n))) (rest low)))))])))]
    qsort-in))





(define sort-strings (qsort string<? widget-name))
(map widget-name (sort-strings Telephone))



(define sort-overstocked (qsort > widget-quantity))
(map widget-name (sort-overstocked Necklace))