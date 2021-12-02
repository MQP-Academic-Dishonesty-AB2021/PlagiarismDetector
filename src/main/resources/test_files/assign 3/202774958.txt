

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))







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




  







(check-expect (widget-outer Chain (λ (widget)                      
                                    (< (widget-price widget) 10)))
              (list Chain))

(check-expect (widget-outer Telephone (λ (widget)                  
                                    (< (widget-quantity widget) 10)))
              (list Telephone Buttons Numbers Cord Wire))

(define (widget-outer widget fn?)
  (local [
          (define (fn-for-widget widget)
            (if (fn? widget)
                (cons widget (fn-for-low (widget-parts widget)))
                (fn-for-low (widget-parts widget))))
          (define (fn-for-low low)
            (cond
              [(empty? low) empty]
              [else
               (append (fn-for-widget (first low))
                       (fn-for-low (rest low)))]))]
    (fn-for-widget widget)))




(check-expect (find-widget-name-longer-than Wire 3) (list Wire)) 
(check-expect (find-widget-name-longer-than Buttons 6) (list Buttons Numbers)) 
(check-expect (find-widget-name-longer-than Chain 5) empty) 
(check-expect (find-widget-name-longer-than Telephone 7) (list Telephone Receiver)) 
(check-expect (find-widget-name-longer-than Telephone 10) empty) 

(define (find-widget-name-longer-than widget name)
  (widget-outer widget
                (λ (widget)
                  (> (string-length (widget-name widget)) name))))



(check-expect (find-widget-quantity-over Wire 2) (list Wire)) 
(check-expect (find-widget-quantity-over Buttons 7) (list Buttons Numbers)) 
(check-expect (find-widget-quantity-over Chain 7) empty) 
(check-expect (find-widget-quantity-over Telephone 7) (list Receiver Buttons Numbers)) 

(define (find-widget-quantity-over widget quantity)
  (widget-outer widget
                (λ (widget)
                  (> (widget-quantity widget) quantity))))



(check-expect (find-widgets-cheaper-than Wire 7) (list Wire)) 
(check-expect (find-widgets-cheaper-than Buttons 8) (list Buttons Numbers)) 
(check-expect (find-widgets-cheaper-than Chain 1) empty) 
(check-expect (find-widgets-cheaper-than Telephone 6) (list Buttons Numbers Cord Wire)) 



(define (find-widgets-cheaper-than widget price)
  (widget-outer widget
                (λ (widget)
                  (< (widget-price widget) price))))





(check-expect (find-widget-hard-make Wire 4 4) (list Wire)) 
(check-expect (find-widget-hard-make Wire 4 6) (list Wire)) 
(check-expect (find-widget-hard-make Wire 1 6) empty) 
(check-expect (find-widget-hard-make Buttons 10 4) (list Buttons Numbers)) 
(check-expect (find-widget-hard-make Buttons 9 5) (list Buttons)) 
(check-expect (find-widget-hard-make Chain 7 1) empty) 
(check-expect (find-widget-hard-make Telephone 9 5) (list Telephone Receiver Buttons Cord Wire)) 



(define (find-widget-hard-make widget quantity price)
  (local [(define (hard-make? widget) 
            (or (< (widget-quantity widget) quantity)
                (> (widget-price widget) price)))]
    (widget-outer widget
                  (λ (widget) (hard-make? widget)))))








(define (qsort fn1 fn2) 
  (local [(define (qsort-outer widget) 
            (local
              [(define (qsort-inner low) 
                 (cond
                   [(empty? low) empty]
                   [else 
                    (local [(define pivot (first low))
                            (define (smaller? n) (fn1 (fn2 n) (fn2 pivot)))]
                      (append
                       (qsort-inner (filter smaller? (rest low)))
                       (list pivot)
                       (qsort-inner (filter (λ (n)
                                              (not (smaller? n))) (rest low)))))]))]
              (qsort-inner (find-subwidgets widget))))]
    qsort-outer))
       




(check-expect (find-subwidgets Chain) (list Chain)) 
(check-expect (find-subwidgets Buttons) (list Buttons Numbers)) 
(check-expect (find-subwidgets Jewelry) (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass)) 



(define (find-subwidgets widget)
  (local[
         (define (fn-for-widget widget)
           (cons widget
                 (fn-for-low (widget-parts widget)))) 

         (define (fn-for-low low)
           (cond
             [(empty? low) empty]
             [else
              (append (fn-for-widget (first low))
                      (fn-for-low (rest low)))]))]
    (fn-for-widget widget)))






(check-expect (sort-strings Wire) (list Wire)) 
(check-expect (sort-strings Cord) (list Cord Wire)) 
(check-expect (sort-strings Telephone) (list Buttons Cord Numbers Receiver Telephone Wire)) 

(define sort-strings (qsort string<? widget-name))










