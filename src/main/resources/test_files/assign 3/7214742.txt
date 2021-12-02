

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






(define (fn-for-widget widget)
  (local
    [(define (fn-for-widget widget)
       (... (widget-name widget)
            (widget-quantity widget)
            (widget-time widget)
            (widget-price widget)
            (fn-for-low (widget-parts widget))))
     (define (fn-for-low low)
       (cond [(empty? low) empty]
             [else
              (...
               (fn-for-widget (first low))
               (fn-for-low (rest low)))]))]
    (fn-for-widget widget)))







(define (find-widget widget fn?)
  (local [(define (find-widget--widget widget)
            (if (fn? widget)
                (cons widget (find-widget--low (widget-parts widget)))
                (find-widget--low (widget-parts widget))))
          
          (define (find-widget--low low)
            (cond [(empty? low) empty]
                  [else
                   (append
                    (find-widget--widget (first low))
                    (find-widget--low (rest low)))]))]
    (find-widget--widget widget)))


(check-expect (find-widget Telephone (λ(widget)
                                       (= (string-length(widget-name widget)) 7)))
              (list Buttons Numbers))
(check-expect (find-widget Necklace (λ(widget)
                                      (> (widget-time widget) 2)))
              (list Necklace Pendant))
(check-expect (find-widget Glass (λ(widget)
                                   (> (widget-price widget) 30)))
              empty)







(define (find-widget-name-longer-than widget name-length)
  (find-widget widget (λ(widget) (> (string-length(widget-name widget)) name-length))))

(check-expect (find-widget-name-longer-than Telephone 5)
              (list Telephone Receiver Buttons Numbers))     
(check-expect (find-widget-name-longer-than Jewelry 6)
              (list Jewelry Necklace Pendant Bracelet))       
(check-expect (find-widget-name-longer-than Telephone 13)
              empty)








(define (find-widget-quantity-over widget quantity)
  (find-widget widget (λ (widget) (> (widget-quantity widget) quantity))))

(check-expect (find-widget-quantity-over Cord 5) (list Cord))
(check-expect (find-widget-quantity-over Telephone 8) (list Receiver Numbers))
(check-expect (find-widget-quantity-over Jewelry 9) (list Rings Necklace Beads))







(define (find-widgets-cheaper-than widget price)
  (find-widget widget (λ(widget) (< (widget-price widget) price))))

(check-expect (find-widgets-cheaper-than Numbers 4) empty)
(check-expect (find-widgets-cheaper-than Numbers 6) (list Numbers))
(check-expect (find-widgets-cheaper-than Telephone 6) (list Buttons Numbers Cord Wire))







(define (find-widget-hard-make widget quantity price)
  (find-widget widget (λ(widget)
                        (or (> (widget-quantity widget) quantity)
                            (< (widget-price widget) price)))))

(check-expect (find-widget-hard-make Telephone 8 6) (list Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget-hard-make Necklace 5 2) (list Necklace Chain Pendant))
(check-expect (find-widget-hard-make Bracelet 25 2) empty)








(define (list-widgets widget)
  
  (local [(define (list-widgets--widget widget)
            (cons widget (list-widgets--low (widget-parts widget))))
          
          (define (list-widgets--low low)
            (cond [(empty? low) empty]
                  [else
                   (append
                    (list-widgets--widget (first low))
                    (list-widgets--low (rest low)))]))]
    (list-widgets--widget widget)))
(check-expect (list-widgets Necklace)
              (list Necklace Chain Pendant))
(check-expect (list-widgets Telephone)
              (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (list Chain)
              (list Chain))








(define (qsort fn? widget-field)
  (λ(widget)
    (local [(define (lesser-widgets low widget)
              (filter (λ (wid) (fn? (widget-field wid) (widget-field widget))) low))

            (define (greater-widgets low widget)
              (filter (λ (wid) (not (fn? (widget-field wid) (widget-field widget)))) low))

            (define (qsort--low low) 
              (cond
                [(empty? low) empty]
                [else 
                 (local
                   [(define pivot (first low))]
                   (append
                    (qsort--low (lesser-widgets (rest low) pivot))
                    (list pivot)
                    (qsort--low (greater-widgets (rest low) pivot))))]))

            (define widgets (list-widgets widget))]
     
      (qsort--low widgets))))







(define sort-strings (qsort string<? widget-name))

(check-expect (sort-strings Telephone)
              (list Buttons Cord Numbers Receiver Telephone Wire))
(check-expect (sort-strings Beads)
              (list Beads Glass))
(check-expect (sort-strings Necklace)
              (list Chain Necklace Pendant))

(map widget-name (sort-strings Telephone))





(define sort-overstocked (qsort < widget-quantity))

(check-expect (sort-overstocked Telephone)
              (list Wire Telephone Cord Buttons Numbers Receiver))
(check-expect (sort-overstocked Chain)
              (list Chain))
(check-expect (sort-overstocked Necklace)
              (list Pendant Chain Necklace))

(map widget-name (sort-overstocked Necklace))
