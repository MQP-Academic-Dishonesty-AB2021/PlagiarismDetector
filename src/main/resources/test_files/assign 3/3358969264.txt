

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





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
  (local [(define (fn-for-widget--widget widget)
            (... (widget-name widget)
            (widget-quantity widget)
            (widget-time widget)
            (widget-price widget)
            (fn-for-widget--low (widget-parts widget))))
          (define (fn-for-widget--low low)
            (cond [(empty? low) (...)]
            [else (...
                 (fn-for-widget--widget (first low))
                 (fn-for-widget--low (rest low)))]))]
    (fn-for-widget--widget widget)))


(check-expect (higher-order Wire (lambda
                                  (widget) 
                                  (> (string-length (widget-name Wire)) 1) )
                                  1
                                  -1) (list Wire))
                                  
(check-expect (higher-order Wire (lambda
                                  (widget) 
                                  (> (widget-quantity Wire) 2))
                                  2
                                  -1) (list Wire))

(check-expect (higher-order Telephone (lambda
                                 (widget) 
                                 (or (< (widget-quantity Telephone) 5) (> (widget-price Telephone) 5)) )
                                 5
                                 5) (list Telephone Receiver Buttons Numbers Cord Wire))
                                  


(define (higher-order widget comparison? param1 param2)
  (local [(define (higher-order--widget widget comparison? param1 param2)
            (if (comparison? widget)
                (cons widget (higher-order--low (widget-parts widget) param1 param2))
                (higher-order--low (widget-parts widget) param1 param2)))
          (define (higher-order--low low param1 param2)
            (cond [(empty? low) empty]
                  [else (append
                         (higher-order--widget (first low) comparison? param1 param2)
                         (higher-order--low (rest low) param1 param2))]))]
    (higher-order--widget widget comparison? param1 param2)))


(check-expect (find-widget-name-longer-than Wire 1) (list Wire))
(check-expect (find-widget-name-longer-than Bracelet 7) (list Bracelet))
(check-expect (find-widget-name-longer-than Bracelet 4) (list Bracelet Beads Glass))




(define (find-widget-name-longer-than widget length)
  (local [(define (comparison? widget)
            (> (string-length (widget-name widget)) length))]
    (higher-order widget comparison? length -1)))


(check-expect (find-widget-quantity-over Wire 1) (list Wire))
(check-expect (find-widget-quantity-over Bracelet 7) (list Beads))
(check-expect (find-widget-quantity-over Bracelet 4) (list Bracelet Beads Glass))


(define (find-widget-quantity-over widget quantity)

  (local [(define (comparison? widget)
            (> (widget-quantity widget) quantity))]
    (higher-order widget comparison? quantity -1))
  )

(check-expect (find-widgets-cheaper-than Bracelet 6) (list Bracelet Glass))
(check-expect (find-widgets-cheaper-than Bracelet 0) empty) 
(check-expect (find-widgets-cheaper-than Jewelry 6) (list Necklace Chain Pendant Bracelet Glass))




(define (find-widgets-cheaper-than widget amount)

  (local [(define (comparison? widget)
            (< (widget-price widget) amount))]
    (higher-order widget comparison? amount -1))
  )

(check-expect (find-widget-hard-make Telephone 5 5) (list Telephone Receiver Wire))
(check-expect (find-widget-hard-make Wire 0 999) empty) 
(check-expect (find-widget-hard-make Necklace 20 2) (list Necklace Chain Pendant))




(define (find-widget-hard-make widget quantity amount)
  (local [(define (quantity-less-or-cost-greater? widget quantity amount)
            (or (< (widget-quantity widget) quantity) (> (widget-price widget) amount)))

          (define (comparison? widget)
            (quantity-less-or-cost-greater? widget quantity amount))]
    (higher-order widget comparison? quantity amount)))





(define (qsort comparison? widget-field)
  (lambda (widget)
    (local [(define low (higher-order widget (lambda (widget) true) -1 -1))
            (define (qsort-inner low comparison? widget-field)
              (cond [(empty? low) empty]
                    [else 
                     (local
                       [(define pivot (first low))
                        (define (smaller? widget) (comparison? (widget-field widget) (widget-field pivot)))]
                       (append
                        (qsort-inner (filter smaller? (rest low)) comparison? widget-field)
                        (list pivot)
                        (qsort-inner 
                         (filter 
                          (lambda (widget)
                            (not (smaller? widget)))
                          (rest low)) comparison? widget-field)))]))]
      (qsort-inner low comparison? widget-field))
    )
  )

(check-expect ((qsort string<? widget-name) Necklace) (list Chain Necklace Pendant)) 
(check-expect ((qsort < widget-price) Jewelry) (list Chain Pendant Necklace Glass Bracelet Beads Rings Jewelry)) 
(check-expect ((qsort > widget-price) Jewelry) (list Jewelry Rings Beads Bracelet Glass Necklace Chain Pendant)) 
