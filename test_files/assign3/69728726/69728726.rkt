

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Kai Nakamura and Keaton Mangone Assignment 3 Part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))















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









(define (filter-widget condition? widget)
  (local [(define (filter-widget--low low)
            (cond [(empty? low) empty]
                  [else
                   (append (filter-widget condition? (first low))
                           (filter-widget--low (rest low)))]))
          (define filtered-parts (filter-widget--low (widget-parts widget)))]
    (if (condition? widget)
        (cons widget filtered-parts)
        filtered-parts)))








(check-expect (find-widget-longer-than Wire 2) (list Wire))

(check-expect (find-widget-longer-than Wire 4) empty)

(check-expect (find-widget-longer-than Telephone 4)
              (list Telephone Receiver Buttons Numbers))

(define (find-widget-longer-than widget length)
  (filter-widget (lambda (widget)
                   (> (string-length (widget-name widget)) length))
                 widget))








(check-expect (find-widget-quantity-over Wire 2) (list Wire))

(check-expect (find-widget-quantity-over Wire 3) empty)

(check-expect (find-widget-quantity-over Telephone 7)
              (list Receiver Buttons Numbers))

(check-expect (find-widget-quantity-over Telephone 0)
              (list Telephone Receiver Buttons Numbers Cord Wire))

(check-expect (find-widget-quantity-over Jewelry 10) (list Rings Beads))

(define (find-widget-quantity-over widget quantity)
  (filter-widget (lambda (widget)
                   (> (widget-quantity widget) quantity))
                 widget))







(check-expect (find-widgets-cheaper-than Wire 100) (list Wire))

(check-expect (find-widgets-cheaper-than Wire 5) empty)

(check-expect (find-widgets-cheaper-than Wire 6) (list Wire))

(check-expect (find-widgets-cheaper-than Jewelry 10)
              (list Necklace Chain Pendant Bracelet Beads Glass))

(define (find-widgets-cheaper-than widget price)
  (filter-widget (lambda (widget)
                   (< (widget-price widget) price))
                 widget))








(check-expect (find-widget-hard-make Wire 4 4) (list Wire))

(check-expect (find-widget-hard-make Wire 3 4) (list Wire))

(check-expect (find-widget-hard-make Wire 4 5) (list Wire))

(check-expect (find-widget-hard-make Wire 3 5) empty)

(check-expect (find-widget-hard-make Telephone 8 5)
              (list Telephone Receiver Cord Wire))

(check-expect (find-widget-hard-make Jewelry 8 10)
              (list Jewelry Rings Chain Pendant Bracelet Glass))

(define (find-widget-hard-make widget quantity price)
  (filter-widget (lambda (widget)
                   (or (< (widget-quantity widget) quantity)
                       (> (widget-price widget) price)))
                 widget))






(check-expect (map widget-name (subwidgets Wire))
              (list "Wire"))

(check-expect (map widget-name (subwidgets Cord))
              (list "Cord" "Wire"))

(check-expect (map widget-name (subwidgets Telephone))
              (list "Telephone" "Receiver" "Buttons" "Numbers" "Cord" "Wire"))

(define (subwidgets widget)
  (local [(define (subwidgets--widget widget)
            (cons widget
                  (subwidgets--low (widget-parts widget))))
          (define (subwidgets--low widgets)
            (cond [(empty? widgets) empty]
                  [else
                   (append (subwidgets--widget (first widgets))
                           (subwidgets--low (rest widgets)))]))]
    (subwidgets--widget widget)))







(check-expect (map widget-name ((qsort < widget-price) Wire))
              (list "Wire"))

(check-expect (map widget-name ((qsort string<? widget-name) Telephone))
              (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))

(check-expect (map widget-name ((qsort > widget-quantity) Necklace))
              (list "Necklace" "Chain" "Pendant"))

(define (qsort fn? widget-fn)
  (local [
          
          
          
          (define (qsort--widget widget)
            (qsort--low (subwidgets widget)))
          
          
          
          (define (qsort--low low)
            (cond [(empty? low) empty]
                  [else
                   (local
                     [(define pivot (first low))
                      (define (comes-before? n) (fn? (widget-fn n)
                                                     (widget-fn pivot)))]
                     (append
                      (qsort--low (filter comes-before? (rest low)))   
                      (list pivot)
                      (qsort--low
                       (filter (λ(n)
                                 (not (comes-before? n)))
                               (rest low)))))]))]
    qsort--widget))






(check-expect (map widget-name (sort-strings Wire))
              (list "Wire"))

(check-expect (map widget-name (sort-strings Telephone))
              (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))

(define sort-strings (qsort string<? widget-name))







(check-expect (map widget-name (sort-overstocked Wire))
              (list "Wire"))

(check-expect (map widget-name (sort-overstocked Necklace))
              (list "Necklace" "Chain" "Pendant"))

(define sort-overstocked (qsort > widget-quantity))