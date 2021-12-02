

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


 





(define (general-fn awidget fn comp-widget)
  (local [(define (general-fn--widget awidget fn comp-widget)
            (cond [(fn awidget comp-widget)
                   (cons awidget
                         (general-fn--low (widget-parts awidget)
                                          fn comp-widget))]
                  [else (general-fn--low (widget-parts awidget)
                                         fn comp-widget)]))

          (define (general-fn--low alow fn comp-widget)
            (cond [(empty? alow) empty]
                  [else (append (general-fn--widget (first alow)
                                                    fn comp-widget)
                                (general-fn--low (rest alow)
                                                 fn comp-widget))]))]
    (general-fn--widget awidget fn comp-widget)))






(define (find-widget-name-longer-than awidget length)
  (general-fn awidget
              (λ (awidget comp-widget)
                (> (string-length (widget-name awidget))
                   (string-length (widget-name comp-widget))))
              (make-widget (make-string length #\a) 0 0 0 empty)))

(check-expect (find-widget-name-longer-than Wire 10) empty)

(check-expect (find-widget-name-longer-than Wire 2) (list Wire))

(check-expect (find-widget-name-longer-than Wire 4) empty)


(check-expect (find-widget-name-longer-than Cord 2) (list Cord Wire))

(check-expect (find-widget-name-longer-than Telephone 4)
              (list Telephone Receiver Buttons Numbers))
(check-expect (find-widget-name-longer-than Telephone 3)
              (list Telephone Receiver Buttons Numbers Cord Wire))






(define (find-widget-quantity-over awidget quant)
  (general-fn awidget
              (λ (awidget comp-widget)
                (> (widget-quantity awidget) (widget-quantity comp-widget)))
              (make-widget "" quant 0 0 empty)))

(check-expect (find-widget-quantity-over Wire 10) empty)

(check-expect (find-widget-quantity-over Wire 2) (list Wire))

(check-expect (find-widget-quantity-over Wire 3) empty)


(check-expect (find-widget-quantity-over Cord 1) (list Cord Wire))

(check-expect (find-widget-quantity-over Telephone 1)
              (list Telephone Receiver Buttons Numbers Cord Wire))







(define (find-widgets-cheaper-than awidget cost)
  (general-fn awidget
              (λ (awidget comp-widget)
                (< (widget-price awidget) (widget-price comp-widget)))
              (make-widget "" 0 0 cost empty)))

(check-expect (find-widgets-cheaper-than Wire 2) empty)

(check-expect (find-widgets-cheaper-than Wire 10) (list Wire))

(check-expect (find-widgets-cheaper-than Wire 5) empty)


(check-expect (find-widgets-cheaper-than Cord 10) (list Cord Wire))

(check-expect (find-widgets-cheaper-than Telephone 10)
              (list Receiver Buttons Numbers Cord Wire))









(define (find-widget-hard-make awidget quant cost)
  (general-fn awidget
              (λ (awidget comp-widget)
                (or (< (widget-quantity awidget) (widget-quantity comp-widget))
                    (> (widget-price awidget) (widget-price comp-widget))))
              (make-widget "" quant 0 cost empty)))

(check-expect (find-widget-hard-make Wire 1 10) empty)

(check-expect (find-widget-hard-make Wire 20 10) (list Wire))

(check-expect (find-widget-hard-make Wire 1 1) (list Wire))

(check-expect (find-widget-hard-make Wire 3 10) empty)

(check-expect (find-widget-hard-make Wire 1 5) empty)

(check-expect (find-widget-hard-make Wire 3 5) empty)


(check-expect (find-widget-hard-make Telephone 7 6)
              (list Telephone Receiver Wire))

(check-expect (find-widget-hard-make Telephone 20 0)
              (list Telephone Receiver Buttons Numbers Cord Wire))

(check-expect (find-widget-hard-make Telephone 0 20) empty)








(define (qsort fn parameter)
  (local [(define (sort-fn low)
            (cond [(empty? low) empty]
                  [else (local
                          [(define pivot (first low))
                           (define (compare-widgets W1)
                             (fn (parameter W1) (parameter pivot)))]
                          (append (sort-fn (filter compare-widgets (rest low)))
                                  (list pivot)
                                  (sort-fn (filter
                                            (λ (low)
                                              (not (compare-widgets low)))
                                            (rest low)))))]))
          (define (make-widget-list awidget)
            (local [(define (fn-for-widget--widget awidget)
                      (cons awidget (fn-for-widget--low (widget-parts awidget))))

                    (define (fn-for-widget--low alow)
                      (cond [(empty? alow) empty]
                            [else (append (fn-for-widget--widget (first alow))
                                          (fn-for-widget--low (rest alow)))]))]
              (fn-for-widget--widget awidget)))]
    (λ (awidget) (sort-fn (make-widget-list awidget)))))




(define sort-strings (qsort string<? widget-name))


(define sort-overstocked (qsort > widget-quantity))

(check-expect (sort-strings Telephone)
              (list Buttons Cord Numbers Receiver Telephone Wire))
(check-expect (map widget-name (sort-overstocked Necklace))
              (list "Necklace" "Chain" "Pendant"))







 






