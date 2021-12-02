

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(define-struct widget(name quantity time price parts))


(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 5 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 5 empty))
(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
(define Receiver (make-widget "Receiver" 10 5 7 empty))

(define Telephone (make-widget "Telephone" 5 20 15 (list Receiver Buttons Cord)))


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



 









(define (find-widget operation wdgt cut cut2)
  (local [(define (widget-cutoff operation wdgt cut cut2)
            (cond
              [(and (string=? operation "hard-make") (or (< (widget-quantity wdgt) cut) (> (widget-price wdgt) cut2))) true]
              [(and (string=? operation "cheaper-than") (< (widget-price wdgt) cut)) true]
              [(and (string=? operation "quantity-over") (> (widget-quantity wdgt) cut)) true]
              [(and (string=? operation "name-longer-than") (> (string-length (widget-name wdgt)) cut)) true]
              [else false]
              )
            )
          (define (make-list widget)
                      (local [(define (make-list-widget widget)
                                (cons widget (make-list-low (widget-parts widget))))
                              (define (make-list-low low)
                                (cond
                                  [(empty? low) empty]
                                  [else (append (make-list-widget (first low)) (make-list-low (rest low)))]
                                  )
                                )
                               
                              ]
                        (make-list-widget widget))
                    )
          ]

    (filter (lambda (wid) (widget-cutoff operation wid cut cut2)) (make-list wdgt))
      )
    )

(check-expect (find-widget "hard-make" Wire 4 4) (list Wire))
(check-expect (find-widget "hard-make" Wire 3 4) (list Wire))
(check-expect (find-widget "hard-make" Wire 4 5) (list Wire))
(check-expect (find-widget "hard-make" Wire 3 5) empty)
(check-expect (find-widget "hard-make" Telephone 9 8) (list Telephone Buttons Cord Wire))
(check-expect (find-widget "hard-make" Telephone 99 0) (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget "hard-make" Telephone 0 99) empty)
(check-expect (find-widget "cheaper-than" Telephone 6 0) (list Buttons Numbers Cord Wire))
(check-expect (find-widget "cheaper-than" Numbers 10 0) (list Numbers))
(check-expect (find-widget "cheaper-than" Numbers 99 0) (list Numbers))
(check-expect (find-widget "cheaper-than" Numbers 0 0) empty)
(check-expect (find-widget "cheaper-than" Telephone 16 0) (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget "quantity-over" Telephone 0 0) (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget "quantity-over" Numbers 0 0) (list Numbers))
(check-expect (find-widget "quantity-over" Numbers 3 0) (list Numbers))
(check-expect (find-widget "quantity-over" Telephone 5 0) (list Receiver Buttons Numbers Cord))
(check-expect (find-widget "name-longer-than" Telephone 0 0) (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget "name-longer-than" Numbers 0 0) (list Numbers))
(check-expect (find-widget "name-longer-than" Numbers 3 0) (list Numbers))
(check-expect (find-widget "name-longer-than" Telephone 5 0) (list Telephone Receiver Buttons Numbers))


(define (qsort operation wQual)
  (local [(define (sort widget)    
            (local [(define (sort-low low)
                      (cond
                        [(empty? low) empty]
                        [else
                         (local [(define pivot (first low))]
                           (append (sort-low (filter (lambda (w) (operation (wQual w) (wQual pivot))) (rest low))) (list pivot) (sort-low(filter (lambda (w) (not(operation (wQual w) (wQual pivot)))) (rest low))))
                           )
                         ]
                        )
                      )
                    (define (make-list widget)
                      (local [(define (make-list-widget widget)
                                (cons widget (make-list-low (widget-parts widget))))
                              (define (make-list-low low)
                                (cond
                                  [(empty? low) empty]
                                  [else (append (make-list-widget (first low)) (make-list-low (rest low)))]
                                  )
                                )
                               
                              ]
                        (make-list-widget widget))
                    )
                    ]
              (sort-low (make-list widget))
              )
            
            )
          ]
    sort))

(define sort-strings (qsort string<? widget-name))
(define sort-overstocked (qsort > widget-quantity))

(check-expect (map widget-name (sort-strings Telephone)) (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))
(check-expect (map widget-name (sort-overstocked Necklace)) (list "Necklace" "Chain" "Pendant"))



