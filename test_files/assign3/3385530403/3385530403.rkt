

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part2_DanielB_AlexM) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity time price parts))

 


(define (widget-fn-helper widget fn?)
  (local [(define (fn-for-list-of-widget low)
            (cond [(empty? low) empty]
                  [else  (append (widget-helper (first low))
                                 (fn-for-list-of-widget (rest low)))
                         ]
                  )
            )
          (define (widget-helper widget)
            (if (fn? widget)
                (cons widget (fn-for-list-of-widget (widget-parts widget)))
                (fn-for-list-of-widget (widget-parts widget))
                    
                )
            )
          ]
    (widget-helper widget)
    )
  )

(define (find-widget-name-longer-than widget n)
  (widget-fn-helper widget (λ (widget)
                             (> (string-length (widget-name widget)) n)
                             ))
  )
(define (find-widget-quantity-over widget n)
  (widget-fn-helper widget (λ (widget)
                             (> (widget-quantity widget) n))
                    )
  )


(define (find-widgets-cheaper-than widget n)
  (widget-fn-helper widget (λ (widget)
                             (< (widget-price widget) n))
                    )
  )
(define (find-widgets-hard-make widget n m)
  (widget-fn-helper widget (λ (widget)
                             (or (> (widget-price widget) m)
                                 (< (widget-quantity widget) n))
                             )
                    )
  )











(check-expect (find-widget-name-longer-than Wire 3) (list Wire)) 
(check-expect (find-widget-name-longer-than Wire 4) empty) 
(check-expect (find-widget-name-longer-than Telephone 6) (list Telephone Receiver Buttons Numbers)) 
(check-expect (find-widget-name-longer-than emptyWidget 1) empty) 

(check-expect (find-widget-quantity-over Wire 2) (list Wire)) 
(check-expect (find-widget-quantity-over Wire 3) empty) 
(check-expect (find-widget-quantity-over Telephone 4) (list Telephone Receiver Buttons Numbers Cord)) 
(check-expect (find-widget-quantity-over emptyWidget 1) empty) 

(check-expect (find-widgets-cheaper-than Wire 6) (list Wire)) 
(check-expect (find-widgets-cheaper-than Wire 5) empty) 
(check-expect (find-widgets-cheaper-than Telephone 6) (list Buttons Numbers Cord Wire)) 
(check-expect (find-widgets-cheaper-than emptyWidget 0) empty) 

(check-expect (find-widgets-hard-make Wire 8 4) (list Wire)) 
(check-expect (find-widgets-hard-make Wire 3 5) empty) 
(check-expect (find-widgets-hard-make Telephone 10 6) (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widgets-hard-make emptyWidget 1 0) (list emptyWidget)) 




(define emptyWidget (make-widget "" 0 0 0 empty))
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




(define (qsort comparator widget--field)
  (λ (widget)
    (local [(define (quicksort low)
              (cond [(empty? low) empty] 
                    [(= (length low) 1) low]
                    [else
                     (local [(define pivot (first low))
                             (define (larger-than low) 
                               (filter-2 (λ (x y)
                                           (not (comparator x y))) low (widget--field pivot)
                                                                   )
                               )
                             (define (smaller-than low) 
                               (filter-2 comparator low (widget--field pivot))
                               )
                             ]
                       (append (quicksort (smaller-than (rest low))) (list pivot) (quicksort (larger-than (rest low))))
                       )
                     ]
                    )
              )
            (define (filter-2 fn? lox x)
              (cond [(empty? lox) empty]
                    [else
                     (if (fn? (widget--field (first lox)) x)
                         (cons (first lox) (filter-2 fn? (rest lox) x))
                         (filter-2 fn? (rest lox) x)
                         )
                     ]
                    )
              )
            (define (tree-to-list widget)
              (if (empty? (widget-parts widget))
                  (cons widget empty)
                  (cons widget
                        (low-helper (widget-parts widget)))
                  )
              )
            (define (low-helper low) 
              (cond [(empty? low) empty]
                    [else (append (tree-to-list (first low)) (low-helper (rest low)))]
                    ))
            ]
      (quicksort (tree-to-list widget))
      )
    )
  )




(define sort-strings (qsort string<? widget-name))
(map widget-name (sort-strings Jewelry))
(define sort-costs (qsort < widget-price))
(map widget-name (sort-costs Jewelry))


















