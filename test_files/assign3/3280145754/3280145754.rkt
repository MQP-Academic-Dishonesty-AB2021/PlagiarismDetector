

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


(define (fn-for-widget aWidget)
  (local
    [(define (for-widget w)
       (...
        (widget-name w)
        (widget-quantity w)
        (widget-time w)
        (widget-price w)
        (fn-for-tree (widget-parts w))))

     (define (fn-for-tree t)
       (cond
         [(empty? t) ...]
         [else
          (...
           (for-widget (first t))
           (fn-for-tree (rest t)))]
         )
       )]
    (for-widget aWidget)
    )
  )










(check-expect (searchWidgets Glass
                             (λ (aWidget)(> (string-length (widget-name aWidget)) 7)))
              empty)
(check-expect (searchWidgets Beads
                             (λ (aWidget)(> (string-length (widget-name aWidget)) 2)))
              (list Beads Glass))
(check-expect (searchWidgets Jewelry
                             (λ (aWidget)(> (string-length (widget-name aWidget)) 5)))
              (list Jewelry Necklace Pendant Bracelet ))



(check-expect (searchWidgets Numbers
                             (λ(aWidget)(> (widget-quantity aWidget) 10)))
              empty)
(check-expect (searchWidgets Cord
                             (λ(aWidget)(> (widget-quantity aWidget) 2)))
              (list Cord Wire))
(check-expect (searchWidgets Jewelry
                             (λ(aWidget)(> (widget-quantity aWidget) 1)))
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))


(check-expect (searchWidgets Glass (λ (aWidget)(< (widget-price aWidget) 3)))
              empty)
(check-expect (searchWidgets Beads (λ (aWidget)(< (widget-price aWidget) 12)))
              (list Beads Glass))
(check-expect (searchWidgets Jewelry (λ (aWidget)(< (widget-price aWidget) 20)))
              (list  Rings Necklace Chain Pendant
                     Bracelet Beads Glass ))

(check-expect (searchWidgets Wire (λ (aWidget)
                                    (or (< (widget-quantity aWidget) 1)
                                        (> (widget-price aWidget) 10)
                                        )))
              empty) 
(check-expect (searchWidgets Cord (λ (aWidget)
                                    (or (< (widget-quantity aWidget) 1)
                                        (> (widget-price aWidget) 1)
                                        )))
              (list Cord Wire)) 
(check-expect (searchWidgets Necklace (λ (aWidget)
                                        (or (< (widget-quantity aWidget) 1)
                                            (> (widget-price aWidget) 1)
                                            )))
              (list Necklace)) 

(define (searchWidgets aWidget searchBy? )
  (local [
          (define (search-widget aWidget )
            (if (searchBy? aWidget)
                
                (cons aWidget (search-low (widget-parts aWidget) ))
                
                (search-low (widget-parts aWidget) )
                )
            )
          (define (search-low low )
            (cond
              [(empty? low) empty]
              [else
               (append
                (search-widget (first low) )
                (search-low (rest low) ))]
              )
            )
          ]
    (search-widget aWidget)
    )
  )





(check-expect (getWidgets  Glass) (list Glass))
(check-expect (getWidgets  Bracelet) (list Bracelet Beads Glass))
(check-expect (getWidgets  Jewelry)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass ))
(define (getWidgets awidget)
  (local[
         (define (get-all-widgets widget)
           (cons widget (get-all-widget-subs (widget-parts widget)))
           )
         (define (get-all-widget-subs low)
           (cond
             [(empty? low) empty]
             [else
              (append
               (get-all-widgets(first low))
               (get-all-widget-subs (rest low)))])
           )
         ]
    (get-all-widgets awidget)
    )
  )












(define (qsort fn? widgetfn)
  (λ (aWidget)
    (local [
            (define listOfWidgets (getWidgets aWidget))
            (define (false-branch low pivot)
              (cond [(empty? low) empty]
                    [(false?(fn? (widgetfn (first low)) (widgetfn pivot)))
                     (cons (first low) (false-branch (rest low) pivot))]
                    [else (false-branch (rest low)pivot)]
                    )
              )
            (define (true-branch low pivot)
              (cond [(empty? low) empty]
                    [(fn? (widgetfn (first low)) (widgetfn pivot))
                     (cons (first low) (true-branch (rest low) pivot))]
                    [else (true-branch (rest low) pivot)]
                    )
              )
            (define (realqsort low)
              
              (cond
                [(empty? low) empty]
                [else
                 (local[(define pivot (first low))]
                   (append
                    (realqsort (true-branch (rest low) pivot))
                    (list pivot)
                    (realqsort (false-branch (rest low) pivot))
                    )
                   )
                 ]
                )
                
              )
            ]
      (realqsort listOfWidgets)
      )
    )
  )





(define sort-strings (qsort string<? widget-name))

(check-expect (sort-strings Telephone)
              (list Buttons Cord Numbers Receiver Telephone Wire))

(map widget-name (sort-strings Telephone)) 

(define sort-overstocked (qsort > widget-quantity))

(check-expect (sort-overstocked Necklace)
              (list Necklace Chain Pendant))

(map widget-name (sort-overstocked Necklace))

(define sort-lessthan (qsort < widget-quantity))

(check-expect (sort-lessthan Necklace)
              (list Pendant Chain Necklace))

(map widget-name (sort-lessthan Necklace))
