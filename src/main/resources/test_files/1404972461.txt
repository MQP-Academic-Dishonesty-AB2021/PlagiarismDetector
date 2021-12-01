

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



 







(define (filter-subwidgets fn? widget)
  (local
    [(define (filter-subwidgets--inner widget)
       (if (fn? widget)
           (cons widget
                 (filter-low--inner (widget-parts widget)))
           (filter-low--inner (widget-parts widget))))
     (define (filter-low--inner low)
       (cond
         [(empty? low) empty]
         [else
          (append
           (filter-subwidgets--inner (first low))
           (filter-low--inner (rest low)))]))]
    (filter-subwidgets--inner widget)))




(define (find-widget-name-longer-than widget limit)
  (filter-subwidgets (lambda (widget)
                       (> (string-length (widget-name widget)) limit)) widget))




(check-expect (find-widget-name-longer-than Telephone 7) 
              (list Telephone Receiver))


(check-expect (find-widget-name-longer-than Telephone 9) 
              empty)


(check-expect (find-widget-name-longer-than Wire 2) 
              (list Wire))


(check-expect (find-widget-name-longer-than Numbers 10) 
              empty)





(define (find-widget-quantity-over widget limit)
  (filter-subwidgets (lambda (widget)
                       (> (widget-quantity widget) limit)) widget))



(check-expect (find-widget-quantity-over Telephone 7) 
              (list Receiver Buttons Numbers))


(check-expect (find-widget-quantity-over Telephone 15) 
              empty)


(check-expect (find-widget-quantity-over Wire 2) 
              (list Wire))


(check-expect (find-widget-quantity-over Numbers 10) 
              empty)





(define (find-widgets-cheaper-than widget limit)
  (filter-subwidgets (lambda (widget) (< (widget-price widget) limit)) widget))



(check-expect (find-widgets-cheaper-than Telephone 6)
              (list Buttons Numbers Cord Wire))


(check-expect (find-widgets-cheaper-than Telephone 3)
              empty)


(check-expect (find-widgets-cheaper-than Wire 6) 
              (list Wire))


(check-expect (find-widgets-cheaper-than Numbers 2) 
              empty)





(define (find-widget-hard-make widget stock cost)
  (filter-subwidgets (lambda (widget)
                       (or (< (widget-quantity widget) stock)
                           (> (widget-price widget) cost))) widget))




(check-expect (find-widget-hard-make Telephone 6 20) 
              (list Telephone Wire))


(check-expect (find-widget-hard-make Telephone 2 20) 
              empty)


(check-expect (find-widget-hard-make Wire 2 2) 
              (list Wire))


(check-expect (find-widget-hard-make Numbers 7 10) 
              empty)



              



(define (qsort sort-fn? map-fn) 
  (local
    [(define (flatten-widget widget)
       (cons widget
             (foldr (λ (widget low)
                      (append (flatten-widget widget) low))
                    empty (widget-parts widget))))
     (define (qsort--inner lox)
       (cond
         [(empty? lox) empty]
         [else 
          (local
            [(define pivot (first lox))] 
            (append
             (qsort--inner (filter (λ (widget)
                                     (sort-fn? (map-fn widget)
                                               (map-fn pivot)))
                                   (rest lox))) 
             (list pivot)
             (qsort--inner (filter (λ (widget)
                                     (not (sort-fn? (map-fn widget)
                                                    (map-fn pivot))))
                                   (rest lox)))))]))]
    (lambda (widget)
      (qsort--inner (flatten-widget widget)))))



(define sort-strings (qsort string<? widget-name))

(check-expect (sort-strings Telephone)
              (list Buttons Cord Numbers Receiver Telephone Wire)) 
(check-expect (sort-strings Wire) (list Wire)) 
(check-expect (sort-strings Beads) (list Beads Glass)) 



(define sort-overstocked (qsort > widget-quantity))


(check-expect (sort-overstocked Necklace) (list Necklace Chain Pendant))


(check-expect (sort-overstocked Buttons) (list Numbers Buttons))


(check-expect (sort-overstocked Wire) (list Wire))