

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment3Part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

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







 






(define (find-widget widg fn?)
  (local [(define (fn-for-widget--widget w)
            (if (fn? w)
                (cons w 
                   (fn-for-widget--low (widget-parts w)))
                (fn-for-widget--low (widget-parts w)))
                )
          (define (fn-for-widget--low low)
            (cond [(empty? low) empty]
                  [else
                   (append (fn-for-widget--widget (first low))
                        (fn-for-widget--low (rest low)))]))]
    [fn-for-widget--widget widg]))

(check-expect (find-widget Telephone (lambda (w) false)) empty) 
(check-expect (find-widget Bracelet (lambda (w) true))
              (list Bracelet Beads Glass)) 
(check-expect (find-widget Jewelry (lambda (w)
                                     (> (string-length
                                         (widget-name w))
                                        (widget-price w))))
              (list Necklace Chain Pendant Bracelet Glass)) 
(check-expect (find-widget Receiver (lambda (w)
                                      (string=? (widget-name w) "Receiver")))
              (list Receiver)) 






(check-expect (find-widget-name-longer-than Rings 5) empty)

(check-expect (find-widget-name-longer-than Chain 0) (list Chain))

(check-expect (find-widget-name-longer-than Buttons 4) (list Buttons Numbers)) 
(check-expect (find-widget-name-longer-than Telephone 5)
              (list Telephone Receiver Buttons Numbers)) 
 


(define (find-widget-name-longer-than widget length)
  (find-widget widget (lambda (w)
                        (> (string-length (widget-name w))
                           length)))
)

  







(check-expect (find-widget-quantity-over Rings 25) empty)

(check-expect (find-widget-quantity-over Chain 0) (list Chain))

(check-expect (find-widget-quantity-over Buttons 4) (list Buttons Numbers)) 
(check-expect (find-widget-quantity-over Telephone 8) (list Receiver Numbers)) 
 



(define (find-widget-quantity-over widget quantity)
  (find-widget widget (lambda (w)
                        (> (widget-quantity w)
                           quantity)))
)










(check-expect (find-widget-cheaper-than Rings 0) empty)

(check-expect (find-widget-cheaper-than Chain 20) (list Chain)) 
(check-expect (find-widget-cheaper-than Buttons 5) empty)

(check-expect (find-widget-cheaper-than Telephone 20)
              (list Telephone Receiver Buttons Numbers Cord Wire)) 



(define (find-widget-cheaper-than widget price)
  (find-widget widget (lambda (w)
                        (< (widget-price w)
                           price)))
)







(check-expect (find-widget-hard-make Rings 15 11) empty)

(check-expect (find-widget-hard-make Chain 6 0) (list Chain))

(check-expect (find-widget-hard-make Telephone 3 3)
              (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (find-widget-hard-make Bracelet 6 6) (list Bracelet Beads)) 
 



(define (find-widget-hard-make widget quantity price)
  (find-widget widget (lambda (w)
                        (or
                         (> (widget-price w) price)
                         (< (widget-quantity w) quantity)
                         )))
)








(define (qsort fn? get-data)
  (local [(define (get-list--widget w) 
            (cons w 
               (get-list--low (widget-parts w))))
          (define (get-list--low low)
            (cond [(empty? low) empty]
                  [else
                   (append (get-list--widget (first low))
                        (get-list--low (rest low)))]))
          (define (sort-list low)
            (cond [(empty? low) empty]
                  [else
                   (local [
                           (define pivot (first low))
                           (define (smaller? w)
                             (fn? (get-data w) (get-data pivot)))]
                     [append (sort-list (filter smaller? (rest low)))
                             (list pivot)
                             (sort-list (filter (lambda (w)
                                                  (not (smaller? w))) (rest low)))
                             ]
                     )
                   ]
                  )
            )
          ]
    [lambda (widg) (
            sort-list (get-list--widget widg)
                    )]))

(define dumb-sort (qsort (λ(w1 w2) false) identity)) 
(check-expect (dumb-sort Telephone) (list Telephone Receiver Buttons Numbers Cord Wire))
(define reverse-sort (qsort (λ(w1 w2) true) identity)) 
(check-expect (reverse-sort Telephone) (list  Wire Cord Numbers Buttons Receiver Telephone))

(define sort-strings (qsort string<? widget-name)) 
(check-expect (sort-strings Telephone) (list Buttons Cord Numbers Receiver Telephone Wire))
(define sort-overstocked (qsort > widget-quantity))
(check-expect (sort-overstocked Necklace) (list Necklace Chain Pendant))
