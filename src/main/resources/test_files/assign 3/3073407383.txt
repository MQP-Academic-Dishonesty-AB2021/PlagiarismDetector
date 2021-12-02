

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


 




(define (widget-filter fn? wid)
  (local [(define (wid-fn wid)
            (if (fn? wid) 
                (cons wid (fn-for-low (widget-parts wid)))
                (fn-for-low (widget-parts wid))))
          
          (define (fn-for-low low)
            (cond [(empty? low) empty]
                  [else
                   (append (wid-fn(first low))
                           (fn-for-low(rest low)))]))]
    (wid-fn wid)))




(define (find-widget-name-longer-than wid length)
  (widget-filter (λ (w) (> (string-length (widget-name w)) length)) wid))

(check-expect (find-widget-name-longer-than Jewelry 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect (find-widget-name-longer-than Telephone 7) (list Telephone Receiver))





(define (find-widget-quantity-over wid quantity)
  (widget-filter (λ (w) (> (widget-quantity w) quantity)) wid))

(check-expect (find-widget-quantity-over Telephone 6)
                 (list Receiver Buttons Numbers Cord))
(check-expect (find-widget-quantity-over Jewelry 9) (list Rings Necklace Beads))





(define (find-widget-cheaper-than wid price)
  (widget-filter (λ (w) (< (widget-price w) price)) wid))

(check-expect (find-widget-cheaper-than Telephone 6)
                 (list Buttons Numbers Cord Wire))
(check-expect (find-widget-cheaper-than Jewelry 6)
              (list Necklace Chain Pendant Bracelet Glass))








(define (find-widget-hard-make wid quantity price) 
  (local [(define (is-hard? wid)
          (or (> (widget-price wid) price) (< (widget-quantity wid) quantity)))]
    (widget-filter is-hard? wid)))

(check-expect (find-widget-hard-make Telephone 6 6)
              (list Telephone Receiver Wire))
(check-expect (find-widget-hard-make Jewelry 6 15)
              (list Jewelry Pendant Bracelet))






(define (qsort fn? field)
  (λ (widget)
    (local [
      (define (all-subwidgets wid)
        (cons wid (foldr append empty (map all-subwidgets (widget-parts wid)))))
      (define (qsort-inner low)
        (if (empty? low) empty
         (local [(define pivot (first low))
            (define (compare? wid) (fn? (field wid) (field pivot)))]
               (append
                  (qsort-inner (filter compare? (rest low)))
                  (list pivot)
                  (qsort-inner (filter (λ(w) (not (compare? w))) (rest low)))))))]
    (qsort-inner (all-subwidgets widget))))) 




(define sort-strings (qsort string<? widget-name))

(check-expect  (map widget-name (sort-strings Telephone))
   (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))
(check-expect  (map widget-name (sort-strings Glass))
   (list "Glass"))





(define sort-overstocked (qsort > widget-quantity))

(check-expect (map widget-name (sort-overstocked Necklace))
   (list "Necklace" "Chain" "Pendant"))
(check-expect (map widget-name (sort-overstocked Glass))
   (list "Glass"))
