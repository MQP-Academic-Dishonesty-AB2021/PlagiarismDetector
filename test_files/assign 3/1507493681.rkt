

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





 



(define (filter-widget fn? wid)
  (local [(define (fn-widget wid)
            (if (fn? wid)
                (cons wid (fn-low (widget-parts wid)))
                (fn-low (widget-parts wid))))
          (define (fn-low low)
            (if (empty? low) empty
                (append (fn-widget (first low))
                        (fn-low (rest low)))))]
    (fn-widget wid)))







(define (find-widget-name-longer-than wid cutoff)
  (filter-widget (λ (wid) (> (string-length (widget-name wid)) cutoff)) wid))

(check-expect (find-widget-name-longer-than Wire 0) (list Wire))
(check-expect (find-widget-name-longer-than Wire 10) empty)
(check-expect (find-widget-name-longer-than Jewelry 6) (list Jewelry Necklace Pendant Bracelet))






(define (find-widget-quantity-over wid cutoff)
  (filter-widget (λ (wid) (> (widget-quantity wid) cutoff)) wid))

(check-expect (find-widget-quantity-over Wire 4) empty)
(check-expect (find-widget-quantity-over Wire 2) (list Wire))
(check-expect (find-widget-quantity-over Jewelry 6) (list Rings Necklace Chain Beads))





(define (find-widgets-cheaper-than wid cutoff)
  (filter-widget (λ (wid) (< (widget-price wid) cutoff)) wid))

(check-expect (find-widgets-cheaper-than Wire 6) (list Wire))
(check-expect (find-widgets-cheaper-than Wire 4) empty)
(check-expect (find-widgets-cheaper-than Jewelry 10) (list Necklace Chain Pendant Bracelet Beads Glass))





(define (find-widget-hard-make wid cutoff)
  (filter-widget (λ (wid) (or (< (widget-quantity wid) cutoff) (> (widget-price wid) cutoff))) wid))

(check-expect (find-widget-hard-make Cord 5) (list Wire))
(check-expect (find-widget-hard-make (make-widget "anonymas" 100 5 0 empty) 4) empty)
(check-expect (find-widget-hard-make Jewelry 10) (list Jewelry Rings Chain Pendant Bracelet Glass))







(define (qsort fn? select)
  (local [(define (fn-widget wid)
            (cons wid (fn-low (widget-parts wid))))
          (define (fn-low low)
            (if (empty? low) empty
                (append (fn-widget (first low))
                        (fn-low (rest low)))))]
    (λ (wid) (quicksort (fn-widget wid) (λ (w1 w2) (fn? (select w1) (select w2)))))))



(define sort-strings (qsort string<? widget-name))
(check-expect (map widget-name (sort-strings Telephone)) (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))



(define sort-overstocked (qsort > widget-quantity))
(check-expect (map widget-name (sort-overstocked Necklace)) (list "Necklace" "Chain" "Pendant"))



(define sort-expense (qsort > widget-price))
(check-expect (map widget-name (sort-expense Jewelry)) (list "Jewelry set" "Rings" "Beads" "Bracelet" "Glass" "Necklace" "Chain" "Pendant"))