

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 3 Pt 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))









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




 






(check-expect (find-widget Cord (λ(w) (not (widget? w)))) empty) 
(check-expect (find-widget Bracelet (λ(w) (> (string-length (widget-name w)) 5)))
              (list Bracelet)) 
(check-expect (find-widget Bracelet (λ(w) (> (widget-quantity w) 1)))
              (list Bracelet Beads Glass)) 

(define (find-widget widget test?)
  (local [(define (fn-for-widget widget)
            (if (test? widget)
                (cons widget (fn-for-low (widget-parts widget)))
                (fn-for-low (widget-parts widget))))

          (define (fn-for-low low)
            (cond [(empty? low) empty]
                  [else (append (fn-for-widget (first low))
                                (fn-for-low (rest low)))]))]
    (fn-for-widget widget)))



(check-expect (find-widget-name-longer-than Glass 2) (list Glass)) 
(check-expect (find-widget-name-longer-than Glass 6) empty) 
(check-expect (find-widget-name-longer-than Cord 3) (list Cord Wire)) 
(check-expect (find-widget-name-longer-than Jewelry 12) empty) 

(define (find-widget-name-longer-than widget num)
  (find-widget widget (λ(w) (> (string-length (widget-name w)) num))))




(check-expect (find-widget-quantity-over Wire 3) empty) 
(check-expect (find-widget-quantity-over Wire 2) (list Wire)) 
(check-expect (find-widget-quantity-over Cord 5) (list Cord)) 
(check-expect (find-widget-quantity-over Cord 2) (list Cord Wire)) 

(define (find-widget-quantity-over widget num)
  (find-widget widget (λ(w) (> (widget-quantity w) num))))



(check-expect (find-widgets-cheaper-than Wire 5) empty) 
(check-expect (find-widgets-cheaper-than Wire 6) (list Wire)) 
(check-expect (find-widgets-cheaper-than Bracelet 8) (list Bracelet Beads Glass)) 
(check-expect (find-widgets-cheaper-than Bracelet 6) (list Bracelet Glass)) 

(define (find-widgets-cheaper-than widget num)
  (find-widget widget (λ(w) (< (widget-price w) num))))



(check-expect (find-widget-hard-make Wire 3 5) empty) 
(check-expect (find-widget-hard-make Wire 6 5) (list Wire)) 
(check-expect (find-widget-hard-make Wire 3 4) (list Wire)) 
(check-expect (find-widget-hard-make Wire 6 4) (list Wire)) 
(check-expect (find-widget-hard-make Beads 6 7) empty) 
(check-expect (find-widget-hard-make Beads 6 6) (list Beads)) 
(check-expect (find-widget-hard-make Beads 7 10) (list Glass)) 
(check-expect (find-widget-hard-make Beads 26 5) (list Beads Glass)) 

(define (find-widget-hard-make widget nat num)
  (find-widget widget (λ(w) (or (< (widget-quantity w) nat)
                                (> (widget-price w) num)))))



(check-expect (return-all Cord) (list Cord Wire)) 
(check-expect (return-all Beads) (list Beads Glass)) 

(define (return-all widget)
  (find-widget widget (λ(w) (widget? w))))





(define (qsort fn? field)
  (local [(define (qsort0 widget)
            (local [(define lox0 (return-all widget))
                    (define (qsort-inner lox)
                      (cond
                        [(empty? lox) empty]
                        [else 
                         (local
                           [(define pivot (first lox))
                            (define (smaller? w) (fn? (field w) (field pivot)))]
                           (append
                            (qsort-inner (filter smaller? (rest lox)))               
                            (list pivot)
                            (qsort-inner
                             (filter (λ(n) 
                                       (not (smaller? n)))
                                     (rest lox)))))]))]
              (qsort-inner lox0)))]
    qsort0))



(check-expect (sort-strings Wire) (list Wire)) 
(check-expect (sort-strings Bracelet) (list Beads Bracelet Glass)) 
(check-expect (map widget-name (sort-strings Telephone))
              (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire")) 

(define sort-strings (qsort string<? widget-name))



(check-expect (sort-price Wire) (list Wire)) 
(check-expect (map widget-price (sort-price Bracelet))
              (map widget-price (list Glass Bracelet Beads))) 
(check-expect (map widget-name (sort-price Jewelry))
              (list "Chain" "Pendant" "Necklace" "Glass" "Bracelet"
                    "Beads" "Rings" "Jewelry set")) 
(check-expect (sort-price Jewelry) (list Chain Pendant Necklace Glass
                                         Bracelet Beads Rings Jewelry)) 

(define sort-price (qsort < widget-price))



(check-expect (sort-overstocked Wire) (list Wire)) 
(check-expect (map widget-name (sort-overstocked Necklace))
              (list "Necklace" "Chain" "Pendant")) 
(check-expect (map widget-name (sort-overstocked Bracelet))
              (list "Beads" "Glass" "Bracelet")) 
(check-expect (sort-overstocked Bracelet) (list Beads Glass Bracelet)) 

(define sort-overstocked (qsort > widget-quantity))



