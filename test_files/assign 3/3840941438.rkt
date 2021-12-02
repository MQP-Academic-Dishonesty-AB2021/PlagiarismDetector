

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

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
(define Jewel (make-widget "Jewel" 0 17 300
                           empty))
(define Rings (make-widget "Rings" 15 8 11 empty))
(define Jewelry (make-widget "Jewelry set" 4 17 30
                             (list Rings Necklace Bracelet)))

 
 

 





(check-expect (search
               (λ (n) (< 2 (string-length (widget-name n)))) 
               Rings)
              (list Rings))
(check-expect (search
               (λ (n) (< 7 (string-length (widget-name n)))) 
               Rings)
              empty)
(check-expect (search
               (λ (n) (< 3 (string-length (widget-name n)))) 
               Cord)
              (list Cord Wire))
(check-expect (search
               (λ (n) (< 4 (string-length (widget-name n)))) 
               Cord)
              empty)
(check-expect (search
               (λ (n) (< 0 (widget-quantity n))) 
               Rings)
              (list Rings))
(check-expect (search
               (λ (n) (< 15 (widget-quantity n))) 
               Rings)
              empty)
(check-expect (search (λ (n) (< 14 (widget-quantity n))) Rings) 
              (list Rings))
(check-expect (search (λ (n) (< 5 (widget-quantity n))) Cord) 
              (list Cord))
(check-expect (search (λ (n) (< 2 (widget-quantity n))) Cord) 
              (list Cord Wire))
(check-expect (search (λ (n) (< 8 (widget-quantity n))) Buttons) 
              (list Numbers))
(check-expect (search (λ (n) (> 0 (widget-price n))) Telephone) 
              empty)
(check-expect (search (λ (n) (> 7 (widget-price n))) Cord) 
              (list Cord Wire))
(check-expect (search (λ (n) (> 5 (widget-price n))) Cord) 
              empty)
(check-expect (search (λ (n) (or (> 4 (widget-quantity n)) 
                                 (< 19 (widget-price n))))
                      Telephone)
              (list Wire))
(check-expect (search (λ (n) (or (> 1 (widget-quantity n)) 
                                 (< 20 (widget-price n))))
                      Telephone) empty)
(check-expect (search (λ (n) (or (> 5 (widget-quantity n)) 
                                 (< 15 (widget-price n))))
                      Jewelry)
              (list Jewelry Pendant))

(define (search fn? widget)
  (local [
          (define (search--widget widget)
            (if (fn? widget)
                (cons widget (search--low (widget-parts widget)))
                (search--low (widget-parts widget))))
          (define (search--low low)
            (cond [(empty? low) empty]
                  [else
                   (append (search--widget (first low))
                           (search--low (rest low)))]))]
    (search--widget widget)))



 
(check-expect (widget-longer-than Rings 2) 
              (list Rings))
(check-expect (widget-longer-than Rings 7) 
              empty)
(check-expect (widget-longer-than Cord 3) 
              (list Cord Wire))
(check-expect (widget-longer-than Cord 4) 
              empty)

(define (widget-longer-than widget length)
  (search (λ (n) (< length (string-length (widget-name n)))) widget))



(check-expect (widget-quantity-over Rings 0) 
              (list Rings))
(check-expect (widget-quantity-over Rings 15) 
              empty)
(check-expect (widget-quantity-over Rings 14) 
              (list Rings))
(check-expect (widget-quantity-over Cord 5) 
              (list Cord))
(check-expect (widget-quantity-over Cord 2) 
              (list Cord Wire))
(check-expect (widget-quantity-over Buttons 8) 
              (list Numbers))
 

(define (widget-quantity-over widget quantity)
  (search (λ (n) (< quantity (widget-quantity n))) widget))



(check-expect (widget-cheaper-than Telephone 0) empty)
(check-expect (widget-cheaper-than Cord 7)(list Cord Wire))
(check-expect (widget-cheaper-than Cord 5) empty)
 

(define (widget-cheaper-than widget price)
  (search (λ (n) (> price (widget-price n))) widget))




 
(check-expect (widget-hard-make Telephone 4 19) (list Wire)) 
(check-expect (widget-hard-make Telephone 1 20) empty) 
(check-expect (widget-hard-make Jewelry 5 15) (list Jewelry Pendant)) 

(define (widget-hard-make widget quantity price)
  (search (λ (n) (or (> quantity (widget-quantity n))
                     (< price (widget-price n)))) widget))





(check-expect ((qsort string<? widget-name) Telephone) 
              (list Buttons Cord Numbers Receiver Telephone Wire))
(check-expect ((qsort > widget-quantity) Necklace) 
              (list Necklace Chain Pendant))
(check-expect ((qsort < widget-quantity) Necklace) 
              (list Pendant Chain Necklace))
(check-expect ((qsort (λ (w1 w2) false) identity) Wire) (list Wire)) 
(check-expect ((qsort (λ (w1 w2) false) identity) Jewelry) 
              (list Jewelry Rings Necklace Chain
                    Pendant Bracelet Beads Glass))

(define (qsort fn? fn-get)
  (λ (widget)
    (local [(define (get-list w)
              (cons w
                    (get-list--low (widget-parts w))))
            (define (get-list--low low)
              (cond [(empty? low) empty]
                    [else
                     (append (get-list (first low))
                             (get-list--low (rest low)))]))
            (define (sort low)
              (cond [(empty? low) empty]
                    [else
                     (local [(define pivot (first low))
                             (define (comparison widget)
                               (fn? (fn-get widget) (fn-get pivot)))]
                       (append (sort (filter comparison (rest low)))
                               (list pivot)
                               (sort (filter (λ (w) (not (comparison w)))
                                             (rest low)))))]))]
      (sort (get-list widget)))))