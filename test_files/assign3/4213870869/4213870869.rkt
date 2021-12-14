

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define-struct widget(name quantity time price parts))


(define YUMMY (make-widget "" 0 0 0 empty))

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



 








(check-expect (find-widget-name-longer-than YUMMY 0)
              empty) 
(check-expect (find-widget-name-longer-than Wire 2)
              (list Wire)) 
(check-expect (find-widget-name-longer-than Jewelry 0) 
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect (find-widget-name-longer-than Jewelry 7)
              (list Jewelry Necklace Bracelet)) 

(check-expect (find-widget-quantity-over YUMMY 0)
              empty) 
(check-expect (find-widget-quantity-over Wire 2)
              (list Wire)) 
(check-expect (find-widget-quantity-over Jewelry 5)
              (list Rings Necklace Chain Beads Glass)) 


(check-expect (find-widgets-cheaper-than YUMMY 0)
              empty) 

(check-expect (find-widgets-cheaper-than Jewelry 0)
              empty) 

(check-expect (find-widgets-cheaper-than Wire 2)
              empty) 

(check-expect (find-widgets-cheaper-than Jewelry 5)
              (list Necklace Chain Pendant Glass)) 

(check-expect (find-widgets-cheaper-than Jewelry 4294967295) 
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

(check-expect (find-widgets-cheaper-than Telephone 6)
              (list Buttons Numbers Cord Wire)) 


(check-expect (find-widget-hard-make YUMMY 0 0)
              empty) 

(check-expect (find-widget-hard-make Wire 2 0)
              (list Wire)) 

(check-expect (find-widget-hard-make Jewelry 4294967295 0) 
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

(check-expect (find-widget-hard-make Jewelry 5 5)
              (list Jewelry Rings Pendant Beads)) 



(define (widget-worker fn? widget) 
  (local [(define (fn-for-widget widget)
            (if (fn? widget)
                (cons widget (fn-for-low (widget-parts widget)))
                (fn-for-low (widget-parts widget))))
          (define (fn-for-low low)
            (cond [(empty? low) empty]
                  [else
                   (append (fn-for-widget (first low))
                           (fn-for-low (rest low)))]))]
    (fn-for-widget widget)))










(define (find-widget-name-longer-than widget length)
  (local [(define (fn-for-widget widget)
            (> (string-length (widget-name widget)) length))]
    (widget-worker fn-for-widget widget)))








(define (find-widget-quantity-over widget val)
  (local [(define (fn-for-widget widget)
            (> (widget-quantity widget) val))]
    (widget-worker fn-for-widget widget)))









(define (find-widgets-cheaper-than widget price)
  (local [(define (fn-for-widget widget)
            (< (widget-price widget) price))]
    (widget-worker fn-for-widget widget)))









(define (find-widget-hard-make widget stock cost)
  (local [(define (fn-for-widget widget)
            (or (< (widget-quantity widget) stock) (> (widget-price widget) cost)))]
    (widget-worker fn-for-widget widget)))






(check-expect (sort-strings Telephone) 
              (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))
(check-expect (sort-strings Wire)
              (list "Wire")) 
(check-expect (sort-strings YUMMY)
              (list "")) 



(define (qsort-outer fn? selector)
  (local
    [(define (qsort w)
       (if (list? w)
           (cond
             [(empty? w) empty]
             [else 
              (local
                [(define pivot (first w))
                 (define (selector? n)
                   (fn? (selector n) (selector pivot)))]
                (append
                 (qsort (filter selector? (rest w)))               
                 (list pivot)
                 (qsort (filter (λ(n) 
                                  (not (selector? n)))
                                (rest w)))))])
           (qsort (widget->list w))))]
    qsort))







(define (widget->list widget)
  (local [(define (fn-for-widget w)
            (if (not (empty? w))
                (cons  w (fn-for-low (widget-parts w)))
                (fn-for-low (widget-parts w))))
          (define (fn-for-low low)
            (cond [(empty? low) empty]
                  [else (append (fn-for-widget (first low))
                                (fn-for-low (rest low)))]))]
    (fn-for-widget widget)))





(define sort-strings (qsort-outer string<? widget-name))
(map widget-name (sort-strings Telephone))