

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part22) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




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


 


 




 






(define (widget-top w fn?)
  
  (local [(define (fn-for-widget w fn?)
            (cond [(fn? w)
                   (cons w
                         (fn-for-low (widget-parts w) fn?))]
                  [else (fn-for-low (widget-parts w) fn?)]))

          (define (fn-for-low low fn?)
            (cond
              [(empty? low) empty]
              [else
               (append (fn-for-widget (first low) fn?)
                       (fn-for-low (rest low) fn?))]))]

    (fn-for-widget w fn?)))




(define (find-widget-name-longer-than w param)
  (local [(define (fn? w)
            (> (string-length (widget-name w)) param))]
    (widget-top w fn?)))


  (check-expect (find-widget-name-longer-than Wire 6) empty)

  (check-expect (find-widget-name-longer-than Cord 2) (list Cord Wire))

  (check-expect (find-widget-name-longer-than Telephone 8) (list Telephone))



(define (find-widget-quantity-over w param)
  (local [(define (fn? w)
            (> (widget-quantity w) param))]
    (widget-top w fn?)))


  (check-expect (find-widget-quantity-over Wire 4) empty)

  (check-expect (find-widget-quantity-over Cord 2) (list Cord Wire))

  (check-expect (find-widget-quantity-over Telephone 9) (list Receiver))



(define (find-widget-cheaper-than w param)
  (local [(define (fn? w)
            (< (widget-price w) param))]
    (widget-top w fn?)))


  (check-expect (find-widget-cheaper-than Wire 4) empty)

  (check-expect (find-widget-cheaper-than Cord 10) (list Cord Wire))

  (check-expect (find-widget-cheaper-than Buttons 10) (list Buttons Numbers))

  (check-expect (find-widget-cheaper-than Telephone 10)
                (list Receiver Buttons Numbers Cord Wire))



(define (find-widget-hard-make w param1 param2)
  (local [(define (fn? w)
            (or (< (widget-quantity w) param1) (> (widget-price w) param2)))]
    (widget-top w fn?)))


  (check-expect (find-widget-hard-make Wire 3 5) empty)

  (check-expect (find-widget-hard-make Cord 8 6) (list Cord Wire))

  (check-expect (find-widget-hard-make Buttons 10 4) (list Buttons Numbers))

  (check-expect (find-widget-hard-make Telephone 4 16) (list Wire))







(define (qsort fn? param)
  (local
    [(define (qsort-inner widget)
       (qsort-inner1 (widget-top widget widget?)))
     (define (qsort-inner1 low)
       (cond
         [(empty? low) empty]
         [else 
          (local
            [(define pivot (first low))
             (define (smaller? n) (fn? (param n) (param pivot)))]
            (append
             (qsort-inner1 (filter smaller? (rest low)))               
             (list pivot)
             (qsort-inner1
              (filter (λ(n) 
                        (not (smaller? n)))
                      (rest low)))))]))]
    qsort-inner))



(define sort-strings (qsort string<? widget-name))
(map widget-name (sort-strings Telephone))

(define sort-overstocked (qsort > widget-quantity))
(map widget-name (sort-overstocked Necklace))