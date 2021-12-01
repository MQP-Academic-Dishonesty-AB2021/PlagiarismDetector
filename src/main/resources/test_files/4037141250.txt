

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 3 Part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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
 

(define (filter-widgets awidget fn)
  (local [(define (test-widget awidget fn)
            (if (fn awidget)
                (cons awidget (test-parts (widget-parts awidget) fn))
                (test-parts (widget-parts awidget) fn)
                )
            )

          (define (test-parts low fn)
            (cond
              [(empty? low) empty]
              [else
               (append (test-widget (first low) fn)
                       (test-parts (rest low) fn))]
              )
            )]
    (test-widget awidget fn)))





(define (find-widget-name-longer-than awidget length)
  (filter-widgets awidget (λ (awidget)
                            (> (string-length (widget-name awidget)) length))
                  )
  )





(define (find-widget-quantity-over awidget min-amount)
  (filter-widgets awidget (λ (awidget)
                            (> (widget-quantity awidget) min-amount)))
  )




(define (find-widgets-cheaper-than awidget price)
  (filter-widgets awidget (λ (awidget) (< (widget-price awidget) price)))
  )




(define (find-widget-hard-make awidget amount price)
  (filter-widgets awidget (λ (awidget) (or (< (widget-quantity awidget) amount)
                                           (> (widget-price awidget) price))))
  )


(check-expect (find-widget-name-longer-than Wire 3) (list Wire))

(check-expect (find-widget-name-longer-than Chain 5) empty)

(check-expect (find-widget-name-longer-than Beads 3) (list Beads Glass))

(check-expect (find-widget-name-longer-than Necklace 6) (list Necklace Pendant))


(check-expect (find-widget-quantity-over Wire 5) empty)

(check-expect (find-widget-quantity-over Chain 5) (list Chain))

(check-expect (find-widget-quantity-over Glass 6) empty)

(check-expect (find-widget-quantity-over Buttons 7) (list Buttons Numbers))

(check-expect (find-widget-quantity-over Buttons 8) (list Numbers))


(check-expect (find-widgets-cheaper-than Wire 5) empty)

(check-expect (find-widgets-cheaper-than Beads 6) (list Glass))

(check-expect (find-widgets-cheaper-than Jewelry 3) (list Chain Pendant))


(check-expect (find-widget-hard-make Wire 3 5) empty)

(check-expect (find-widget-hard-make Beads 10 30) (list Glass))

(check-expect (find-widget-hard-make Buttons 10 6) (list Buttons Numbers))








(define (qsort sort access)
  (local [
          (define (widget->list awidget)
            (filter-widgets awidget (λ (n) true))
            )
          (define (do-sort lox comparator accessor)
            (cond [(empty? lox) empty]
                  [(empty? (rest lox)) lox]
                  [else
                   (local [(define (less-than x)
                             (comparator (accessor x)
                                         (accessor (first lox))))]
                     (append (do-sort (filter less-than (rest lox))
                                      comparator accessor)
                             (list (first lox))
                             (do-sort (filter (lambda (x) (not (less-than x)))
                                              (rest lox))
                                      comparator accessor)))]))
          (define (return-fn awidget)
            (do-sort (widget->list awidget) sort access))
          ]
    return-fn)
  )


(check-expect ((qsort < widget-quantity) Wire) (list Wire))


(check-expect ((qsort > widget-time) Telephone)
              (list Telephone Receiver Buttons Numbers Cord Wire))

(check-expect ((qsort > widget-price) Jewelry)
              (list Jewelry Rings Beads Bracelet Glass Necklace Chain Pendant))







(check-expect (sort-strings Wire) (list Wire))

(check-expect (sort-strings Telephone)
              (list Buttons Cord Numbers Receiver Telephone Wire))

(check-expect (sort-strings Jewelry)
              (list Beads Bracelet Chain Glass Jewelry Necklace Pendant Rings))


(define sort-strings (qsort string<? widget-name))