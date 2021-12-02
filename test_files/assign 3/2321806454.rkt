

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

 



(check-expect (find-widget Telephone
                           (λ (wid)
                             (< (widget-time wid) 10)))
              (list Receiver Buttons Numbers Cord Wire))

(check-expect (find-widget Jewelry
                           (λ (wid)
                             (> (string-length(widget-name wid)) 5)))
              (list Jewelry Necklace Pendant Bracelet))

(check-expect (find-widget Bracelet
                           (λ (wid)
                             (> (string-length(widget-name wid)) 10)))
              empty)

(check-expect (find-widget Numbers
                           (λ (wid)
                             (> (string-length(widget-name wid)) 3)))
              (list Numbers))

(check-expect (find-widget Telephone
                           (λ (wid)
                             (> (widget-quantity wid) 5)))
              (list Receiver Buttons Numbers Cord))

(check-expect (find-widget Wire
                           (λ (wid)
                             (> (widget-quantity wid) 7)))
              empty)

(check-expect (find-widget Necklace
                           (λ (wid)
                             (> (widget-quantity wid) 0)))
              (list Necklace Chain Pendant))

(check-expect (find-widget Telephone
                           (λ (wid)
                             (< (widget-price wid) 8)))
              (list Receiver Buttons Numbers Cord Wire))

(check-expect (find-widget Wire
                           (λ (wid)
                             (< (widget-price wid) 2.5)))
              empty)

(check-expect (find-widget Necklace
                           (λ (wid)
                             (< (widget-price wid) 15)))
              (list Necklace Chain Pendant))

(check-expect (find-widget Telephone
                           (λ (wid)
                             (or (>  (widget-price wid) 4)
                                 (< (widget-quantity wid) 8))))
              (list Telephone Receiver Buttons Numbers Cord Wire))

(check-expect (find-widget Wire
                           (λ (wid)
                             (or (>  (widget-price wid) 5)
                                 (< (widget-quantity wid) 2))))
              empty)

(check-expect (find-widget Necklace
                           (λ (wid)
                             (or (>  (widget-price wid) 2)
                                 (< (widget-quantity wid) 1))))
              (list Necklace)) 




(define (find-widget wid fn)
  (local[(define (fn-for-widget wid)
           (if (fn wid)
               (cons wid
                     (fn-for-low (widget-parts wid)))
               (fn-for-low (widget-parts wid))))

         (define (fn-for-low low)
           (cond
             [(empty? low) empty]
             [else
              (append (fn-for-widget (first low))
                      (fn-for-low (rest low)))]))]
    (fn-for-widget wid)))







(define (qsort fn? field)
  (local [(define (qsort-inner low)
            (cond [(empty? low) empty]
                  [else
                   (local [(define pivot (first low))
                           (define (smaller? n)
                             (fn? (field n) (field pivot)))]
                     (append
                      (qsort-inner (filter smaller? (rest low)))
                      (list pivot)
                      (qsort-inner
                       (filter (λ(n)
                                 (not (smaller? n)))
                               (rest low)))))]))]
    (local [(define (qsort-final wid)
              (qsort-inner (make-widget-list wid)))]
      qsort-final)))






(check-expect (make-widget-list Telephone) (list Telephone Receiver Buttons Numbers Cord Wire)) 
(check-expect (make-widget-list Bracelet) (list Bracelet Beads Glass))
(check-expect (make-widget-list Wire) (list Wire))

(define (make-widget-list wid)
  (local[(define (fn-for-widget wid)
           (cons wid
                 (fn-for-low (widget-parts wid))))

         (define (fn-for-low low)
           (cond
             [(empty? low) empty]
             [else
              (append (fn-for-widget (first low))
                      (fn-for-low (rest low)))]))]
    (fn-for-widget wid)))



(define sort-strings (qsort string<? widget-name))

(check-expect (sort-strings Telephone)
              (list Buttons Cord Numbers Receiver Telephone Wire))
(check-expect (sort-strings Buttons)
              (list Buttons Numbers))

(check-expect ((qsort string<? widget-name) Buttons)
              (list Buttons Numbers))





(define sort-overstocked (qsort > widget-quantity))

(check-expect (sort-overstocked Necklace)
              (list Necklace Chain Pendant))
(check-expect (sort-overstocked Beads)
              (list Beads Glass))

(check-expect ((qsort > widget-quantity) Beads)
              (list Beads Glass))