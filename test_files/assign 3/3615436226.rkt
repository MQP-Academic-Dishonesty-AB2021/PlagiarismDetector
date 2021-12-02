

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





 












(check-expect (find-widget-name-longer-than Glass 0) (list Glass))
(check-expect (find-widget-name-longer-than Glass 20) empty)
(check-expect (find-widget-name-longer-than Beads 4) (list Beads Glass))
(check-expect (find-widget-name-longer-than Buttons 9) empty)

(define (find-widget-name-longer-than widget n)
  (find-widget (λ (w) (> (string-length (widget-name w)) n)) widget))











(check-expect (find-widget-quantity-over Glass 0) (list Glass))
(check-expect (find-widget-quantity-over Glass 20) empty)
(check-expect (find-widget-quantity-over Beads 4) (list Beads Glass))
(check-expect (find-widget-quantity-over Buttons 8) (list Numbers))
(check-expect (find-widget-quantity-over Buttons 10) empty)

(define (find-widget-quantity-over widget n)
  (find-widget (λ (w) (> (widget-quantity w) n)) widget))










(check-expect (find-widget-cheaper-than Glass 5) (list Glass))
(check-expect (find-widget-cheaper-than Chain 3) (list Chain))
(check-expect (find-widget-cheaper-than Beads 6) (list Glass))
(check-expect (find-widget-cheaper-than Necklace 4) (list Necklace Chain Pendant))
(check-expect (find-widget-cheaper-than Buttons 10) (list Buttons Numbers))

(define (find-widget-cheaper-than widget n)
  (find-widget (λ (w) (< (widget-price w) n)) widget))










(check-expect (find-widget-hard-make Cord 8 6) (list Cord Wire))
(check-expect (find-widget-hard-make Buttons 3 10) empty)
(check-expect (find-widget-hard-make Beads 20 8) (list Glass))
(check-expect (find-widget-hard-make Telephone 6 19) (list Telephone Wire))

(define (find-widget-hard-make widget quant price)
  (find-widget (λ (w) (or (< (widget-quantity w) quant) (> (widget-price w) price))) widget))





(define (find-widget fn? wid)
  (local
    [(define (fn-element wid)
       (if (fn? wid)
           (cons wid (fn-low (widget-parts wid)))
           (fn-low (widget-parts wid))))

     (define (fn-low low)
       (cond
         [(empty? low) empty]
         [else
          (append (fn-element (first low))
                  (fn-low (rest low)))]))]
    (fn-element wid)))








(check-expect (sort-strings Telephone)
              (list Buttons Cord Numbers Receiver Telephone Wire))
(check-expect (sort-strings Jewelry)
              (list Beads Bracelet Chain Glass Jewelry Necklace Pendant Rings))
(check-expect (sort-strings Glass) (list Glass))


(check-expect (sort-overstocked Necklace)
              (list Necklace Chain Pendant))
(check-expect (sort-overstocked Buttons)
              (list Numbers Buttons))
(check-expect (sort-overstocked Wire)
              (list Wire))

  
(define (qsort fn? wid-prop)
  (λ (wid) 
    (local
      [(define (build wid)
         (cons wid (foldr append empty (map build (widget-parts wid)))))
       (define (sort alow)
         (cond
           [(empty? alow) empty]
           [else
            (local
              [(define pivot (first alow))
               (define (fn-for-wid wid)
                 (fn? (wid-prop wid) (wid-prop pivot)))] 
              (append
               (sort (filter fn-for-wid (rest alow))) 
               (list pivot)
               (sort (filter (λ(n)
                               (not (fn-for-wid n))) (rest alow)))))]))]
      (sort (build wid)))))




(define sort-strings (qsort string<? widget-name))  
(define sort-overstocked (qsort > widget-quantity)) 






  



      
    



