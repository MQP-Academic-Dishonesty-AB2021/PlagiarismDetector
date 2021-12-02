

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))







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

(define Test-123 (make-widget "Test-123" 15 8 11 empty))
(define test (make-widget "Test" 15 8 11 (list Test-123)))









(define (widget-finder widget fn?)
  (local [(define (single-widget widget)
            (if (fn? widget)
                (cons widget (list-widget (widget-parts widget)))
                (list-widget (widget-parts widget))))
          
          (define (list-widget low)
            (cond
              [(empty? low) empty]
              [else
               (append (single-widget (first low)) (list-widget (rest low)))
               ]))]
    
    (single-widget widget)))










(check-expect (find-widget-name-longer-than Necklace 5)
              (list Necklace Pendant)) 
(check-expect (find-widget-name-longer-than Glass 5)
              empty) 
(check-expect (find-widget-name-longer-than Telephone 8)
              (list Telephone)) 
(check-expect (find-widget-name-longer-than test 4)
              (list Test-123))



(define (find-widget-name-longer-than widget length)
  (widget-finder widget (λ (w)
                          (> (string-length (widget-name w)) length))))








(check-expect (find-widget-quantity-over Necklace 5)
              (list Necklace Chain)) 
(check-expect (find-widget-quantity-over Glass 6)
              empty) 
(check-expect (find-widget-quantity-over Telephone 8)
              (list Receiver Numbers)) 



(define (find-widget-quantity-over widget stock)
  (widget-finder widget (λ (w)
                          (> (widget-quantity w) stock))))






(check-expect (find-widget-cheaper-than Necklace 5)
              (list Necklace Chain Pendant)) 
(check-expect (find-widget-cheaper-than Glass 1)
              empty) 
(check-expect (find-widget-cheaper-than Telephone 8)
              (list Receiver Buttons Numbers Cord Wire)) 



(define (find-widget-cheaper-than widget price)
  (widget-finder widget (λ (w)
                          (< (widget-price w) price))))







(check-expect (find-widget-hard-make Necklace 5 4)
              (list Pendant)) 
(check-expect (find-widget-hard-make Glass 5 5)
              empty) 
(check-expect (find-widget-hard-make Telephone 4 5)
              (list Telephone Receiver Wire)) 



(define (find-widget-hard-make widget stock price)
  (widget-finder widget (λ (w)
                          (or (< (widget-quantity w) stock)
                              (> (widget-price w) price)))))








(define (qsort w-field fn?)
  (local [
          (define (qsort widget)
              (local [
                      (define (sort-list low)
                        (cond
                          [(empty? low) empty]
                          [else
                           (local [
                                   (define (smaller? n)
                                     (fn? (w-field n) (w-field pivot)))
                                   (define pivot (first low))]
                             (append
                              (sort-list (filter smaller? (rest low)))               
                              (list pivot)
                              (sort-list
                               (filter (λ(n) 
                                         (not (smaller? n)))                                   
                                       (rest low)))))]))]
                (sort-list (widget-finder widget (λ (w) true)))))]
    qsort))







(check-expect (map widget-name (sort-strings Telephone)) 
              (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))
(check-expect (map widget-name (sort-strings Jewelry)) 
              (list "Beads" "Bracelet" "Chain" "Glass" "Jewelry set"
                    "Necklace" "Pendant" "Rings"))
(check-expect (map widget-name (sort-strings Glass))
                   (list "Glass")) 



(define sort-strings (qsort widget-name string<?))





(check-expect (map widget-name (sort-overstocked Necklace))
              (list "Necklace" "Chain" "Pendant")) 
(check-expect (map widget-name (sort-overstocked Telephone))
              (list "Receiver" "Numbers" "Buttons" "Cord" "Telephone" "Wire"))
(check-expect (map widget-name (sort-overstocked Wire))
                   (list "Wire")) 



(define sort-overstocked (qsort widget-quantity >))