

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part1_DanielB_AlexM) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget(name quantity time price parts))


(define (fn-for-widget widget)
  (... (widget-name widget)
       (widget-quantity widget)
       (widget-time widget)
       (widget-price widget)
       (fn-for-list-of-widget (widget-parts widget))
       )
)
 








(check-expect (find-widget-name-longer-than Wire 3) (list Wire)) 
(check-expect (find-widget-name-longer-than Wire 4) empty) 
(check-expect (find-widget-name-longer-than Telephone 6) (list Receiver Numbers Buttons Telephone)) 
(check-expect (find-widget-name-longer-than emptyWidget 1) empty) 

(define (find-widget-name-longer-than widget n)
  (if (empty? (widget-parts widget))
      (widget-compare-name widget n)
      (append (low-name-helper (widget-parts widget) n) (widget-compare-name widget n))))
      






(check-expect (low-name-helper empty 3) empty) 
(check-expect (low-name-helper (list Receiver Buttons Cord) 3) (list Receiver Numbers Buttons Wire Cord)) 
(check-expect (low-name-helper (list Receiver Buttons Cord) 7) (list Receiver)) 
(check-expect (low-name-helper (list Cord) 4 ) empty) 

(define (low-name-helper low n)
  (cond [(empty? low) empty]
        [else (append (find-widget-name-longer-than (first low) n) (low-name-helper (rest low) n))]))
               



(check-expect(widget-compare-name Cord 4) empty) 
(check-expect(widget-compare-name Cord 5) empty) 
(check-expect(widget-compare-name Buttons 5) (list Buttons)) 



(define (widget-compare-name widget n)
      (if (> (string-length (widget-name widget)) n)
             (cons widget empty)
             empty
      )
)











(check-expect (find-widget-quanity-over Wire 2) (list Wire)) 
(check-expect (find-widget-quanity-over Wire 3) empty) 
(check-expect (find-widget-quanity-over Telephone 4) (list Receiver Numbers Buttons Cord Telephone)) 
(check-expect (find-widget-quanity-over emptyWidget 1) empty) 

(define (find-widget-quanity-over widget n)
  (if (empty? (widget-parts widget))
      (widget-compare-quanity widget n)
      (append (low-quanity-helper (widget-parts widget) n) (widget-compare-quanity widget n)))) 
      






(check-expect (low-quanity-helper empty 3) empty) 
(check-expect (low-quanity-helper (list Receiver Buttons Cord) 3) (list Receiver Numbers Buttons Cord)) 
(check-expect (low-quanity-helper (list Receiver Buttons Cord) 7) (list Receiver Numbers Buttons)) 
(check-expect (low-quanity-helper (list Numbers) 9) empty) 

(define (low-quanity-helper low n)
  (cond [(empty? low) empty]
        [else (append (find-widget-quanity-over (first low) n) (low-quanity-helper (rest low) n))]))
               



(check-expect(widget-compare-quanity Cord 7) empty) 
(check-expect(widget-compare-quanity Cord 8) empty) 
(check-expect(widget-compare-quanity Buttons 7) (list Buttons)) 



(define (widget-compare-quanity widget n)
      (if (> (widget-quantity widget) n)
             (cons widget empty)
             empty
      )
)











(check-expect (find-widgets-cheaper-than Wire 6) (list Wire)) 
(check-expect (find-widgets-cheaper-than Wire 5) empty) 
(check-expect (find-widgets-cheaper-than Telephone 6) (list Numbers Buttons Wire Cord)) 
(check-expect (find-widgets-cheaper-than emptyWidget 0) empty) 

(define (find-widgets-cheaper-than widget n)
  (if (empty? (widget-parts widget))
      (widget-compare-price widget n)
      (append (low-price-helper (widget-parts widget) n) (widget-compare-price widget n)))) 
      






(check-expect (low-price-helper empty 3) empty) 
(check-expect (low-price-helper (list Receiver Buttons Cord) 8) (list Receiver Numbers Buttons Wire Cord)) 
(check-expect (low-price-helper (list Receiver Buttons Cord) 6) (list Numbers Buttons Wire Cord))
(check-expect (low-price-helper (list Receiver Buttons Cord) 5) empty) 

(define (low-price-helper low n)
  (cond [(empty? low) empty]
        [else (append (find-widgets-cheaper-than (first low) n) (low-price-helper (rest low) n))]))
               



(check-expect(widget-compare-price Cord 5) empty) 
(check-expect(widget-compare-price Cord 4) empty) 
(check-expect(widget-compare-price Buttons 7) (list Buttons)) 




(define (widget-compare-price widget n)
      (if (< (widget-price widget) n)
             (cons widget empty)
             empty
      )
)










(check-expect (find-widgets-hard-make Wire 8 4) (list Wire)) 
(check-expect (find-widgets-hard-make Wire 3 5) empty) 
(check-expect (find-widgets-hard-make Telephone 10 6) (list Receiver Numbers Buttons Wire Cord Telephone)) 
(check-expect (find-widgets-hard-make emptyWidget 1 0) (list emptyWidget)) 


(define (find-widgets-hard-make widget nat n)
  (if (empty? (widget-parts widget))
      (widget-compare-hard-make widget nat n)
      (append (low-hard-make-helper (widget-parts widget) nat n) (widget-compare-hard-make widget nat n)))) 
      






(check-expect (low-hard-make-helper empty 3 2) empty) 
(check-expect (low-hard-make-helper (list Receiver Buttons Cord) 5 6) (list Receiver Wire)) 
(check-expect (low-hard-make-helper (list Receiver Buttons Cord) 10 10) (list Numbers Buttons Wire Cord))


(define (low-hard-make-helper low nat n)
  (cond [(empty? low) empty]
        [else (append (find-widgets-hard-make (first low) nat n) (low-hard-make-helper (rest low) nat n))]))
               



(check-expect(widget-compare-hard-make Cord 7 5) empty) 
(check-expect(widget-compare-hard-make Cord 6 4) (list Cord)) 
(check-expect(widget-compare-hard-make Cord 8 6) (list Cord)) 
(check-expect(widget-compare-hard-make Buttons 7 4) (list Buttons)) 


(define (widget-compare-hard-make widget quantity cost)
      (if (or (< (widget-quantity widget) quantity) (> (widget-price widget) cost))
             (cons widget empty)
             empty
      )
)


      
      (define emptyWidget (make-widget "" 0 0 0 empty))
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

      