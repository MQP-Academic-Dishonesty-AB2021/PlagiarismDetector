

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




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






 

 










(check-expect (find-widget-name-longer-than--element Glass 0) (list Glass))
(check-expect (find-widget-name-longer-than--element Glass 20) empty)
(check-expect (find-widget-name-longer-than--element Beads 4) (list Beads Glass))
(check-expect (find-widget-name-longer-than--element Buttons 9) empty)
(check-expect (find-widget-name-longer-than--low (list Buttons Cord) 6)
              (list Buttons Numbers)) 
(check-expect (find-widget-name-longer-than--low (list Buttons Cord) 3)
              (list Buttons Numbers Cord Wire))
(check-expect (find-widget-name-longer-than--low empty 6) empty)


(define (find-widget-name-longer-than--element e n)
  (if (> (string-length (widget-name e)) n)
      (cons e (find-widget-name-longer-than--low (widget-parts e) n))
      (find-widget-name-longer-than--low (widget-parts e) n)))

(define (find-widget-name-longer-than--low low n)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-name-longer-than--element (first low) n)
           (find-widget-name-longer-than--low (rest low) n))]))











(check-expect (find-widget-quantity-over--element Glass 0) (list Glass))
(check-expect (find-widget-quantity-over--element Glass 20) empty)
(check-expect (find-widget-quantity-over--element Beads 4) (list Beads Glass))
(check-expect (find-widget-quantity-over--element Buttons 8) (list Numbers))
(check-expect (find-widget-quantity-over--element Buttons 10) empty)
(check-expect (find-widget-quantity-over--low (list Buttons Cord) 4)
              (list Buttons Numbers Cord))
(check-expect (find-widget-quantity-over--low (list Buttons Cord) 2)
              (list Buttons Numbers Cord Wire))
(check-expect (find-widget-quantity-over--low  empty 0) empty)

(define (find-widget-quantity-over--element e n)
  (if (> (widget-quantity e) n)
      (cons e (find-widget-quantity-over--low (widget-parts e) n))
      (find-widget-quantity-over--low (widget-parts e) n)))

(define (find-widget-quantity-over--low low n)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-quantity-over--element (first low) n)
           (find-widget-quantity-over--low (rest low) n))]))










(check-expect (find-widget-cheaper-than--element Glass 5) (list Glass))
(check-expect (find-widget-cheaper-than--element Chain 3) (list Chain))
(check-expect (find-widget-cheaper-than--element Beads 6) (list Glass))
(check-expect (find-widget-cheaper-than--element Necklace 4) (list Necklace Chain Pendant))
(check-expect (find-widget-cheaper-than--element Buttons 10) (list Buttons Numbers))
(check-expect (find-widget-cheaper-than--low (list Buttons Cord) 6)
              (list Buttons Numbers Cord Wire))
(check-expect (find-widget-cheaper-than--low (list Telephone) 18)
              (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget-cheaper-than--low  empty 0) empty)

(define (find-widget-cheaper-than--element e n)
  (if (< (widget-price e) n)
      (cons e (find-widget-cheaper-than--low (widget-parts e) n))
      (find-widget-cheaper-than--low (widget-parts e) n)))

(define (find-widget-cheaper-than--low low n)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-cheaper-than--element (first low) n)
           (find-widget-cheaper-than--low (rest low) n))]))














(check-expect (find-widget-hard-make--element Cord 8 6) (list Cord Wire))
(check-expect (find-widget-hard-make--element Buttons 3 10) empty)
(check-expect (find-widget-hard-make--element Beads 20 8) (list Glass))
(check-expect (find-widget-hard-make--element Telephone 6 19) (list Telephone Wire))
(check-expect (find-widget-hard-make--low empty 0 0) empty)
(check-expect (find-widget-hard-make--low (list Buttons) 19 4) (list Buttons Numbers))
(check-expect (find-widget-hard-make--low (list Necklace) 8 2) (list Necklace Chain Pendant))
(check-expect (find-widget-hard-make--low (list Receiver) 9 13) empty)

(define (find-widget-hard-make--element e q p)
  (if (or (< (widget-quantity e) q) (> (widget-price e) p))
      (cons e (find-widget-hard-make--low (widget-parts e) q p))
      (find-widget-hard-make--low (widget-parts e) q p)))

(define (find-widget-hard-make--low low q p)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-hard-make--element (first low) q p)
           (find-widget-hard-make--low (rest low) q p))]))




