

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname FINALAssignment#2_12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)


(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT))
(define SLOW .75)
(define SLOWER .5)
(define STOP 0)

(define TICKS-SECOND 28) 






(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 


(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))

(define RON
  (above (circle LIGHT-RADIUS "solid" "red")
         (circle LIGHT-RADIUS "outline" "yellow")
         (circle LIGHT-RADIUS "outline" "green")
         ))
(define GON
  (above (circle LIGHT-RADIUS "outline" "red")
         (circle LIGHT-RADIUS "outline" "yellow")
         (circle LIGHT-RADIUS "solid" "green")
         ))
(define YON
  (above (circle LIGHT-RADIUS "outline" "red")
         (circle LIGHT-RADIUS "solid" "yellow")
         (circle LIGHT-RADIUS "outline" "green")
         ))







(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)



(define-struct timer (ticks))




 



(define-struct auto (x y speed image))







 

(define A1 (make-auto 0 300 5 0))
(define A2 (make-auto 0 500 10 1))
(define A3 (make-auto 0 444 15 2))
(define A4 (make-auto 390 400 15 0))
(define A5 (make-auto 500 400 10 1))
(define A6 (make-auto 400 400 10 2))
(define A7 (make-auto 500 400 10 0))
(define A8 (make-auto 850 400 10 1))







 

(define LoA0 empty)
(define LoA1 (list A1))
(define LoA2 (list A1 A2 A3))
(define LoA3 (list A1 A2 A3 A4))
(define LoA4 (list A6))
(define LoA5 (list A5))
(define LoA6 (list A1 A2 A3 A4 A5 A6 A7))
(define LoA7 (list A1 A2 A3 A4 A5 A6 A7 A8))



(define-struct stop-light (color remaining-time time LoA))








 

(define SL1 (make-stop-light 0 (* GREEN-LENGTH TICKS-SECOND) 0 LoA1))
(define SL2 (make-stop-light 1 (* YELLOW-LENGTH TICKS-SECOND) 0 LoA6))
(define SL3 (make-stop-light 2 (* RED-LENGTH TICKS-SECOND) 0 LoA6))
(define SL4 (make-stop-light 0 (* 1 TICKS-SECOND) 0 LoA6))
(define SL5 (make-stop-light 1 (* 1 TICKS-SECOND) 0 LoA6))
(define SL6 (make-stop-light 2 (* 1 TICKS-SECOND) 0 LoA6))
(define SL7 (make-stop-light 0 (* 0 TICKS-SECOND) 0 LoA6))
(define SL8 (make-stop-light 1 (* 0 TICKS-SECOND) 0 LoA6))
(define SL9 (make-stop-light 2 (* 0 TICKS-SECOND) 0 LoA6))



(define START (make-stop-light 0 (* TICKS-SECOND GREEN-LENGTH) 0 LoA1))



(define (main stop-light)
  (big-bang stop-light
    (on-tick advance-time)
    (to-draw render)))







(check-random (advance-time SL1) (make-stop-light
                                  0
                                  (sub1 (* GREEN-LENGTH TICKS-SECOND))
                                  (add1 (stop-light-time SL1))
                                  (update-autos (stop-light-LoA SL1) SL1)))
(check-random (advance-time SL9) (make-stop-light
                                  0
                                  (* GREEN-LENGTH TICKS-SECOND)
                                  (add1 (stop-light-time SL9))
                                  (update-autos (stop-light-LoA SL9) SL9)))
(check-random (advance-time SL5) (make-stop-light
                                  1
                                  (sub1 TICKS-SECOND)
                                  (add1 (stop-light-time SL5))
                                  (update-autos (stop-light-LoA SL5) SL5)))
(check-random (advance-time SL2) (make-stop-light
                                  1
                                  (sub1 (* YELLOW-LENGTH TICKS-SECOND))
                                  (add1 (stop-light-time SL2))
                                  (update-autos (stop-light-LoA SL2) SL2)))
(check-random (advance-time SL7) (make-stop-light
                                  1
                                  (* YELLOW-LENGTH TICKS-SECOND)
                                  (add1 (stop-light-time SL7))
                                  (update-autos (stop-light-LoA SL7) SL7)))
(check-random (advance-time SL8) (make-stop-light
                                  2
                                  (* RED-LENGTH TICKS-SECOND)
                                  (add1 (stop-light-time SL8))
                                  (update-autos (stop-light-LoA SL8) SL8)))



(define (advance-time SL)
  (cond [(zero? (stop-light-remaining-time SL))
         (cond [(= (stop-light-color SL) 0) 
                (make-stop-light
                 1
                 (* TICKS-SECOND YELLOW-LENGTH)
                 (add1 (stop-light-time SL))
                 (update-autos (stop-light-LoA SL) SL))]
               [(= (stop-light-color SL) 1) 
                (make-stop-light 
                 2
                 (* TICKS-SECOND RED-LENGTH)
                 (add1 (stop-light-time SL))
                 (update-autos (stop-light-LoA SL) SL))]
               [(= (stop-light-color SL) 2) 
                (make-stop-light
                 0
                 (* TICKS-SECOND GREEN-LENGTH)
                 (add1 (stop-light-time SL))
                 (update-autos (stop-light-LoA SL) SL))])]
        [else
         (make-stop-light (stop-light-color SL)
                          (sub1 (stop-light-remaining-time SL))
                          (add1 (stop-light-time SL))
                          (update-autos (stop-light-LoA SL) SL))]))






(check-random (update-autos empty SL1)
              (list (make-auto
               0
               (+ 300 (random (- HEIGHT 330)))
               (+ 5 (random 15))
               (random 3))))




(define (update-autos LoA SL)
  (generate-auto (check-delete (next-auto LoA SL))))






(check-random (generate-auto empty)
              (list (make-auto
               0
               (+ 300 (random (- HEIGHT 330)))
               (+ 5 (random 15))
               (random 3))))





(define (generate-auto LoA)
  (cond [(empty? LoA)
         (cons (make-auto
                0
                (+ 300 (random (- HEIGHT 330)))
                (+ 5 (random 15))
                (random 3))
               LoA)]
        [(< (random 100) 5)
         (cons (make-auto
                0
                (+ 300 (random (- HEIGHT 330)))
                (+ 5 (random 15))
                (random 3))
               LoA)]
        [else LoA]))






(check-expect (check-delete empty) empty)
(check-expect (check-delete LoA3) LoA3)
(check-expect (check-delete LoA7) (list A1 A2 A3 A4 A5 A6 A7))



(define (check-delete LoA)
  (cond [(empty? LoA) empty]
        [else (if (> (auto-x (first LoA)) WIDTH)
                  (append (check-delete (rest LoA)))
                  (cons (first LoA) (check-delete (rest LoA))))]))






(check-expect (next-auto empty SL1) empty)
(check-expect (next-auto LoA1 SL1)
              (list (make-auto
                     (+ (auto-x (first LoA1)) (auto-speed (first LoA1)))
                     (auto-y (first LoA1))
                     (auto-speed (first LoA1))
                     (auto-image (first LoA1)))))
(check-expect (next-auto LoA1 SL2)
              (list (make-auto
                     (+ (auto-x (first LoA1)) (* (auto-speed (first LoA1))
                                                 SLOW))
                     (auto-y (first LoA1))
                     (auto-speed (first LoA1))
                     (auto-image (first LoA1)))))
(check-expect (next-auto LoA1 SL3)
              (list (make-auto
                     (+ (auto-x (first LoA1)) (* (auto-speed (first LoA1))
                                                 SLOWER))
                     (auto-y (first LoA1))
                     (auto-speed (first LoA1))
                     (auto-image (first LoA1)))))
(check-expect (next-auto LoA4 SL3)
              (list (make-auto
                     X-POS
                     (auto-y A6)
                     (auto-speed A6)
                     (auto-image A6))))
(check-expect (next-auto LoA5 SL3)
              (list (make-auto
                     (+ (auto-x A5) (auto-speed A5))
                     (auto-y A5)
                     (auto-speed A5)
                     (auto-image A5))))



(define (next-auto LoA SL)
  (cond [(empty? LoA) empty]
        [(> (auto-x (first LoA)) X-POS) 
         (cons (make-auto
                (+ (auto-x (first LoA)) (auto-speed (first LoA)))
                (auto-y (first LoA))
                (auto-speed (first LoA))
                (auto-image (first LoA)))
               (next-auto (rest LoA) SL))]
        [(= (stop-light-color SL) 1) 
         (cons
          (make-auto
           (+ (auto-x (first LoA)) (* (auto-speed (first LoA)) SLOW))
           (auto-y (first LoA))
           (auto-speed (first LoA))
           (auto-image (first LoA)))
          (next-auto (rest LoA) SL))]
        [(= (stop-light-color SL) 2) 
         (cond [(> (+ (auto-x (first LoA)) (auto-speed (first LoA))) X-POS)
                (cons
                 (make-auto
                  X-POS
                  (auto-y (first LoA))
                  (auto-speed (first LoA))
                  (auto-image (first LoA)))
                 (next-auto (rest LoA) SL))]
               [else
                (cons
                 (make-auto
                  (+ (auto-x (first LoA)) (* (auto-speed (first LoA)) SLOWER))
                  (auto-y (first LoA))
                  (auto-speed (first LoA))
                  (auto-image (first LoA)))
                 (next-auto (rest LoA) SL))])]
        [else   
         (cons
          (make-auto
           (+ (auto-x (first LoA)) (auto-speed (first LoA)))
           (auto-y (first LoA))
           (auto-speed (first LoA))
           (auto-image (first LoA)))
          (next-auto (rest LoA) SL))]))






(check-expect (render SL1)
              (overlay
               (render-autos (stop-light-LoA SL1))
               (overlay/align/offset "right"
                                     "middle"
                                     (text (tick->string (stop-light-time SL1))
                                           24
                                           "black")
                                     300
                                     150
                                     (place-image GON X-POS Y-POS MTS))))
(check-expect (render SL2)
              (overlay
               (render-autos (stop-light-LoA SL2))
               (overlay/align/offset "right"
                                     "middle"
                                     (text (tick->string (stop-light-time SL2))
                                           24
                                           "black")
                                     300
                                     150
                                     (place-image YON X-POS Y-POS MTS))))
(check-expect (render SL3)
              (overlay
               (render-autos (stop-light-LoA SL3))
               (overlay/align/offset "right"
                                     "middle"
                                     (text (tick->string (stop-light-time SL3))
                                           24
                                           "black")
                                     300
                                     150
                                     (place-image RON X-POS Y-POS MTS))))



(define (render SL)
  (overlay
   (render-autos (stop-light-LoA SL))
   (overlay/align/offset "right" "middle"
                         (text (tick->string (stop-light-time SL)) 24 "black")
                         300 150
                         (cond [(= (stop-light-color SL) 0)
                                (place-image GON X-POS Y-POS MTS)]
                               [(= (stop-light-color SL) 1)
                                (place-image YON X-POS Y-POS MTS)]
                               [(= (stop-light-color SL) 2)
                                (place-image RON X-POS Y-POS MTS)]))))






(check-expect (tick->string 0) "0")
(check-expect (tick->string 28) "1")
(check-expect (tick->string 30) "1")



(define (tick->string TICK)
  (number->string (ticks->seconds TICK)))






(check-expect (render-autos empty) (rectangle WIDTH HEIGHT "outline" "white"))
(check-expect (render-autos LoA1) (place-image
                                   (pick-image (auto-image (first LoA1)))
                                   (auto-x (first LoA1))
                                   (auto-y (first LoA1))
                                   (rectangle WIDTH HEIGHT "outline" "white")))



(define (render-autos LoA)
  (cond [(empty? LoA) (rectangle WIDTH HEIGHT "outline" "white")]
        [else
         (place-image
          (pick-image (auto-image (first LoA)))
          (auto-x (first LoA))
          (auto-y (first LoA))
          (render-autos (rest LoA)))]))
  









(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))

(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)





(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))
(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)