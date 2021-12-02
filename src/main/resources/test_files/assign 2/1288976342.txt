

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 2 Traffic|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)




(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT))

(define TICKS-SECOND 28) 






(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 

(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)




(define-struct clock (ticks seconds color loa))









 





 

(define-struct auto (speed img x y))









 




(define START (make-clock 0 0 "green" empty))
(define MIN-Y (+ 30 (* 3 2 LIGHT-RADIUS) (image-height AUTO-IMAGE1)))
(define MAX-Y (- HEIGHT (image-height AUTO-IMAGE1)))
(define STOP-LINE (/ WIDTH 2))
(define MIN-SPEED 4) 
(define MAX-SPEED 25)

(define LOA (list (make-auto 10 AUTO-IMAGE1 0 30) (make-auto 5 AUTO-IMAGE2 30 30)))






(define (main clock)
  (big-bang clock
    (on-tick increase-clock)
    (to-draw render)))




(check-expect (increase-clock START) (make-clock 1 0 "green" empty)) 
(check-expect (increase-clock (make-clock TICKS-SECOND 0 "green" empty)) 
              (make-clock (add1 TICKS-SECOND) 1 "green" empty))          

(define (increase-clock clock)
  (local 
    [(define time-ratio (/ (clock-ticks clock) TICKS-SECOND))]
    (if (and (integer? time-ratio) (not (zero? time-ratio)))
        (make-clock (add1 (clock-ticks clock))
                    (add1 (clock-seconds clock))(light-color (clock-seconds clock))
                    (update-autos (clock-loa clock) (clock-color clock)))
        (make-clock (add1 (clock-ticks clock))
                    (clock-seconds clock)(light-color (clock-seconds clock))
                    (update-autos (clock-loa clock) (clock-color clock))))))



(check-satisfied (update-autos empty "green") list?)
(check-satisfied (update-autos (list (make-auto 1 AUTO-IMAGE1 0 0)) "green") list?)
 
(define (update-autos loa color)
  (if (< (random 100) 5) 
      (cons (make-auto (between MIN-SPEED MAX-SPEED) (pick-image (random 3)) 0
                       (between MIN-Y MAX-Y)) (check-autos loa color))
      (check-autos loa color)))
 


(check-expect (between 5 6) 5)
(check-range (between 10 15) 10 15)

(define (between mini maxi)
  (+ mini (random (- maxi mini))))




(check-expect (check-autos empty "green") empty)
(check-expect (check-autos (list (make-auto 2 AUTO-IMAGE1 0 -250)) "green")
              (list (make-auto 2 AUTO-IMAGE1 2 -250)))
(check-expect (check-autos (list (make-auto 5 AUTO-IMAGE1 WIDTH -250)) "green")
              empty)

(define (check-autos loa color)
  (cond [(empty? loa) empty]
        [(>= (auto-x (first loa)) WIDTH)
         (check-autos (rest loa) color)]
        [else
         (cons (move-auto (first loa) color) (check-autos (rest loa) color))]))                                  



(check-expect (move-auto (make-auto 2 AUTO-IMAGE1 0 -250) "green")
              (make-auto 2 AUTO-IMAGE1 2 -250))

(define (move-auto auto color)
  (make-auto (auto-speed auto)
             (auto-img auto)
             (+ (determine-speed auto color) (auto-x auto))
             (auto-y auto)))



(check-expect (determine-speed (make-auto 12 AUTO-IMAGE1 0 0) "green") 12)
(check-expect (determine-speed (make-auto 12 AUTO-IMAGE1 (/ WIDTH 2) 0) "green") 12)
(check-expect (determine-speed (make-auto 12 AUTO-IMAGE1 (add1 (/ WIDTH 2))
                                          0) "green") 12)
(check-expect (determine-speed (make-auto 12 AUTO-IMAGE1 0 0) "yellow") 9)
(check-expect (determine-speed (make-auto 12 AUTO-IMAGE1 (/ WIDTH 2) 0) "yellow") 9)
(check-expect (determine-speed (make-auto 12 AUTO-IMAGE1 (add1 (/ WIDTH 2))
                                          0) "yellow") 12)
(check-expect (determine-speed (make-auto 12 AUTO-IMAGE1 0 0) "red") 6)
(check-expect (determine-speed (make-auto 12 AUTO-IMAGE1 (/ WIDTH 2) 0) "red") 0)
(check-expect (determine-speed (make-auto 12 AUTO-IMAGE1 (add1 (/ WIDTH 2))
                                          0) "red") 12)

(define (determine-speed auto color)
  (cond [(or (string=? color "green")(> (auto-x auto) (/ WIDTH 2)))
         (auto-speed auto)]
        [(and (> (+ (auto-speed auto) (auto-x auto)) (/ WIDTH 2))(string=? color "red"))
         (- (/ WIDTH 2) (auto-x auto))]
        [(string=? color "yellow")
         (round (/ (* 3 (auto-speed auto)) 4))]
        [(and (string=? color "red") (= (auto-x auto) (/ WIDTH 2)))
         0]
        [else (round (/ (auto-speed auto) 2))]))



(check-expect (light-color GREEN-LENGTH) "yellow")
(check-expect (light-color (+ GREEN-LENGTH YELLOW-LENGTH)) "red")
(check-expect (light-color (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)) "green")

(define (light-color seconds)
  (cond [(< seconds GREEN-LENGTH)
         "green"]
        [(< seconds (+ GREEN-LENGTH YELLOW-LENGTH))
         "yellow"]
        [(< seconds (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))
         "red"]
        [else 
         (light-color (- seconds (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)))]))



(check-expect (render (make-clock 0 0 "green" LOA))
              (local [(define light (traffic-light (clock-color
                                                    (make-clock 0 0 "green" LOA))))
                      (define number (text (number->string
                                            (clock-seconds
                                             (make-clock 0 0 "green" LOA))) 20 "black"))]
                (overlay/offset
                 number
                 -100 (+ 30 (* 3 LIGHT-RADIUS))
                 (overlay/align/offset "middle" "top"
                                       light
                                       0 -30                          
                                       (auto-images
                                        (clock-loa (make-clock 0 0 "green" LOA)))))))

(define (render clock)
  (local [(define light (traffic-light (clock-color clock)))
          (define number (text (number->string
                                (clock-seconds clock)) 20 "black"))]
    (overlay/offset
     number
     -100 (+ 30 (* 3 LIGHT-RADIUS))
     (overlay/align/offset "middle" "top"
                           light
                           0 -30                          
                           (auto-images (clock-loa clock))))))



(check-expect (auto-images empty) MTS)
(check-expect (auto-images LOA) (place-image (auto-img (first LOA)) 
                                             (auto-x (first LOA))
                                             (auto-y (first LOA))
                                             (place-image (auto-img (second LOA))
                                                          (auto-x (second LOA))
                                                          (auto-y (second LOA))
                                                          MTS)))

(define (auto-images loa)
  (cond [(empty? loa) MTS]
        [else
         (place-image (auto-img (first loa))
                      (auto-x (first loa))
                      (auto-y (first loa))
                      (auto-images (rest loa)))]))



(check-expect (traffic-light "green")
              (above (circle LIGHT-RADIUS "outline" "red")
                     (circle LIGHT-RADIUS "outline" "yellow")
                     (circle LIGHT-RADIUS "solid" "green")))
(check-expect (traffic-light "yellow")
              (above (circle LIGHT-RADIUS "outline" "red")
                     (circle LIGHT-RADIUS "solid" "yellow")
                     (circle LIGHT-RADIUS "outline" "green")))
(check-expect (traffic-light "red")
              (above (circle LIGHT-RADIUS "solid" "red")
                     (circle LIGHT-RADIUS "outline" "yellow")
                     (circle LIGHT-RADIUS "outline" "green")))

(define (traffic-light color)
  (cond [(string=? color "green")
         (above (circle LIGHT-RADIUS "outline" "red")
                (circle LIGHT-RADIUS "outline" "yellow")
                (circle LIGHT-RADIUS "solid" "green"))]
        [(string=? "yellow" color)
         (above (circle LIGHT-RADIUS "outline" "red")
                (circle LIGHT-RADIUS "solid" "yellow")
                (circle LIGHT-RADIUS "outline" "green"))]
        [(string=? "red" color)
         (above (circle LIGHT-RADIUS "solid" "red")
                (circle LIGHT-RADIUS "outline" "yellow")
                (circle LIGHT-RADIUS "outline" "green"))]))



(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)

(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))

