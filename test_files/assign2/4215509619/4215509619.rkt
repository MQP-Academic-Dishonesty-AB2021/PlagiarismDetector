

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 2 Connor Ehrensperger and Zachary Rioux|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MAX-SPEED 10)
(define MTS (empty-scene WIDTH HEIGHT))

(define BLANK (square 0 "solid" "white"))

(define TICKS-SECOND 28) 






(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 
(define TOTAL-LENGTH (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))

(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)








(define GREEN 0)  
(define YELLOW 1) 
(define RED 2)    
 




(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   
   (circle LIGHT-RADIUS "solid" "yellow")
   
   
   (circle LIGHT-RADIUS "solid" "green")))





(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))

(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)

(define (main ws)
  (big-bang ws
    (on-tick clock)
    (to-draw draw)))












(define-struct ws (LOA Ticks Color))
(define START (make-ws empty 0 0))


(define-struct auto (Img Speed-Current Speed-Normal Pos-x Pos-y))









(define auto1 (make-auto AUTO-IMAGE1 1 1 0 450))
(define auto2 (make-auto AUTO-IMAGE2 3 3 X-POS 550))
(define auto3 (make-auto AUTO-IMAGE3 6 6 WIDTH 500))
(define auto4 (make-auto AUTO-IMAGE1 1 1 X-POS 450))
(define auto5 (make-auto AUTO-IMAGE1 1 1 (+ WIDTH 1) 500))

 





(define loa1 empty)
(define loa2 (list auto1))
(define loa3 (list auto1 auto2 auto3))
(define loa4 (list auto4))
(define loa5 (list auto1 auto5))

 


(define WS0 (make-ws loa2 0 GREEN))
(define WS1 (make-ws loa1 0 GREEN))
(define WS2 (make-ws loa2 28 0))
(define WS3 (make-ws loa3 (* 28 GREEN-LENGTH) YELLOW))
(define WS4 (make-ws loa4
                     (* 28 (+ GREEN-LENGTH YELLOW-LENGTH)) RED))



 



(check-expect (clock WS1)
              (make-ws
               (update-speeds (move-autos (update-auto loa1)) WS1)
               (+ (ws-Ticks WS1) 1)
               0))
(check-expect (clock WS3)
              (make-ws
               (update-speeds (move-autos (update-auto loa3)) WS3)
               (+ (ws-Ticks WS3) 1)
               1))
(check-expect (clock WS4)
              (make-ws
               (update-speeds (move-autos (update-auto loa4)) WS4)
               (+ (ws-Ticks WS4) 1)
               2))





(define (clock ws)
  (make-ws (update-speeds
            (move-autos
             (update-auto (ws-LOA ws))) ws)
           (+ (ws-Ticks ws) 1)
           (cond
             [(<
               (counter (ticks->seconds (ws-Ticks ws)))
               GREEN-LENGTH) GREEN]
             [(<
               (counter (ticks->seconds (ws-Ticks ws)))
               (+ GREEN-LENGTH YELLOW-LENGTH)) YELLOW]
             [else RED])))




(check-expect (counter 11) 0)
(check-expect (counter 15) 4)

(define (counter seconds)
  (remainder seconds TOTAL-LENGTH))
 





(check-expect (draw WS1)
              (place-image
               (above
                (circle LIGHT-RADIUS "outline" "red")
                (circle LIGHT-RADIUS "outline" "yellow")
                (circle LIGHT-RADIUS "solid" "green"))
               X-POS
               Y-POS
               (place-image
                (text (number->string
                       (ticks->seconds (ws-Ticks WS1))) 20 "black")
                (+ X-POS 80)
                Y-POS
                MTS)))

(check-expect (draw WS4)
              (place-image
               (above
                (circle LIGHT-RADIUS "solid" "red")
                (circle LIGHT-RADIUS "outline" "yellow")
                (circle LIGHT-RADIUS "outline" "green"))
               X-POS
               Y-POS
               (place-image
                (text (number->string
                       (ticks->seconds (ws-Ticks WS4))) 20 "black")
                (+ X-POS 80)
                Y-POS
                (draw-autos loa4))))

(check-expect (draw WS3)
              (place-image
               (above
                (circle LIGHT-RADIUS "outline" "red")
                (circle LIGHT-RADIUS "solid" "yellow")
                (circle LIGHT-RADIUS "outline" "green"))
               X-POS
               Y-POS

               (place-image
                (text (number->string (ticks->seconds (ws-Ticks WS3)))
                      20 "black")
                (+ X-POS 80)
                Y-POS
                (draw-autos loa3))))


(define (draw ws)
  (place-image
   (next-light (ws-Color ws))
   X-POS Y-POS
   (place-image (text
                 (number->string
                  (ticks->seconds (ws-Ticks ws))) 20 "black")
                (+ X-POS 80) Y-POS (draw-autos (ws-LOA ws)))))





(check-expect (next-light GREEN)
              (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "solid" "green")))

(check-expect (next-light YELLOW)
              (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "solid" "yellow")
               (circle LIGHT-RADIUS "outline" "green")))

(check-expect (next-light RED)
              (above
               (circle LIGHT-RADIUS "solid" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "outline" "green")))





(define (next-light color)
  (cond [(= color 0) (above
                      (circle LIGHT-RADIUS "outline" "red")
                      (circle LIGHT-RADIUS "outline" "yellow")
                      (circle LIGHT-RADIUS "solid" "green"))]
        [(= color 1) (above
                      (circle LIGHT-RADIUS "outline" "red")
                      (circle LIGHT-RADIUS "solid" "yellow")
                      (circle LIGHT-RADIUS "outline" "green"))]
        [(= color 2) (above
                      (circle LIGHT-RADIUS "solid" "red")
                      (circle LIGHT-RADIUS "outline" "yellow")
                      (circle LIGHT-RADIUS "outline" "green"))]))




(check-expect (draw-autos loa1) MTS)
(check-expect (draw-autos loa2) (place-image (auto-Img auto1)
                                             (auto-Pos-x auto1)
                                             (auto-Pos-y auto1)
                                             MTS))
(check-expect (draw-autos loa3)
              (place-image
               (auto-Img auto1)
               (auto-Pos-x auto1)
               (auto-Pos-y auto1)
               (place-image
                (auto-Img auto2)
                (auto-Pos-x auto2)
                (auto-Pos-y auto2)
                (place-image
                 (auto-Img auto3)
                 (auto-Pos-x auto3)
                 (auto-Pos-y auto3) MTS))))



(define (draw-autos loa)
  (cond
    [(empty? loa) MTS]
    [else
     (place-image (auto-Img (first loa))
                  (auto-Pos-x (first loa))
                  (auto-Pos-y (first loa))
                  (draw-autos (rest loa)))]))





(check-expect (move-autos loa1) empty)


(check-expect (move-autos loa2) (list
                                 (make-auto
                                  (auto-Img auto1)
                                  (auto-Speed-Current auto1)
                                  (auto-Speed-Normal auto1)
                                  (+ (auto-Pos-x auto1)
                                     (auto-Speed-Current auto1))
                                  (auto-Pos-y auto1))))
(check-expect (move-autos loa3)
              (list
               (make-auto (auto-Img auto1)
                          (auto-Speed-Current auto1)
                          (auto-Speed-Normal auto1)
                          (+ (auto-Pos-x auto1)
                             (auto-Speed-Current auto1))
                          (auto-Pos-y auto1))
               (make-auto (auto-Img auto2)
                          (auto-Speed-Current auto2)
                          (auto-Speed-Normal auto2)
                          (+ (auto-Pos-x auto2)
                             (auto-Speed-Current auto2))
                          (auto-Pos-y auto2))
               (make-auto (auto-Img auto3)
                          (auto-Speed-Current auto3)
                          (auto-Speed-Normal auto3)
                          (+ (auto-Pos-x auto3)
                             (auto-Speed-Current auto3))
                          (auto-Pos-y auto3))))



(define (move-autos loa)
  (cond
    [(empty? loa) empty]
    [else
     (cons
      (make-auto (auto-Img (first loa))
                 (auto-Speed-Current (first loa))
                 (auto-Speed-Normal (first loa))
                 (+ (auto-Pos-x (first loa))
                    (auto-Speed-Current (first loa)))
                 (auto-Pos-y (first loa)))
      (move-autos (rest loa)))]))





(check-expect (update-speed auto1 RED)
              (make-auto (auto-Img auto1)
                         (* (auto-Speed-Current auto1) 0.5)
                         (auto-Speed-Normal auto1)
                         (auto-Pos-x auto1)
                         (auto-Pos-y auto1)))

(check-expect (update-speed auto1 YELLOW)
              (make-auto (auto-Img auto1)
                         (* (auto-Speed-Current auto1) 0.75)
                         (auto-Speed-Normal auto1)
                         (auto-Pos-x auto1)
                         (auto-Pos-y auto1)))

(check-expect (update-speed auto1 GREEN)
              (make-auto (auto-Img auto1)
                         (* (auto-Speed-Current auto1) 1)
                         (auto-Speed-Normal auto1)
                         (auto-Pos-x auto1)
                         (auto-Pos-y auto1)))





(define (update-speed auto color)
  (make-auto (auto-Img auto)
             (* (speed-multiplier color) (auto-Speed-Normal auto))
             (auto-Speed-Normal auto)
             (auto-Pos-x auto)
             (auto-Pos-y auto)))




(check-expect (speed-multiplier RED) 0.5)
(check-expect (speed-multiplier YELLOW) 0.75)
(check-expect (speed-multiplier GREEN) 1)





(define (speed-multiplier color)
  (cond [(= color GREEN) 1]
        [(= color YELLOW) 0.75]
        [(= color RED) 0.5]))



(check-expect (update-speeds loa1 WS1) empty)
(check-expect (update-speeds loa2 WS3)
              (list (make-auto (auto-Img auto1)
                               (* 0.75 (auto-Speed-Normal auto1))
                               (auto-Speed-Normal auto1)
                               (auto-Pos-x auto1)
                               (auto-Pos-y auto1))))

(check-expect (update-speeds loa3 WS4)
              (list
               (make-auto
                (auto-Img auto1)
                (* 0.5 (auto-Speed-Normal auto1))
                (auto-Speed-Normal auto1)
                (auto-Pos-x auto1)
                (auto-Pos-y auto1))
               (make-auto
                (auto-Img auto2)
                (* 0 (auto-Speed-Normal auto2))
                (auto-Speed-Normal auto2)
                (auto-Pos-x auto2)
                (auto-Pos-y auto2))
               (make-auto
                (auto-Img auto3)
                (auto-Speed-Normal auto3)
                (auto-Speed-Normal auto3)
                (auto-Pos-x auto3)
                (auto-Pos-y auto3))))
(check-expect (update-speeds loa4 WS4)
              (list (make-auto (auto-Img auto4)
                               (* 0 (auto-Speed-Normal auto4))
                               (auto-Speed-Normal auto4)
                               (auto-Pos-x auto4)
                               (auto-Pos-y auto4))))

(check-expect (update-speeds (ws-LOA WS0) WS0)
              (list (first (ws-LOA WS0))))




(define (update-speeds loa ws) 
  (cond
    [(empty? loa) empty]
    [(= (ws-Color ws) GREEN)
     (cons (make-auto (auto-Img (first loa))
                      (auto-Speed-Normal (first loa))
                      (auto-Speed-Normal (first loa))
                      (auto-Pos-x (first loa))
                      (auto-Pos-y (first loa)))
           (update-speeds (rest loa) ws))]
    [(< (abs (- (auto-Pos-x (first loa)) X-POS)) 50)
     (if (= (ws-Color ws) RED)
         (cons (make-auto (auto-Img (first loa))
                          0
                          (auto-Speed-Normal (first loa))
                          (auto-Pos-x (first loa))
                          (auto-Pos-y (first loa)))
               (update-speeds (rest loa) ws))
         (cons (make-auto (auto-Img (first loa))
                          (auto-Speed-Current (first loa))
                          (auto-Speed-Normal (first loa))
                          (auto-Pos-x (first loa))
                          (auto-Pos-y (first loa)))
               (update-speeds (rest loa) ws)))]
    [(< (auto-Pos-x (first loa)) X-POS)
     (cons
      (update-speed (first loa) (ws-Color ws))
      (update-speeds (rest loa) ws))]
    [else
     (cons (make-auto (auto-Img (first loa))
                      (auto-Speed-Normal (first loa))
                      (auto-Speed-Normal (first loa))
                      (auto-Pos-x (first loa))
                      (auto-Pos-y (first loa)))
           (update-speeds (rest loa) ws))]))





(define (update-auto loa)
  (if (<= (random 100) 4)     
      (local
        [(define SPEED (+ (random MAX-SPEED) 1))]
        (cons (make-auto (pick-image (random 3))
                         SPEED
                         SPEED
                         0
                         (+ (random 300) 300))
              (delete-auto loa)))
      (delete-auto loa)))
         





(check-expect (delete-auto loa1) empty)
(check-expect (delete-auto loa5) (list auto1))
(check-expect (delete-auto (list auto5)) empty)

(define (delete-auto loa)
  (cond
    [(empty? loa) empty]
    [else
     (if (< (auto-Pos-x (first loa)) WIDTH)
         (cons (first loa) (delete-auto (rest loa)))
         (delete-auto (rest loa)))]))
 








(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))
(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)