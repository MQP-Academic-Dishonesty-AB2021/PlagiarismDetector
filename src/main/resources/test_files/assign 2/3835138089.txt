

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1102 assignment 2_part 1 and 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





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



(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))







(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)










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














(define-struct traffic-signal (color total-ticks color-ticks))

(define TS1 (make-traffic-signal "red"
                                 (* TICKS-SECOND (- RED-LENGTH 1))
                                 (* TICKS-SECOND (- RED-LENGTH 1))))
(define TS2 (make-traffic-signal "yellow" TICKS-SECOND TICKS-SECOND))
(define TS3 (make-traffic-signal "green"
                                 (* TICKS-SECOND GREEN-LENGTH)
                                 (* TICKS-SECOND GREEN-LENGTH)))
(define TS4 (make-traffic-signal "red"
                                 (* TICKS-SECOND RED-LENGTH)
                                 (* TICKS-SECOND RED-LENGTH)))
(define TS5 (make-traffic-signal "yellow"
                                 (* TICKS-SECOND YELLOW-LENGTH)
                                 (* TICKS-SECOND YELLOW-LENGTH)))

(define T-START (make-traffic-signal "green" 0 0))


 











 


(check-expect (advance-signal TS1) 
              (make-traffic-signal
               (traffic-signal-color TS1)
               (+ (traffic-signal-total-ticks TS1) 1)
               (+ (traffic-signal-color-ticks TS1) 1)))

(check-expect (advance-signal TS2) 
              (make-traffic-signal
               (traffic-signal-color TS2)
               (+ (traffic-signal-total-ticks TS2) 1)
               (+ (traffic-signal-color-ticks TS2) 1)))

(check-expect (advance-signal TS3) 
              (make-traffic-signal
               "yellow"
               (+ (traffic-signal-total-ticks TS3) 1)
               1))

(check-expect (advance-signal TS4) 
              (make-traffic-signal
               "green"
               (+ (traffic-signal-total-ticks TS4) 1)
               1))

(check-expect (advance-signal TS5) 
              (make-traffic-signal
               "red"
               (+ (traffic-signal-total-ticks TS5) 1)
               1))



(define (advance-signal ts)
  (cond [(and (string=? "red" (traffic-signal-color ts)) (>= (traffic-signal-color-ticks ts) (* TICKS-SECOND RED-LENGTH))) 
         (make-traffic-signal "green" (+ (traffic-signal-total-ticks ts) 1) 1)]
        [(and (string=? "yellow" (traffic-signal-color ts)) (>= (traffic-signal-color-ticks ts) (* TICKS-SECOND YELLOW-LENGTH)))
         (make-traffic-signal "red" (+ (traffic-signal-total-ticks ts) 1) 1)]
        [(and (string=? "green" (traffic-signal-color ts)) (>= (traffic-signal-color-ticks ts) (* TICKS-SECOND GREEN-LENGTH)))
         (make-traffic-signal "yellow" (+ (traffic-signal-total-ticks ts) 1) 1)]
        [else 
         (make-traffic-signal (traffic-signal-color ts) (+ (traffic-signal-total-ticks ts) 1) (+ 1 (traffic-signal-color-ticks ts)))]))


(check-expect (render-ts TS1)
              (place-image
               (text (number->string (ticks->seconds (traffic-signal-total-ticks TS1))) 20 "black")
               (* 3/4 WIDTH)
               (* 1/4 HEIGHT)
               (place-image
                (render-color TS1)
                X-POS
                Y-POS
                MTS)))

(check-expect (render-ts TS2)
              (place-image
               (text (number->string (ticks->seconds (traffic-signal-total-ticks TS2))) 20 "black")
               (* 3/4 WIDTH)
               (* 1/4 HEIGHT)
               (place-image
                (render-color TS2)
                X-POS
                Y-POS
                MTS)))

(check-expect (render-ts TS3)
              (place-image
               (text (number->string (ticks->seconds (traffic-signal-total-ticks TS3))) 20 "black")
               (* 3/4 WIDTH)
               (* 1/4 HEIGHT)
               (place-image
                (render-color TS3)
                X-POS
                Y-POS
                MTS)))



(define (render-ts ts)
  (place-image
   (text (number->string (ticks->seconds (traffic-signal-total-ticks ts))) 20 "black")
   (* 3/4 WIDTH)
   (* 1/4 HEIGHT)
   (place-image
    (render-color ts)
    X-POS
    Y-POS
    MTS)))




(define (render-color ts)
  (cond [(string=? "red" (traffic-signal-color ts))
         (render-red ts)] 
        [(string=? "yellow" (traffic-signal-color ts))
         (render-yellow ts)]
        [else
         (render-green ts)]))




(define (render-red ts) 
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))




(define (render-yellow ts)
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))




(define (render-green ts)
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))











(define-struct auto (x y speed image))



(define-struct world-state (ts autos))


(define TOP (/ HEIGHT 2))
(define BOTTOM HEIGHT)

(define BEFORE-YELLOW 0.75)
(define BEFORE-RED 0.5)

(define MAX-SPEED 15)

(define RED-RANGE 10) 

(define BLANK (square 0 "solid" "white"))


(define START
  (make-world-state T-START
                    (list
                     (make-auto 0 400 4 AUTO-IMAGE1)
                     (make-auto 0 500 2 AUTO-IMAGE2))))


(define LOA1 (list (make-auto 0 400 8 AUTO-IMAGE1)))
(define LOA2 (list
              (make-auto 0 400 8 AUTO-IMAGE1)
              (make-auto 450 500 4 AUTO-IMAGE2)))
(define LOA3 (list
              (make-auto 0 400 8 AUTO-IMAGE1)
              (make-auto 450 500 4 AUTO-IMAGE2)
              (make-auto 400 450 6 AUTO-IMAGE1)))
(define LOA4 (list (make-auto (+ WIDTH 1) 400 2 AUTO-IMAGE1)))

(define WS1 (make-world-state TS1 LOA1))
(define WS2 (make-world-state TS2 LOA2))
(define WS3 (make-world-state TS3 LOA3))
(define WS4 (make-world-state TS1 LOA3))
(define WS5 (make-world-state TS1 LOA4))
(define WS6 (make-world-state TS2 LOA4))
(define WS7 (make-world-state TS3 LOA4))







 

 

 


(define (main ws)
  (big-bang ws
    (on-tick advance-ws)
    (to-draw render-ws)))



 

 

 

 
 
 
 

 



(define (advance-ws ws)
  (make-world-state
   (advance-signal (world-state-ts ws))
   (if (< (random 100) 5) 
       (cons (make-auto
              0
              (+ (random (/ HEIGHT 2)) TOP)
              (+ 1 (random MAX-SPEED)) 
              (pick-image (random 3)))
             (advance-autos ws))
       (advance-autos ws))))







































(check-expect (advance-autos WS1)
              (list (make-auto
                     (+ (* BEFORE-RED (auto-speed (first (world-state-autos WS1)))) (auto-x (first (world-state-autos WS1))))
                     (auto-y (first (world-state-autos WS1)))
                     (auto-speed (first (world-state-autos WS1)))
                     (auto-image (first (world-state-autos WS1))))))

(check-expect (advance-autos WS2)
              (list (make-auto
                     (+ (* BEFORE-YELLOW (auto-speed (first (world-state-autos WS2)))) (auto-x (first (world-state-autos WS2))))
                     (auto-y (first (world-state-autos WS2)))
                     (auto-speed (first (world-state-autos WS2)))
                     (auto-image (first (world-state-autos WS2))))
                    (make-auto
                     (+ (auto-speed (first (rest (world-state-autos WS2)))) (auto-x (first (rest (world-state-autos WS2)))))
                     (auto-y (first (rest (world-state-autos WS2))))
                     (auto-speed (first (rest (world-state-autos WS2))))
                     (auto-image (first (rest (world-state-autos WS2)))))))

(check-expect (advance-autos WS3)
              (list (make-auto
                     (+ (auto-speed (first (world-state-autos WS3))) (auto-x (first (world-state-autos WS3))))
                     (auto-y (first (world-state-autos WS3)))
                     (auto-speed (first (world-state-autos WS3)))
                     (auto-image (first (world-state-autos WS3))))
                    (make-auto
                     (+ (auto-speed (first (rest (world-state-autos WS3)))) (auto-x (first (rest (world-state-autos WS3)))))
                     (auto-y (first (rest (world-state-autos WS3))))
                     (auto-speed (first (rest (world-state-autos WS3))))
                     (auto-image (first (rest (world-state-autos WS3)))))
                    (make-auto
                     (+ (auto-speed (first (rest (rest (world-state-autos WS3))))) (auto-x (first (rest (rest (world-state-autos WS3))))))
                     (auto-y (first (rest (rest (world-state-autos WS3)))))
                     (auto-speed (first (rest (rest (world-state-autos WS3)))))
                     (auto-image (first (rest (rest (world-state-autos WS3))))))))

(check-expect (advance-autos WS4)
              (list (make-auto
                     (+ (* BEFORE-RED (auto-speed (first (world-state-autos WS4)))) (auto-x (first (world-state-autos WS4))))
                     (auto-y (first (world-state-autos WS4)))
                     (auto-speed (first (world-state-autos WS4)))
                     (auto-image (first (world-state-autos WS4))))
                    (make-auto
                     (+ (auto-speed (first (rest (world-state-autos WS4)))) (auto-x (first (rest (world-state-autos WS4)))))
                     (auto-y (first (rest (world-state-autos WS4))))
                     (auto-speed (first (rest (world-state-autos WS4))))
                     (auto-image (first (rest (world-state-autos WS4)))))
                    (first (rest (rest (world-state-autos WS4))))))

(check-expect (advance-autos WS5) empty)
(check-expect (advance-autos WS6) empty)
(check-expect (advance-autos WS7) empty)




(define (advance-autos ws)
  (cond [(string=? "red" (traffic-signal-color (world-state-ts ws)))
         (advance-autos-red (world-state-autos ws))]
        [(string=? "yellow" (traffic-signal-color (world-state-ts ws)))
         (advance-autos-yellow (world-state-autos ws))]
        [else
         (advance-autos-green (world-state-autos ws))]))




(define (advance-autos-red loa)
  (cond [(empty? loa) loa]
        [else
         (if (< (auto-x (first loa)) WIDTH)
             (cons 
              (cond [(< (auto-x (first loa)) (- X-POS RED-RANGE)) 
                     (make-auto
                      (+ (* BEFORE-RED (auto-speed (first loa))) (auto-x (first loa)))
                      (auto-y (first loa))
                      (auto-speed (first loa))
                      (auto-image (first loa)))]
                    [(and (<= (auto-x (first loa)) X-POS) (>= (auto-x (first loa)) (- X-POS RED-RANGE))) 
                     (first loa)]
                    [else 
                     (make-auto
                      (+ (auto-speed (first loa)) (auto-x (first loa)))
                      (auto-y (first loa))
                      (auto-speed (first loa))
                      (auto-image (first loa)))])
              (advance-autos-red (rest loa)))
             (advance-autos-red (rest loa)))])) 
                



(define (advance-autos-yellow loa)
  (cond [(empty? loa) loa]
        [else
         (if (< (auto-x (first loa)) WIDTH)
             (cons 
              (cond [(<= (auto-x (first loa)) X-POS) 
                     (make-auto
                      (+ (* BEFORE-YELLOW (auto-speed (first loa))) (auto-x (first loa)))
                      (auto-y (first loa))
                      (auto-speed (first loa))
                      (auto-image (first loa)))]
                    [else 
                     (make-auto
                      (+ (auto-speed (first loa)) (auto-x (first loa)))
                      (auto-y (first loa))
                      (auto-speed (first loa))
                      (auto-image (first loa)))])
              (advance-autos-yellow (rest loa)))
             (advance-autos-yellow (rest loa)))])) 




(define (advance-autos-green loa)
  (cond [(empty? loa) loa]
        [else
         (if (< (auto-x (first loa)) WIDTH)
             (cons 
              (make-auto
               (+ (auto-speed (first loa)) (auto-x (first loa)))
               (auto-y (first loa))
               (auto-speed (first loa))
               (auto-image (first loa)))
              (advance-autos-green (rest loa)))
             (advance-autos-green (rest loa)))])) 


(check-expect (render-ws WS1)
              (place-image
               (auto-image (first (world-state-autos WS1)))
               (auto-x (first (world-state-autos WS1)))
               (auto-y (first (world-state-autos WS1)))
               (render-ts (world-state-ts WS1))))

(check-expect (render-ws WS2)
              (place-image
               (auto-image (first (world-state-autos WS2)))
               (auto-x (first (world-state-autos WS2)))
               (auto-y (first (world-state-autos WS2)))
               (place-image
                (auto-image (first (rest (world-state-autos WS2))))
                (auto-x (first (rest (world-state-autos WS2))))
                (auto-y (first (rest (world-state-autos WS2))))
                (render-ts (world-state-ts WS2)))))



(define (render-ws ws)
  (render-autos (world-state-autos ws)
                (render-ts (world-state-ts ws))))




(define (render-autos loa img)
  (cond [(empty? loa) img]
        [else
         (place-image 
          (auto-image (first loa)) 
          (auto-x (first loa))
          (auto-y (first loa))
          (render-autos (rest loa) img))]))


