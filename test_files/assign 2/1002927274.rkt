

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Ellys Gorodisch, Ian Poulsen, Daiwik Pal - Assignment #2 - Parts 1 + 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






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

(define EMPTY-BULBS
  (above (circle LIGHT-RADIUS "outline" "red")
         (circle LIGHT-RADIUS "outline" "yellow")
         (circle LIGHT-RADIUS "outline" "green")))
   
(define BULBS-GREEN
  (above (circle LIGHT-RADIUS "outline" "red")
         (circle LIGHT-RADIUS "outline" "yellow")
         (circle LIGHT-RADIUS "solid" "green")))
   
(define BULBS-YELLOW
  (above (circle LIGHT-RADIUS "outline" "red")
         (circle LIGHT-RADIUS "solid" "yellow")
         (circle LIGHT-RADIUS "outline" "green")))
   
(define BULBS-RED
  (above (circle LIGHT-RADIUS "solid" "red")
         (circle LIGHT-RADIUS "outline" "yellow")
         (circle LIGHT-RADIUS "outline" "green")))

(define FONT-SIZE 24)
(define FONT-COLOR "black")
(define TIMER-X-POS 500)
(define TIMER-Y-POS 100)
(define BILLBOARD-X-POS 200)
(define BILLBOARD-Y-POS 100)

(define AUTO-IMAGE1 .)
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)




(define-struct signal (color time-elapsed))






(define S1 (make-signal "green" 0))
(define S2 (make-signal "yellow" (* 1 TICKS-SECOND)))
(define S3 (make-signal "red" (* RED-LENGTH TICKS-SECOND)))

  


(define-struct auto (x y dx start-dx img))








(define A1 (make-auto 50 50 5 5 AUTO-IMAGE1))
(define A2 (make-auto 100 50 7 10 AUTO-IMAGE2))
(define A3 (make-auto 50 100 1 2 AUTO-IMAGE3))

  






(define LOA1 empty)
(define LOA2 (list A1))
(define LOA3 (list A1 A2 A3))

 


(define-struct billboard (text))


(define B1 (make-billboard ""))
(define B2 (make-billboard "I'm lovin' it"))
(define B3 (make-billboard "We have the meats"))

  


(define-struct traffic (light loa time board))







(define T1 (make-traffic S1 LOA1 0 B1))
(define T2 (make-traffic S2 LOA2 168 B2))
(define T3 (make-traffic S3 LOA3 308 B3))

(define START (make-traffic
               (make-signal "green" 0)
               empty 0 (make-billboard "")))

  







(define (main traffic)
  (big-bang traffic       
    (on-tick tock)        
    (to-draw render)      
    (on-key handle-key))) 








(define (tock traffic)
  (make-traffic
   (next-signal (traffic-light traffic))
   (update-autos (traffic-loa traffic) (traffic-light traffic))
   (add1 (traffic-time traffic))
   (traffic-board traffic)))





(check-expect
 (next-signal (make-signal "red" (* 0 TICKS-SECOND)))
 (make-signal "red" (add1 (* 0 TICKS-SECOND))))

(check-expect
 (next-signal (make-signal "green" (* 3 TICKS-SECOND)))
 (make-signal "green" (add1 (* 3 TICKS-SECOND))))

(check-expect
 (next-signal (make-signal "green" (* GREEN-LENGTH TICKS-SECOND)))
 (make-signal "yellow" 0))

(check-expect
 (next-signal (make-signal "yellow" (* YELLOW-LENGTH TICKS-SECOND)))
 (make-signal "red" 0))

(check-expect
 (next-signal (make-signal "red" (* RED-LENGTH TICKS-SECOND)))
 (make-signal "green" 0))



(define (next-signal s)
  (cond [(string=? (signal-color s) "red")
         (if (>= (ticks->seconds (signal-time-elapsed s)) RED-LENGTH)
             (make-signal "green" 0)
             (make-signal "red" (add1 (signal-time-elapsed s))))]
        [(string=? (signal-color s) "yellow")
         (if (>= (ticks->seconds (signal-time-elapsed s)) YELLOW-LENGTH)
             (make-signal "red" 0)
             (make-signal "yellow" (add1 (signal-time-elapsed s))))]
        [(string=? (signal-color s) "green")	
         (if (>= (ticks->seconds (signal-time-elapsed s)) GREEN-LENGTH)
             (make-signal "yellow" 0)
             (make-signal "green" (add1 (signal-time-elapsed s))))]))








(define (update-autos loa s)
  (cond [(empty? loa)
         (new-auto (random 100))]
        [(= (auto-x (first loa)) WIDTH)
         (update-autos (rest loa) s)]
        [else
         (cons (make-auto (update-auto-x (first loa) s)
                          (auto-y (first loa))
                          (update-auto-dx (first loa) s)
                          (auto-start-dx (first loa))
                          (auto-img (first loa)))
               (update-autos (rest loa) s))]))










(define (new-auto r)
  (if (<= r 4)
      (local [(define dx (+ (random 20) 1))]
        (list (make-auto 0 (+ (random (- HEIGHT 300)) 300)
                         dx dx (pick-img (random 3)))))
      empty))





(check-expect (pick-img 0) AUTO-IMAGE1)
(check-expect (pick-img 1) AUTO-IMAGE2)
(check-expect (pick-img 2) AUTO-IMAGE3)



(define (pick-img r)
  (cond [(= r 0)
         AUTO-IMAGE1]
        [(= r 1)
         AUTO-IMAGE2]
        [(= r 2)
         AUTO-IMAGE3]))





(check-expect (update-auto-x
               (make-auto (/ WIDTH 2) 400
                          5 5 AUTO-IMAGE1)
               (make-signal "red" 0))
              (+ (/ WIDTH 2) 0))
(check-expect (update-auto-x A1 S1) (+ 50 5))
(check-expect (update-auto-x A2 S2) (+ 100 (* 10 3/4)))
(check-expect (update-auto-x A3 S3) (+ 50 (* 2 1/2)))



(define (update-auto-x auto s)
  (+ (auto-x auto)
     (update-auto-dx auto s)))








(check-expect (update-auto-dx
               (make-auto (/ WIDTH 2) 400
                          5 5 AUTO-IMAGE1)
               (make-signal "red" 0))
              0)
(check-expect (update-auto-dx A1 S1) 5)
(check-expect (update-auto-dx A2 S2) (* 10 3/4))
(check-expect (update-auto-dx A3 S3) (* 2 1/2))



(define (update-auto-dx auto s)
  (cond [(string=? (signal-color s) "red")
         (cond [(<= (auto-x auto) (/ WIDTH 2))
                (if (>= (+ (auto-x auto) (auto-dx auto)) (/ WIDTH 2))
                    (- (/ WIDTH 2) (auto-x auto))
                    (* (auto-start-dx auto) 1/2))]
               [(> (auto-x auto) (/ WIDTH 2))
                (auto-start-dx auto)])]
        [(string=? (signal-color s) "yellow")
         (if (< (auto-x auto) (/ WIDTH 2))
             (* (auto-start-dx auto) 3/4)
             (auto-start-dx auto))]
        [else
         (auto-start-dx auto)]))





(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)



(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))






(check-expect
 (render T1)
 (place-image BULBS-GREEN X-POS Y-POS
              (place-image
               (text
                (number->string
                 (ticks->seconds
                  (traffic-time T1)))
                24 "black")
               500 100  (place-image
                         (text
                          (billboard-text
                           (traffic-board T1)) 24 "black")
                         200 100 MTS))))
(check-expect
 (render T2)
 (place-image BULBS-YELLOW X-POS Y-POS
              (place-image
               (text
                (number->string
                 (ticks->seconds
                  (traffic-time T2)))
                24 "black")
               500 100  (place-image
                         (text
                          (billboard-text
                           (traffic-board T2)) 24 "black")
                         200 100
                         (place-image
                          (auto-img A1)
                          (auto-x A1)
                          (auto-y A1)
                          MTS)))))

(check-expect
 (render T3)
 (place-image BULBS-RED X-POS Y-POS
              (place-image
               (text
                (number->string
                 (ticks->seconds
                  (traffic-time T3)))
                24 "black")
               500 100 (place-image
                        (text
                         (billboard-text
                          (traffic-board T3)) 24 "black")
                        200 100
                        (place-image
                         (auto-img A1)
                         (auto-x A1)
                         (auto-y A1)
                         (place-image
                          (auto-img A2)
                          (auto-x A2)
                          (auto-y A2)
                          (place-image
                           (auto-img A3)
                           (auto-x A3)
                           (auto-y A3)
                           MTS)))))))



(define (render traffic)
  (place-image (choose-bulb traffic) X-POS Y-POS
               
               (place-image
                (text
                 (number->string
                  (ticks->seconds
                   (traffic-time traffic))) 
                 FONT-SIZE FONT-COLOR)
                TIMER-X-POS TIMER-Y-POS
                
                (place-image
                 (text
                  (billboard-text(traffic-board traffic))
                  FONT-SIZE FONT-COLOR)
                 BILLBOARD-X-POS BILLBOARD-Y-POS
                 (render-autos (traffic-loa traffic))))))







(check-expect (choose-bulb T1) BULBS-GREEN)
(check-expect (choose-bulb T2) BULBS-YELLOW)
(check-expect (choose-bulb T3) BULBS-RED)


(define (choose-bulb traffic)
  (cond [(string=? (signal-color (traffic-light traffic)) "green")
         BULBS-GREEN]
        [(string=? (signal-color (traffic-light traffic)) "yellow")
         BULBS-YELLOW]
        [(string=? (signal-color (traffic-light traffic)) "red")
         BULBS-RED]))





(check-expect (render-autos LOA1) MTS)
(check-expect (render-autos LOA2)
              (place-image (auto-img A1)
                           (auto-x A1)
                           (auto-y A1)
                           MTS))
(check-expect (render-autos LOA3)
              (place-image (auto-img A1)
                           (auto-x A1)
                           (auto-y A1)
                           
                           (place-image
                            (auto-img A2)
                            (auto-x A2)
                            (auto-y A2)
                            
                            (place-image
                             (auto-img A3)
                             (auto-x A3)
                             (auto-y A3)
                             MTS))))



(define (render-autos loa)
  (cond [(empty? loa) MTS]
        [else
         (place-image (auto-img (first loa))
                      (auto-x (first loa))
                      (auto-y (first loa))
                      (render-autos (rest loa)))]))










(check-expect
 (handle-key T1 "a")
 (make-traffic (traffic-light T1)
               (traffic-loa T1)
               (traffic-time T1)
               (make-billboard
                (string-append
                 (billboard-text
                  (traffic-board T1))
                 "a"))))
(check-expect
 (handle-key T2 "\r")
 (make-traffic (traffic-light T2)
               (traffic-loa T2)
               (traffic-time T2)
               (make-billboard
                (string-append
                 (billboard-text
                  (traffic-board T2))
                 "\n"))))
(check-expect
 (handle-key T3 "\b")
 (make-traffic (traffic-light T3)
               (traffic-loa T3)
               (traffic-time T3)
               (make-billboard
                (substring
                 (billboard-text
                  (traffic-board T3))
                 0
                 (sub1 (string-length
                        (billboard-text
                         (traffic-board T3))))))))



(define (handle-key traffic ke)
  (make-traffic (traffic-light traffic)
                (traffic-loa traffic)
                (traffic-time traffic)
                (make-billboard
                 (cond [(string=? ke "\b")
                        (if (> (string-length
                                (billboard-text
                                 (traffic-board traffic)))
                               0)
                            (substring
                             (billboard-text (traffic-board traffic))
                             0
                             (sub1
                              (string-length
                               (billboard-text
                                (traffic-board traffic)))))
                            "")]
                       
                       [(string=? ke "\r")
                        (string-append
                         (billboard-text
                          (traffic-board traffic)) "\n" )]
                       
                       [(string=? ke "\t") ""]
                       
                       [(= (string-length ke) 1)
                        (string-append
                         (billboard-text (traffic-board traffic))
                         ke)]
                       
                       [else
                        (billboard-text (traffic-board traffic))]))))