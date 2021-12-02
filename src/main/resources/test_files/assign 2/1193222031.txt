

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Lab 02 Parts 1 and 2 CS1102 - Benjamin Antupit and Grace Phillips|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 800) 
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT))

(define MIN-SPEED (min 4 WIDTH)) 
(define MAX-SPEED (ceiling (/ WIDTH 64))) 

(define LIGHT-RADIUS 40) 
(define TEXT-SIZE (/ HEIGHT 20)) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 
(define SUM-LENGTH (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))

(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 

(define SIGNAL-X-POS (+ X-POS LIGHT-RADIUS)) 
                                             

(define COUNTER-X (+ X-POS (* 2.5 LIGHT-RADIUS))) 
                                                  

(define X-POS-BOARD (/ (-  X-POS LIGHT-RADIUS) 2)) 
                                                   
(define Y-POS-BOARD (/ HEIGHT 5)) 

(define TICKS-SECOND 28) 

(define CHANCE-OF-NEW-AUTO 5) 


(define YELLOW-COEFF 3/4)
(define RED-COEFF 1/2)







(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))

(define BULBS-OFF
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))



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








(define-struct ws (ticks autos billboards))
 

(define START (make-ws 0 empty (list "")))
(define WS1 (make-ws 1 empty (list "")))
(define WS2 (make-ws (- (* TICKS-SECOND  GREEN-LENGTH) 1) empty (list "")))
(define WS3 (make-ws (* TICKS-SECOND GREEN-LENGTH) empty (list "")))
(define WS4
  (make-ws (- (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH)) 1) empty
           (list "")))
(define WS5
  (make-ws (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH)) empty (list "")))
(define WS6
  (make-ws (- (* TICKS-SECOND (+ RED-LENGTH (+ YELLOW-LENGTH GREEN-LENGTH))) 1)
           empty (list "")))
(define WS7
  (make-ws (* TICKS-SECOND (+ RED-LENGTH (+ YELLOW-LENGTH GREEN-LENGTH)))
           empty (list "")))
(define WS2LONG (make-ws 123456789012345678901234567890 empty (list "")))



(define (is-green? ticks)
  (< (modulo (ticks->seconds ticks) SUM-LENGTH) GREEN-LENGTH))

(check-expect (is-green? (ws-ticks START)) true)
(check-expect (is-green? (ws-ticks WS1)) true)
(check-expect (is-green? (ws-ticks WS2)) true)
(check-expect (is-green? (ws-ticks WS3)) false)
(check-expect (is-green? (ws-ticks WS4)) false)
(check-expect (is-green? (ws-ticks WS5)) false)
(check-expect (is-green? (ws-ticks WS6)) false)
(check-expect (is-green? (ws-ticks WS7)) true)



(define (is-yellow? ticks)
  (and (>= (modulo (ticks->seconds ticks) SUM-LENGTH) GREEN-LENGTH)
       (< (modulo (ticks->seconds ticks) SUM-LENGTH)
          (+ GREEN-LENGTH YELLOW-LENGTH))))

(check-expect (is-yellow? (ws-ticks START)) false)
(check-expect (is-yellow? (ws-ticks WS1)) false)
(check-expect (is-yellow? (ws-ticks WS2)) false)
(check-expect (is-yellow? (ws-ticks WS3)) true)
(check-expect (is-yellow? (ws-ticks WS4)) true)
(check-expect (is-yellow? (ws-ticks WS5)) false)
(check-expect (is-yellow? (ws-ticks WS6)) false)
(check-expect (is-yellow? (ws-ticks WS7)) false)



(define (is-red? ticks)
  (and (>= (modulo (ticks->seconds ticks) SUM-LENGTH)
           (+ GREEN-LENGTH YELLOW-LENGTH))
       (< (modulo (ticks->seconds ticks) SUM-LENGTH) SUM-LENGTH)))

(check-expect (is-red? (ws-ticks START)) false)
(check-expect (is-red? (ws-ticks WS1)) false)
(check-expect (is-red? (ws-ticks WS2)) false)
(check-expect (is-red? (ws-ticks WS3)) false)
(check-expect (is-red? (ws-ticks WS4)) false)
(check-expect (is-red? (ws-ticks WS5)) true)
(check-expect (is-red? (ws-ticks WS6)) true)
(check-expect (is-red? (ws-ticks WS7)) false)







(define LOS0-0 empty) 
(define LOS0 (list ""))
(define LOS1 (list "Before we go any further, let's talk about a VPN!"))

 



(define LOB0 (list ""))
(define LOB1 (list "Let's talk about a VPN!"))
(define LOB2
  (list "I hate the real, physical billboard that says 'I make sexy teeth.'"
        "Please make it go away. >:("))
(define LOB3 (list "I CAN DO ANYTHING!" "🃏" "1...2...3..." "BYE \nBYE!"))

 





(define-struct auto (x y speed img-index))










(define A1 (make-auto 0 (- Y-POS (* 3 LIGHT-RADIUS)) MIN-SPEED 0))

(define A2 (make-auto (- (/ WIDTH 2) (/ WIDTH 16))
                      (- (/ WIDTH 7) (- Y-POS (* 3 LIGHT-RADIUS))) MAX-SPEED 1))

(define A3 (make-auto (/ WIDTH 20) (- (/ WIDTH (/ 8 3))
                                      (- Y-POS (* 3 LIGHT-RADIUS)))
                      (+ (/ WIDTH 20) MIN-SPEED) 2))

(define A4-poff (make-auto (+ 2 WIDTH) (- (/ WIDTH (/ 8 3))
                                          (- Y-POS (* 3 LIGHT-RADIUS)))
                           (+ (/ WIDTH 20) MIN-SPEED) 2))

(define A5-off (make-auto (+ (image-width AUTO-IMAGE1) WIDTH)
                          (- (/ WIDTH (/ 8 3)) (- Y-POS (* 3 LIGHT-RADIUS)))
                          (+ (/ WIDTH 20) MIN-SPEED) 2))

(define A6-hit-mid (make-auto (- X-POS (/ WIDTH 40))
                              (- (/ WIDTH 16) (- Y-POS (* 3 LIGHT-RADIUS)))
                              (/ WIDTH 20) 0))

(define A7-pass-mid (make-auto (- (/ WIDTH 2) (/ WIDTH 80))
                               (- (/ WIDTH 16) (- Y-POS (* 3 LIGHT-RADIUS)))
                               (/ WIDTH 20) 0))

(define A8-past-mid (make-auto (+ (/ WIDTH 2) (/ WIDTH 20))
                               (- (/ WIDTH 16) (- Y-POS (* 3 LIGHT-RADIUS)))
                               (/ WIDTH 20) 0))


 





 





(define (is-off-screen? auto) 
  (> (auto-x auto) (+ WIDTH (/ (image-width AUTO-IMAGE1) 2))))

(check-expect (is-off-screen? A1) false) 
(check-expect (is-off-screen? A4-poff) false) 
(check-expect (is-off-screen? A5-off) true) 









(define (make-random-auto chance-ceil)
  (if (< (random 100) chance-ceil)
      (list (make-auto 
             0
             (+ (random (round
                         (- HEIGHT (+ Y-POS (* 3 LIGHT-RADIUS)
                                      (* 2 (image-height AUTO-IMAGE1))))))
                (+ Y-POS (* 3 LIGHT-RADIUS) (image-height AUTO-IMAGE1)))
             (+ (random MAX-SPEED) MIN-SPEED)
             (random 3)))
      empty))

(check-random (make-random-auto 101) (list (make-auto 
             (* 0 (random 1)) 
                              
             (+ (random (round
                         (- HEIGHT (+ Y-POS (* 3 LIGHT-RADIUS)
                                      (* 2 (image-height AUTO-IMAGE1))))))
                (+ Y-POS (* 3 LIGHT-RADIUS) (image-height AUTO-IMAGE1)))
             (+ (random MAX-SPEED) MIN-SPEED)
             (random 3))))

(check-expect (make-random-auto 0) empty)



(define (calc-movement auto ticks) 
  (cond [(or (is-green? ticks) (> (auto-x auto) X-POS))
         
         (move-auto auto (auto-speed auto))]
        [(is-yellow? ticks)
         
         (move-auto auto (* YELLOW-COEFF (auto-speed auto)))]
        [(and (is-red? ticks) (> (+ (auto-x auto) (auto-speed auto)) X-POS))
         
         (move-auto auto (- X-POS (auto-x auto)))]
        [else
         
         (move-auto auto (* RED-COEFF (auto-speed auto)))]))

(check-expect (calc-movement A1 (- (* TICKS-SECOND  GREEN-LENGTH) 1)) 
              (make-auto (+ (auto-speed A1) (auto-x A1)) (auto-y A1)
                         (auto-speed A1) (auto-img-index A1))) 
(check-expect (calc-movement A2 (* TICKS-SECOND GREEN-LENGTH)) 
              (make-auto (+ (* YELLOW-COEFF (auto-speed A2)) (auto-x A2))
                         (auto-y A2) (auto-speed A2) (auto-img-index A2)))

(check-expect (calc-movement  A6-hit-mid (* TICKS-SECOND GREEN-LENGTH)) 
              (make-auto (+ (* YELLOW-COEFF (auto-speed A6-hit-mid ))
                            (auto-x A6-hit-mid)) (auto-y A6-hit-mid)
                                                 (auto-speed A6-hit-mid)
                                                 (auto-img-index A6-hit-mid)))

(check-expect (calc-movement A7-pass-mid (* TICKS-SECOND GREEN-LENGTH)) 
              (make-auto (+ (* YELLOW-COEFF (auto-speed A7-pass-mid))
                            (auto-x A7-pass-mid)) (auto-y A7-pass-mid)
                                                  (auto-speed A7-pass-mid)
                                                  (auto-img-index A7-pass-mid)))

(check-expect (calc-movement A8-past-mid (* TICKS-SECOND GREEN-LENGTH)) 
              (make-auto (+ (auto-speed A8-past-mid) (auto-x A8-past-mid))
                         (auto-y A8-past-mid) (auto-speed A8-past-mid)
                         (auto-img-index A8-past-mid)))

(check-expect (calc-movement A3 (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH))) 
              (make-auto (+ (* RED-COEFF (auto-speed A3)) (auto-x A3))
                         (auto-y A3) (auto-speed A3) (auto-img-index A3)))

(check-expect (calc-movement A6-hit-mid
                             (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH))) 
              (make-auto (+ (- X-POS (auto-x A6-hit-mid)) (auto-x A6-hit-mid))
                         (auto-y A6-hit-mid) (auto-speed A6-hit-mid)
                         (auto-img-index A6-hit-mid))) 
(check-expect (calc-movement A7-pass-mid
                             (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH))) 
              (make-auto (+ (- X-POS (auto-x A7-pass-mid))
                            (auto-x A7-pass-mid)) (auto-y A7-pass-mid)
                                                  (auto-speed A7-pass-mid)
                                                  (auto-img-index A7-pass-mid)))

(check-expect (calc-movement A8-past-mid
                             (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH))) 
              (make-auto (+ (auto-speed A8-past-mid) (auto-x A8-past-mid))
                         (auto-y A8-past-mid) (auto-speed A8-past-mid)
                         (auto-img-index A8-past-mid))) 



(define (move-auto auto speed)
  (make-auto (+ speed (auto-x auto))
             (auto-y auto) 
             (auto-speed auto)
             (auto-img-index auto)))

(check-expect (move-auto A1 (auto-speed A1)) 
              (make-auto (+ (auto-speed A1) (auto-x A1)) (auto-y A1)
                         (auto-speed A1) (auto-img-index A1)))
(check-expect (move-auto A1 0) 
              (make-auto (auto-x A1) (auto-y A1)
                         (auto-speed A1) (auto-img-index A1)))
(check-expect (move-auto A2 (* YELLOW-COEFF (auto-speed A2))) 
              (make-auto (+ (* YELLOW-COEFF (auto-speed A2)) (auto-x A2))
                         (auto-y A2) (auto-speed A2) (auto-img-index A2)))




(define (move-all-autos loa ticks)
  (cond [(empty? loa) empty]
        [(is-off-screen? (first loa))
         (move-all-autos (rest loa) ticks)]
        [else 
         (cons (calc-movement (first loa) ticks) 
               (move-all-autos (rest loa) ticks))]))

(check-expect (move-all-autos empty 0) empty)
(check-expect (move-all-autos (list A1 A2 A3) 0) 
              (list (make-auto (+ (auto-speed A1) (auto-x A1)) (auto-y A1)
                               (auto-speed A1) (auto-img-index A1))
                    (make-auto (+ (auto-speed A2) (auto-x A2)) (auto-y A2)
                               (auto-speed A2) (auto-img-index A2))
                    (make-auto (+ (auto-speed A3) (auto-x A3)) (auto-y A3)
                               (auto-speed A3) (auto-img-index A3))))
(check-expect (move-all-autos (list A1 A4-poff A2) 0)
              (list (make-auto (+ (auto-speed A1) (auto-x A1)) (auto-y A1)
                               (auto-speed A1) (auto-img-index A1))
                    (make-auto (+ (auto-speed A4-poff) (auto-x A4-poff))
                               (auto-y A4-poff) (auto-speed A4-poff)
                               (auto-img-index A4-poff))
                    (make-auto (+ (auto-speed A2) (auto-x A2)) (auto-y A2)
                               (auto-speed A2) (auto-img-index A2))))
(check-expect (move-all-autos (list A1 A3 A5-off) 0)
              (list  (make-auto (+ (auto-speed A1) (auto-x A1)) (auto-y A1)
                                (auto-speed A1) (auto-img-index A1))
                     (make-auto (+ (auto-speed A3) (auto-x A3)) (auto-y A3)
                                (auto-speed A3) (auto-img-index A3))))






(define (get-light-state ticks)
  (cond
    [(is-green? ticks)
     (overlay/align "center" "bottom" 
                    (circle LIGHT-RADIUS "solid" "green")
                    BULBS-OFF)]
    [(is-yellow? ticks)
     (overlay/align "center" "middle" 
                    (circle LIGHT-RADIUS "solid" "yellow")
                    BULBS-OFF)]
    [else 
     (overlay/align "center" "top" 
                    (circle LIGHT-RADIUS "solid" "red")
                    BULBS-OFF)]))

(check-expect (get-light-state (ws-ticks START))
              (overlay/align "center" "bottom"
                             (circle LIGHT-RADIUS "solid" "green")
                             BULBS-OFF))
(check-expect (get-light-state (ws-ticks WS1))
              (overlay/align "center" "bottom"
                             (circle LIGHT-RADIUS "solid" "green")
                             BULBS-OFF))
(check-expect (get-light-state (ws-ticks WS2))
              (overlay/align "center" "bottom"
                             (circle LIGHT-RADIUS "solid" "green")
                             BULBS-OFF))
(check-expect (get-light-state (ws-ticks WS3))
              (overlay/align "center" "middle"
                             (circle LIGHT-RADIUS "solid" "yellow")
                             BULBS-OFF))
(check-expect (get-light-state (ws-ticks WS4))
              (overlay/align "center" "middle"
                             (circle LIGHT-RADIUS "solid" "yellow")
                             BULBS-OFF))
(check-expect (get-light-state (ws-ticks WS5))
              (overlay/align "center" "top"
                             (circle LIGHT-RADIUS "solid" "red")
                             BULBS-OFF))
(check-expect (get-light-state (ws-ticks WS6))
              (overlay/align "center" "top"
                             (circle LIGHT-RADIUS "solid" "red")
                             BULBS-OFF))
(check-expect (get-light-state (ws-ticks WS7))
              (overlay/align "center" "bottom"
                             (circle LIGHT-RADIUS "solid" "green")
                             BULBS-OFF))




(define (get-counter ticks) (text (number->string (ticks->seconds ticks))
                                  TEXT-SIZE "black"))

(check-expect (get-counter (ws-ticks START))
              (text "0" TEXT-SIZE "black"))
(check-expect (get-counter (ws-ticks WS1))
              (text (number->string (ticks->seconds (ws-ticks WS1)))
                    TEXT-SIZE "black"))
(check-expect (get-counter (ws-ticks WS2LONG)) 
              (text (number->string (ticks->seconds
                                     (ws-ticks WS2LONG)))
                    TEXT-SIZE "black"))



(define (get-auto-images loa)
  (cond
    [(empty? loa) empty]
    [else
     (cons (pick-image (auto-img-index (first loa)))
           (get-auto-images (rest loa)))]))

(check-expect (get-auto-images empty) empty)
(check-expect (get-auto-images (list A1)) (list (pick-image 0)))
(check-expect (get-auto-images (list A1 A2 A3)) (list (pick-image 0)
                                                      (pick-image 1)
                                                      (pick-image 3)))



(define (get-auto-positions loa)
  (cond
    [(empty? loa) empty]
    [else
     (cons (make-posn (auto-x (first loa)) (auto-y (first loa)))
           (get-auto-positions (rest loa)))]))

(check-expect (get-auto-positions empty) empty)
(check-expect (get-auto-positions (list A1))
              (list (make-posn (auto-x A1)(auto-y A1))))
(check-expect (get-auto-positions (list A1 A2 A3))
              (list (make-posn (auto-x A1)(auto-y A1))
                    (make-posn (auto-x A2)(auto-y A2))
                    (make-posn (auto-x A3)(auto-y A3))))



(define (get-billboard billboards)
  (if (empty? billboards)
      (text "" 20 "black")
      (text (first billboards) 20 "black")))

(check-expect (get-billboard empty) (text "" 20 "black"))
(check-expect (get-billboard (list "test" "do not display"))
              (text "test" 20 "black"))
(check-expect (get-billboard LOB3)
              (text (first LOB3) 20 "black"))






(define (update-billboard ws text)
  (if (empty? (ws-billboards ws))
      (make-ws 
       (ws-ticks ws)
       (ws-autos ws)
       (list text))
      (make-ws 
       (ws-ticks ws)
       (ws-autos ws)
       (cons text (rest (ws-billboards ws))))))
  
(check-expect (update-billboard START "testing")
              (make-ws 0 empty (list "testing")))
(check-expect (update-billboard START "") (make-ws 0 empty (list "")))
(check-expect (update-billboard START "check") (make-ws 0 empty (list "check")))
(check-expect (update-billboard (make-ws 0 empty empty) "")
              (make-ws 0 empty (list "")))
(check-expect (update-billboard (make-ws 0 empty (list "delete me")) "")
              (make-ws 0 empty (list "")))



(define (add-to-billboard ws text)
  (if (empty? (ws-billboards ws))
      (update-billboard ws text)
      (update-billboard ws (string-append (first (ws-billboards ws)) text))))

(check-expect (add-to-billboard START "testing")
              (make-ws 0 empty (list "testing")))
(check-expect (add-to-billboard START "") (make-ws 0 empty (list "")))
(check-expect (add-to-billboard (make-ws 0 empty (list "please")) " check")
              (make-ws 0 empty (list "please check")))
(check-expect (add-to-billboard (make-ws 0 empty empty) "")
              (make-ws 0 empty (list "")))





(define (advance ws) 
  (make-ws 
   (add1 (ws-ticks ws)) 
   (append (make-random-auto CHANCE-OF-NEW-AUTO) 
           (move-all-autos (ws-autos ws) (ws-ticks ws)))
   (ws-billboards ws)))

(check-random (advance START)
              (make-ws 1 (make-random-auto CHANCE-OF-NEW-AUTO) (list "")))
(check-random (advance WS1)
              (make-ws 2 (make-random-auto CHANCE-OF-NEW-AUTO) (list "")))
(check-random (advance WS2)
              (make-ws (+ (ws-ticks WS2) 1)
                          (make-random-auto CHANCE-OF-NEW-AUTO) (list "")))



(define (render ws) 
  (place-images/align (append (list (get-light-state (ws-ticks ws))
                                    (get-counter (ws-ticks ws)))
                              (get-auto-images (ws-autos ws)))
                      (append (list (make-posn SIGNAL-X-POS Y-POS)
                                    (make-posn COUNTER-X Y-POS))
                              (get-auto-positions (ws-autos ws)))
                      "right" "middle"
                      (place-image/align
                       (get-billboard (ws-billboards ws))
                       X-POS-BOARD
                       Y-POS
                       "center"
                       "middle"
                       MTS)))

(check-expect (render START)
              (place-images/align (list (get-light-state (ws-ticks START))
                                        (get-counter (ws-ticks START)))
                                  (list (make-posn SIGNAL-X-POS Y-POS)
                                        (make-posn COUNTER-X Y-POS))
                                  "right" "middle" MTS))
(check-expect (render WS1)
              (place-images/align (list (get-light-state (ws-ticks WS1))
                                        (get-counter (ws-ticks WS1)))
                                  (list (make-posn SIGNAL-X-POS Y-POS)
                                        (make-posn COUNTER-X Y-POS))
                                  "right" "middle" MTS))
(check-expect (render WS2LONG)
              (place-images/align (list (get-light-state (ws-ticks WS2LONG))
                                        (get-counter (ws-ticks WS2LONG)))
                                  (list (make-posn SIGNAL-X-POS Y-POS)
                                        (make-posn COUNTER-X Y-POS))
                                  "right" "middle" MTS))

(check-expect (render (update-billboard WS2LONG "testing"))
              (place-images/align (list (get-light-state (ws-ticks WS2LONG))
                                        (get-counter (ws-ticks WS2LONG)))
                                  (list (make-posn SIGNAL-X-POS Y-POS)
                                        (make-posn COUNTER-X Y-POS))
                                  "right" "middle" (place-image/align
                       (get-billboard (ws-billboards
                                       (update-billboard WS2LONG "testing")))
                       X-POS-BOARD
                       Y-POS
                       "center"
                       "middle"
                       MTS)))

(check-expect (render (make-ws 30 (list A1 A3) LOB3))
              (place-images/align (append (list (get-light-state 30)
                                    (get-counter 30))
                              (get-auto-images (list A1 A3)))
                      (append (list (make-posn SIGNAL-X-POS Y-POS)
                                    (make-posn COUNTER-X Y-POS))
                              (get-auto-positions (list A1 A3)))
                                  "right" "middle"
                                  (place-image/align
                                   (get-billboard LOB3)
                                    X-POS-BOARD
                                    Y-POS
                                    "center"
                                    "middle"
                                    MTS)))





(define (handle-key ws key-event) 
  (cond
    [(string=? key-event "next")
     
     (make-ws (ws-ticks ws) (ws-autos ws) (cons "" (ws-billboards ws)))]
    [(and (string=? key-event "prior") (> (length (ws-billboards ws)) 1))
     
     (make-ws (ws-ticks ws) (ws-autos ws) (rest (ws-billboards ws)))]
    [(string=? key-event "\t") 
     
     (update-billboard ws "")]
    [(and (or (string=? key-event "\b") (string=? key-event "\u007F"))
          (> (string-length (first (ws-billboards ws))) 0))
     
     
     (update-billboard ws
                       (substring
                        (first (ws-billboards ws))
                        0 (- (string-length (first (ws-billboards ws))) 1)))]
    [(string=? key-event "\r")
     
     (add-to-billboard ws "\n")]
    [(= 1 (string-length key-event))
     
     (add-to-billboard ws key-event)]
    
    [else ws]))

(check-expect (handle-key START "\t") START)
(check-expect (handle-key START "prior") START)
(check-expect (handle-key START "next")
              (make-ws (ws-ticks START) (ws-autos START) (list "" "")))
(check-expect (handle-key START "1")
              (make-ws (ws-ticks START) (ws-autos START) (list "1")))
(check-expect (handle-key
               (make-ws (ws-ticks START) (ws-autos START) (list "1")) "\r") 
              (make-ws (ws-ticks START) (ws-autos START) (list "1\n")))
(check-expect (handle-key
               (make-ws (ws-ticks START) (ws-autos START) (list "1\n")) "\t") 
              (make-ws (ws-ticks START) (ws-autos START) (list "")))
(check-expect (handle-key
               (make-ws (ws-ticks START) (ws-autos START) LOB1) "next") 
              (make-ws (ws-ticks START) (ws-autos START)
                       (list "" "Let's talk about a VPN!")))
(check-expect (handle-key (make-ws (ws-ticks START) (ws-autos START) LOB3) "!") 
              (make-ws (ws-ticks START) (ws-autos START)
                       (list "I CAN DO ANYTHING!!"
                             "🃏" "1...2...3..." "BYE \nBYE!")))            
(check-expect (handle-key (make-ws (ws-ticks START) (ws-autos START) LOB3) "\b") 
              (make-ws (ws-ticks START) (ws-autos START)
                       (list "I CAN DO ANYTHING"
                             "🃏" "1...2...3..." "BYE \nBYE!")))
(check-expect (handle-key
               (make-ws (ws-ticks START) (ws-autos START) LOB3) "\u007F") 
              (make-ws (ws-ticks START) (ws-autos START)
                       (list "I CAN DO ANYTHING"
                             "🃏" "1...2...3..." "BYE \nBYE!")))
(check-expect (handle-key
               (make-ws (ws-ticks START) (ws-autos START) LOB2) "prior") 
              (make-ws (ws-ticks START) (ws-autos START)
                       (list "Please make it go away. >:(")))  





(define (main ws)
  (big-bang ws
    (on-tick advance)
    (to-draw render)
    (on-key handle-key)))

