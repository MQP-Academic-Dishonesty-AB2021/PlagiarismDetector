

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname FINALTRAFFICPROJ) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define downCOW (rotate -10 COW))
(define upCOW (rotate 10 COW))

(define MTS (empty-scene WIDTH HEIGHT))


(define-struct cowState (x y speed laps isWiggle))









(define cowState1 (make-cowState 10 10 2 4 -1))
(define cowState2 (make-cowState 10 10 0 4 0))
(define cowState3 (make-cowState 100 20 5 10 1))

(define START (make-cowState 20 (/ HEIGHT 2) 1 0 -1))

(define (fn-for-cowState cow)
  (...
   (cowState-x        cow)
   (cowState-y        cow)
   (cowState-speed    cow)
   (cowState-laps     cow)
   (cowState-isWiggle cow)))








(define (main ws)
  (big-bang ws
    (on-tick  updateCow)  
    (to-draw  drawCow)    
    (on-mouse onClick)    
    (on-key   onPress)))  







(check-expect (updateCow START) (make-cowState 21 (/ HEIGHT 2) 1 0 1))
(check-expect (updateCow (make-cowState 400 300 5 1 1)) (make-cowState (+ 400 5) 300 5 1 (* -1 1)))
(check-expect (updateCow (make-cowState 790 100 10 0 1)) (make-cowState (+ 790 10) 100 10 0 (* -1 1)))
(check-expect (updateCow (make-cowState 795 (/ HEIGHT 2) 10 0 1)) (make-cowState 0 (/ HEIGHT 2) 10 1 (* -1 1)))
(check-expect (updateCow (make-cowState (/ WIDTH 2) (/ HEIGHT 2) 0 1 1)) (make-cowState (/ WIDTH 2) (/ HEIGHT 2) 0 1 0))



(define (updateCow cow)
  (cond [(> (+ (cowState-x cow) (cowState-speed cow)) WIDTH) (make-cowState 0 (cowState-y cow) (cowState-speed cow) (add1 (cowState-laps cow)) (* -1 (cowState-isWiggle cow)))]
        [(= (cowState-speed cow) 0) (make-cowState (cowState-x cow) (cowState-y cow) (cowState-speed cow) (cowState-laps cow) 0)]
        [else 
         (make-cowState (+ (cowState-x cow) (cowState-speed cow)) (cowState-y cow) (cowState-speed cow) (cowState-laps cow) (* -1 (cowState-isWiggle cow)))]))







(check-expect (drawCow START) (place-images (cons (text (string-append "Speed: " (number->string (cowState-speed START))) 25 "red")
                      (cons  (text (string-append "Laps: " (number->string (cowState-laps START))) 25 "red")
                             (cons upCOW empty)))

                (cons (make-posn 75 50) (cons (make-posn 75 80) (cons (make-posn (cowState-x START) (cowState-y START)) empty)))
                MTS))

(check-expect (drawCow (make-cowState 400 200 4 2 1)) (place-images (cons (text (string-append "Speed: " (number->string 4)) 25 "red")
                      (cons  (text (string-append "Laps: " (number->string 2)) 25 "red")
                             (cons downCOW empty)))

                (cons (make-posn 75 50) (cons (make-posn 75 80) (cons (make-posn 400 200) empty)))
                MTS))

(check-expect (drawCow (make-cowState 400 200 0 5 0)) (place-images (cons (text (string-append "Speed: " (number->string 0)) 25 "red")
                      (cons  (text (string-append "Laps: " (number->string 5)) 25 "red")
                             (cons COW empty)))

                (cons (make-posn 75 50) (cons (make-posn 75 80) (cons (make-posn 400 200) empty)))
                MTS))

  

 


(define (drawCow cow)
  (place-images (cons (text (string-append "Speed: " (number->string (cowState-speed cow))) 25 "red")
                      (cons  (text (string-append "Laps: " (number->string (cowState-laps cow))) 25 "red")
                             (cons (chooseCowImg cow) empty)))

                (cons (make-posn 75 50) (cons (make-posn 75 80) (cons (make-posn (cowState-x cow) (cowState-y cow)) empty)))
                MTS))


        





(check-expect (chooseCowImg START) upCOW)
(check-expect (chooseCowImg (make-cowState 200 200 2 0 1)) downCOW)
(check-expect (chooseCowImg (make-cowState 200 200 0 0 0)) COW)
(check-expect (chooseCowImg (make-cowState 200 200 2 0 -1)) upCOW)

  

(define (chooseCowImg cow)
  (cond [(= (cowState-isWiggle cow) 1) downCOW]
        [(= (cowState-isWiggle cow) -1) upCOW]
        [(= (cowState-isWiggle cow) 0) COW]))






(check-expect (onClick START 200 200 "button-up") (make-cowState 200 200 (cowState-speed START) (cowState-laps START) (cowState-isWiggle START)))
(check-expect (onClick (make-cowState (/ WIDTH 2) (/ HEIGHT 2) 2 0 1) 25 25 "button-up") (make-cowState 25 25 2 0 1))
(check-expect (onClick (make-cowState (/ WIDTH 2) (/ HEIGHT 2) 2 0 1) 25 25 "move") (make-cowState (/ WIDTH 2) (/ HEIGHT 2) 2 0 1))

  

(define (onClick cow x y mouseIn)
  (if (mouse=? mouseIn "button-up")
      (make-cowState x y (cowState-speed cow) (cowState-laps cow) (cowState-isWiggle cow))
      cow))
  




(check-expect (onPress START "s") (make-cowState (cowState-x START) (cowState-y START) (+ (cowState-speed START) 1) (cowState-laps START) (cowState-isWiggle START)))
(check-expect (onPress START "a") (make-cowState (cowState-x START) (cowState-y START) (- (cowState-speed START) 1) (cowState-laps START) (cowState-isWiggle START)))
(check-expect (onPress (make-cowState 200 400 0 3 -1) "a") (make-cowState 200 400 0 3 -1))
(check-expect (onPress (make-cowState 300 450 0 5 0) "s") (make-cowState 300 450 1 5 -1))
(check-expect (onPress START "w") START)

  

(define (onPress cow keyIn)
  
  (cond [(key=? keyIn "s")  (make-cowState (cowState-x cow) (cowState-y cow) (+ (cowState-speed cow) 1) (cowState-laps cow) -1)]
        [(key=? keyIn "a") (make-cowState (cowState-x cow) (cowState-y cow) (max (- (cowState-speed cow) 1) 0) (cowState-laps cow) (cowState-isWiggle cow))]
        [else cow]))
























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

(define CYCLE-LENGTH (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))


(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))

(define RED-LIGHT
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))

(define YELLOW-LIGHT
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))

(define GREEN-LIGHT
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
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







(define-struct auto (speed x y imgVal))








(define auto1 (make-auto 2 100 700 1))
(define auto2 (make-auto 1 200 500 0))
(define auto3 (make-auto 5 0 450 2))
(define auto4 (make-auto 10 2 400 0))

(define (fn-for-auto auto0)
  (...
   (auto-speed auto0)
   (auto-x auto0)
   (auto-y auto0)
   (auto-imgVal auto0)))







(define loa1 empty)
(define loa2 (cons (make-auto 2 100 700 1) empty))
(define loa3 (cons (make-auto 1 200 500 0) (cons (make-auto 5 0 450 2) (cons (make-auto 10 2 400 0) empty))))

 





(define-struct signalState (ticksElapsed currentColor autoList))






(define ss1 (make-signalState 10 "Yellow" empty))
(define ss2 (make-signalState 203 "Red" (cons (make-auto 2 100 700 1) empty)))
(define ss3 (make-signalState 26 "Green" (cons (make-auto 1 200 500 0) (cons (make-auto 2 100 700 1) (cons (make-auto 10 2 400 0) empty)))))

(define START (make-signalState 0 "Green" empty))

 



(define (main ws)
  (big-bang ws
    (on-tick  updateSignal)  
    (to-draw  drawSignal)))  








(check-expect (filterAutos empty) empty)
(check-expect (filterAutos (cons (make-auto 3 3 403 0) empty)) (cons (make-auto 3 3 403 0) empty))
(check-expect (filterAutos (cons (make-auto 3 (+ 10 WIDTH) 403 0) empty)) empty)
(check-expect (filterAutos (list (make-auto 3 (+ 10 WIDTH) 403 0) (make-auto 5 (+ 1 WIDTH) 403 0) (make-auto 7 (+ 100 WIDTH) 403 0) (make-auto 9 (+ 12 WIDTH) 403 0)))
              empty)
(check-expect (filterAutos (list (make-auto 3 100 403 0) (make-auto 5 (+ 1 WIDTH) 403 0) (make-auto 7 250 403 0) (make-auto 9 600 403 0)))
              (list (make-auto 3 100 403 0) (make-auto 7 250 403 0) (make-auto 9 600 403 0)))

  

(define (filterAutos loa)
  (cond [(empty? loa) empty]
        [else
         (if (< (auto-x (first loa)) WIDTH)
             (cons (first loa) (filterAutos (rest loa)))
             (filterAutos (rest loa)))]))










(check-expect (determineSpeedScalar (make-auto 3 3 403 0) "Green") 1)
(check-expect (determineSpeedScalar (make-auto 3 600 403 0) "Red") 1)
(check-expect (determineSpeedScalar (make-auto 3 3 403 0) "Yellow") .75)
(check-expect (determineSpeedScalar (make-auto 3 3 403 0) "Red") .5)



(define (determineSpeedScalar auto lightColor)
  (cond [(or (string=? "Green" lightColor) (> (auto-x auto) LIGHT-BOUND)) 1]
        [(string=? "Yellow" lightColor) .75]
        [(string=? "Red" lightColor) .5]))





(check-expect (updateAutoPos empty "Green") empty)
(check-expect (updateAutoPos (cons (make-auto 3 3 403 0) empty) "Green") (cons (make-auto 3 6 403 0) empty))
(check-expect (updateAutoPos (list (make-auto 3 100 403 0) (make-auto 5 (+ 1 WIDTH) 403 0) (make-auto 7 250 403 0) (make-auto 9 600 403 0)) "Green")
              (list (make-auto 3 (+ 3 100) 403 0) (make-auto 5 (+ 5 1 WIDTH) 403 0) (make-auto 7 (+ 7 250) 403 0) (make-auto 9 (+ 9 600) 403 0)))

(check-expect (updateAutoPos (cons (make-auto 5 (- LIGHT-BOUND 1) 403 0) empty) "Red") (cons (make-auto 5 LIGHT-BOUND 403 0) empty))




(define LIGHT-BOUND X-POS)






(define (updateAutoPos loa lightColor)
  (cond [(empty? loa) empty]
        [else
         (cond [(> (auto-x (first loa)) LIGHT-BOUND)
                (cons (make-auto (auto-speed (first loa))
                                 (+ (auto-speed (first loa)) (auto-x (first loa)))
                                 (auto-y (first loa))
                                 (auto-imgVal (first loa)))
                      (updateAutoPos (rest loa) lightColor))]
               [(and (string=? lightColor "Red")
                     (> (+ (auto-speed (first loa)) (auto-x (first loa))) LIGHT-BOUND))
                (cons (make-auto (auto-speed (first loa))
                                 LIGHT-BOUND
                                 (auto-y (first loa))
                                 (auto-imgVal (first loa)))
                      (updateAutoPos (rest loa) lightColor))]
               [else
                (cons (make-auto (auto-speed (first loa))
                                 (+ (* (auto-speed (first loa)) (determineSpeedScalar (first loa) lightColor)) (auto-x (first loa)))
                                 (auto-y (first loa))
                                 (auto-imgVal (first loa)))
                      (updateAutoPos (rest loa) lightColor))])]))



























(define approxLightBottomY (+ (* 3 LIGHT-RADIUS) Y-POS 20))



 

(define (addNewAuto loa)
  (if (< (random 100) 5)
      (append loa (list (make-auto (+ 1 (random 20)) 0 (+ approxLightBottomY (random (- HEIGHT approxLightBottomY))) (random 3))))
      loa))









(check-expect (updateSignal START) (make-signalState 1 "Green" empty))
(check-expect (updateSignal (make-signalState 4 "Green" empty)) (make-signalState 5 "Green" empty))
(check-expect (updateSignal (make-signalState (- (* GREEN-LENGTH TICKS-SECOND) 1) "Green" empty))
              (make-signalState (* GREEN-LENGTH TICKS-SECOND) "Yellow" empty))
(check-expect (updateSignal (make-signalState 150 "Yellow" empty)) (make-signalState 151 "Yellow" empty))
(check-expect (updateSignal (make-signalState (- (* (+ GREEN-LENGTH YELLOW-LENGTH) TICKS-SECOND) 1) "Yellow" empty))
              (make-signalState (* (+ GREEN-LENGTH YELLOW-LENGTH) TICKS-SECOND) "Red" empty))
(check-expect (updateSignal (make-signalState 250 "Red" empty)) (make-signalState 251 "Red" empty))
(check-expect (updateSignal (make-signalState (- (* CYCLE-LENGTH TICKS-SECOND) 1) "Red" empty))
              (make-signalState (* CYCLE-LENGTH TICKS-SECOND) "Green" empty))
(check-expect (updateSignal (make-signalState (- (* 2 (* CYCLE-LENGTH TICKS-SECOND)) 1) "Red" empty))
              (make-signalState (* 2 (* CYCLE-LENGTH TICKS-SECOND)) "Green" empty))

(check-expect (updateSignal (make-signalState 203 "Yellow" (list (make-auto 3 621 403 0) (make-auto 5 555 403 0) (make-auto 7 757 403 0) (make-auto 9 609 403 0))))
              (make-signalState 204 "Red" (list (make-auto 3 624 403 0) (make-auto 5 560 403 0) (make-auto 7 764 403 0) (make-auto 9 618 403 0))))
(check-expect (updateSignal (make-signalState 4 "Green" (list (make-auto 3 100 403 0) (make-auto 5 (+ 1 WIDTH) 403 0) (make-auto 7 250 403 0) (make-auto 9 600 403 0))))
              (make-signalState 5 "Green" (list (make-auto 3 103 403 0) (make-auto 7 257 403 0) (make-auto 9 609 403 0))))

  



(define (updateSignal ss)
  (cond [(< (modulo (ticks->seconds (+ 1 (signalState-ticksElapsed ss))) CYCLE-LENGTH) GREEN-LENGTH)
         (make-signalState (+ 1 (signalState-ticksElapsed ss)) "Green" (filterAutos (updateAutoPos (addNewAuto (signalState-autoList ss)) "Green")))]
        [(< (modulo (ticks->seconds (+ 1 (signalState-ticksElapsed ss))) CYCLE-LENGTH) (+ GREEN-LENGTH YELLOW-LENGTH))
         (make-signalState (+ 1 (signalState-ticksElapsed ss)) "Yellow" (filterAutos (updateAutoPos (addNewAuto (signalState-autoList ss)) "Yellow")))]
        [else (make-signalState (+ 1 (signalState-ticksElapsed ss)) "Red" (filterAutos (updateAutoPos (addNewAuto (signalState-autoList ss)) "Red")))]))













(check-expect (drawSignal START) (place-images (cons GREEN-LIGHT
                                                     (cons (text (string-append "Time elapsed: " (number->string (ticks->seconds (signalState-ticksElapsed START)))) 25 "red") empty))
                                               (cons (make-posn X-POS Y-POS) (cons (make-posn (/ WIDTH 4) (/ HEIGHT 3)) empty))
                                               MTS))

(check-expect (drawSignal (make-signalState 150 "Yellow" empty)) (place-images (cons YELLOW-LIGHT
                                                                               (cons (text (string-append "Time elapsed: "
                                                                                           (number->string (ticks->seconds (signalState-ticksElapsed (make-signalState 150 "Yellow" empty)))))
                                                                                           25 "red") empty))
                                                                               (cons (make-posn X-POS Y-POS) (cons (make-posn (/ WIDTH 4) (/ HEIGHT 3)) empty))
                                                                               MTS))

(check-expect (drawSignal (make-signalState (- (* 2 (* CYCLE-LENGTH TICKS-SECOND)) 1) "Red" empty)) (place-images (cons RED-LIGHT
                                                                                                                  (cons (text (string-append "Time elapsed: "
                                                                                                                              (number->string (ticks->seconds (- (* 2 (* CYCLE-LENGTH TICKS-SECOND)) 1))))
                                                                                                                              25 "red") empty))
                                                                                                                  (cons (make-posn X-POS Y-POS) (cons (make-posn (/ WIDTH 4) (/ HEIGHT 3)) empty))
                                                                                                                  MTS))              

(check-expect (drawSignal (make-signalState 150 "Yellow" (cons (make-auto 3 3 403 0) empty))) (place-images (cons YELLOW-LIGHT
                                                                                                            (cons (text (string-append "Time elapsed: "
                                                                                                                        (number->string (ticks->seconds (signalState-ticksElapsed (make-signalState 150 "Yellow" empty)))))
                                                                                                                        25 "red") (cons (pick-image 0) empty)))
                                                                                                            (cons (make-posn X-POS Y-POS) (cons (make-posn (/ WIDTH 4) (/ HEIGHT 3)) (cons (make-posn 3 403) empty)))

                                                                                                            MTS))


(check-expect (drawSignal (make-signalState 150 "Yellow" (list (make-auto 3 (+ 10 WIDTH) 403 0) (make-auto 5 200 403 0)))) (place-images (cons YELLOW-LIGHT
                                                                                                                                         (cons (text (string-append "Time elapsed: "
                                                                                                                                                                    (number->string (ticks->seconds (signalState-ticksElapsed (make-signalState 150 "Yellow" empty)))))
                                                                                                                                                     25 "red") (cons (pick-image 0) (cons (pick-image 0) empty))))
                                                                                                                                         (cons (make-posn X-POS Y-POS) (cons (make-posn (/ WIDTH 4) (/ HEIGHT 3)) (cons (make-posn (+ 10 WIDTH) 403) (cons (make-posn 200 403) empty))))

                                                                                                                                         MTS))




 

(check-expect (getAutoImages (cons (make-auto 1 5 403 0) empty)) (cons (pick-image 0) empty))
(check-expect (getAutoImages (cons (make-auto 1 5 403 0) (cons (make-auto 1 5 403 1) empty))) (cons (pick-image 0) (cons (pick-image 1) empty)))
(check-expect (getAutoImages empty) empty)

(define (getAutoImages loa)
  (cond [(empty? loa) empty]
        [else
         (cons 
          (pick-image (auto-imgVal (first loa)))
          (getAutoImages (rest loa)))]))





 

(check-expect (getAutoPosns (cons (make-auto 1 5 403 0) empty)) (cons (make-posn 5 403) empty))
(check-expect (getAutoPosns (cons (make-auto 1 5 403 0) (cons (make-auto 1 200 523 1) empty))) (cons (make-posn 5 403) (cons (make-posn 200 523) empty)))
(check-expect (getAutoPosns empty) empty)

(define (getAutoPosns loa)
  (cond [(empty? loa) empty]
        [else
         (cons 
          (make-posn (auto-x (first loa)) (auto-y (first loa)))
          (getAutoPosns (rest loa)))]))



(define (drawSignal ss)
  (place-images (cons (chooseSignalImg ss)
                      (cons (text (string-append "Time elapsed: "
                                                 (number->string (ticks->seconds (signalState-ticksElapsed ss))))
                                  25 "red")
                            (getAutoImages (signalState-autoList ss))))
                (cons (make-posn X-POS Y-POS)
                      (cons (make-posn (/ WIDTH 4) (/ HEIGHT 3))
                            (getAutoPosns (signalState-autoList ss))))
                MTS))





(check-expect (chooseSignalImg START) GREEN-LIGHT)
(check-expect (chooseSignalImg (make-signalState 150 "Yellow" empty)) YELLOW-LIGHT)
(check-expect (chooseSignalImg (make-signalState 250 "Red" empty)) RED-LIGHT)



(define (chooseSignalImg ss)
  (cond [(string=? (signalState-currentColor ss) "Green") GREEN-LIGHT] 
        [(string=? (signalState-currentColor ss) "Yellow") YELLOW-LIGHT]
        [(string=? (signalState-currentColor ss) "Red") RED-LIGHT]
        [else BULBS])) 












