

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname assignment2_trafficAutos_audreyG_cierraO) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



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

(define FONTSIZE 20)
(define FONTCOLOR "black")
(define X-POS-TIMER (+ X-POS (* 2 LIGHT-RADIUS) 15)) 
(define Y-POS-TIMER Y-POS) 








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















(define-struct signal (ticks seconds autos los))

(define START (make-signal 0 0 empty empty))

 





(define (main signal)
  (big-bang signal
    (on-tick update-signal) 
    (to-draw render-signal)
    (on-key update-text)))














(define (update-signal signal)
  (make-signal (add1 (signal-ticks signal)) (ticks->seconds (signal-ticks signal)) (generate-autos signal) (signal-los signal)))




































(define (render-signal signal)
  (place-image (choose-light signal) X-POS Y-POS
                  (place-image/align (text (implode (signal-los signal)) 20 "midnight blue") 40 100 "left" "bottom" 
                     (place-image (text (number->string (signal-seconds signal)) FONTSIZE FONTCOLOR) X-POS-TIMER Y-POS-TIMER
                            (place-images (createImageList (signal-autos signal)) (createPosnList (signal-autos signal)) MTS)))))







(check-expect (choose-light (make-signal 0 0 empty empty)) (above
                                              (circle LIGHT-RADIUS "outline" "red")
                                              (circle LIGHT-RADIUS "outline" "yellow")
                                              (circle LIGHT-RADIUS "solid" "green")))
(check-expect (choose-light (make-signal 168 6 empty empty)) (above
                                              (circle LIGHT-RADIUS "outline" "red")
                                              (circle LIGHT-RADIUS "solid" "yellow")
                                              (circle LIGHT-RADIUS "outline" "green")))
(check-expect (choose-light (make-signal 280 10 empty empty)) (above
                                               (circle LIGHT-RADIUS "solid" "red")
                                               (circle LIGHT-RADIUS "outline" "yellow")
                                               (circle LIGHT-RADIUS "outline" "green")))

(define (choose-light signal)
  (local [(define iteration-time (modulo (signal-seconds signal) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)))]
    (cond [(<= iteration-time (sub1 GREEN-LENGTH))
             (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "solid" "green"))]
          [(<= iteration-time (sub1 (+ GREEN-LENGTH YELLOW-LENGTH)))
             (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "solid" "yellow")
               (circle LIGHT-RADIUS "outline" "green"))]
          [else
           (above
            (circle LIGHT-RADIUS "solid" "red")
            (circle LIGHT-RADIUS "outline" "yellow")
            (circle LIGHT-RADIUS "outline" "green"))]
          )))











(define-struct auto (image xPos yPos velocity))
 




 








(define (generate-autos signal)
  (if (< (random 100) 5) 
      (cons (generate-auto 0) (update-autos (signal-autos signal) (signal-seconds signal)))
      (update-autos (signal-autos signal) (signal-seconds signal))))




  

  


(define (generate-auto x)
  (make-auto (pick-image (random 3)) 0 (+ (random (/ HEIGHT 2)) (* 2 Y-POS)) (+ 5 (random 15))))


  







(check-expect (update-autos (list (make-auto AUTO-IMAGE1 0 300 10) (make-auto AUTO-IMAGE2 100 150 17)) 2)
                      (list (make-auto AUTO-IMAGE1 (+ 0 10) 300 10) (make-auto AUTO-IMAGE2 (+ 100 17) 150 17)))

(check-expect (update-autos (list (make-auto AUTO-IMAGE1 X-POS 300 10) (make-auto AUTO-IMAGE2 500 150 17)) 9)
                      (list (make-auto AUTO-IMAGE1 X-POS 300 10) (make-auto AUTO-IMAGE2 (+ 500 17) 150 17)))
                                       
(define (update-autos loa currentSeconds)
  (cond [(empty? loa) empty]
        [(> (auto-xPos (first loa)) WIDTH) (update-autos (rest loa) currentSeconds)] 
        [else (cons (auto-behavior (first loa) currentSeconds) (update-autos (rest loa) currentSeconds))]))
  






(define (auto-behavior auto currentSeconds)
  (local [(define iteration-time (modulo currentSeconds (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)))]
    (cond [(or (<= iteration-time (sub1 GREEN-LENGTH)) (> (auto-xPos auto) X-POS)) 
             (generate-auto-behavior auto "green")]
          
          
          [(<= iteration-time (sub1 (+ GREEN-LENGTH YELLOW-LENGTH)))
             (generate-auto-behavior auto "yellow")]
          
          
          [(and (<= iteration-time (sub1 (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))) (= (auto-xPos auto) X-POS)) auto]
          
          [else 
             (generate-auto-behavior auto "red")])))







(check-expect (generate-auto-behavior (make-auto AUTO-IMAGE1 0 300 10) "yellow")
              (make-auto AUTO-IMAGE1 (+ (* 0.75 10) 0) 300 10))
(check-expect (generate-auto-behavior (make-auto AUTO-IMAGE1 0 300 10) "green")
              (make-auto AUTO-IMAGE1 (+ 10 0) 300 10))
(check-expect (generate-auto-behavior (make-auto AUTO-IMAGE1 0 300 10) "red")
              (make-auto AUTO-IMAGE1 (+ (* 0.5 10) 0) 300 10))

(define (generate-auto-behavior auto light)
  (cond [(string=? light "green")
         (make-auto (auto-image auto)                                                   
                    (+ (auto-velocity auto) (auto-xPos auto))
                    (auto-yPos auto)
                    (auto-velocity auto))]
        [(string=? light "yellow")
         (make-auto (auto-image auto)
                        (+ (* (auto-velocity auto) 0.75) (auto-xPos auto))
                        (auto-yPos auto)
                        (auto-velocity auto))]
        [(string=? light "red")
         (make-auto (auto-image auto)
                        (if (>= (+ (* (auto-velocity auto) 0.5) (auto-xPos auto)) X-POS)
                            X-POS
                            (+ (* (auto-velocity auto) 0.5) (auto-xPos auto)))
                        (auto-yPos auto)
                        (auto-velocity auto))]))
  








(check-expect (createImageList (list (make-auto AUTO-IMAGE1 0 300 10) (make-auto AUTO-IMAGE2 100 150 17)))
              (list AUTO-IMAGE1 AUTO-IMAGE2))
(check-expect (createImageList empty) empty)

(define (createImageList loa)
  (cond [(empty? loa) empty]
        [else
         (cons (auto-image (first loa)) (createImageList (rest loa)))]))







(check-expect (createPosnList (list (make-auto AUTO-IMAGE1 0 300 10) (make-auto AUTO-IMAGE2 100 150 17)))
              (list (make-posn 0 300) (make-posn 100 150)))
(check-expect (createPosnList empty) empty)

(define (createPosnList loa)
  (cond [(empty? loa) empty]
        [else
         (cons (make-posn (auto-xPos (first loa)) (auto-yPos (first loa))) (createPosnList (rest loa)))]))









(check-expect (update-text START "a") (make-signal (signal-ticks START) (signal-seconds START) (signal-autos START) (list "a")))
(check-expect (update-text (make-signal 28 1 empty (list "h" "i" " ")) "w") (make-signal 28 1 empty (list "h" "i" " " "w")))
(check-expect (update-text (make-signal 28 1 empty (list "1")) "\r") (make-signal 28 1 empty (list "1" "\n")))
(check-expect (update-text (make-signal 28 1 empty (list "h" "i")) "\b") (make-signal 28 1 empty (list "h")))
(check-expect (update-text (make-signal 28 1 empty (list "b" "y" "e")) "\t") (make-signal 28 1 empty empty))

(define (update-text signal keyEvent)
  (cond [(string=? keyEvent "\b")
          (make-signal (signal-ticks signal) (signal-seconds signal) (signal-autos signal) (remove-last (signal-los signal)))]
        [(string=? keyEvent "\t")
          (make-signal (signal-ticks signal) (signal-seconds signal) (signal-autos signal) empty)]
        [(string=? keyEvent "\r")
          (make-signal (signal-ticks signal) (signal-seconds signal) (signal-autos signal) (append (signal-los signal) (list"\n")))]
        [(= (string-length keyEvent) 1)
         (make-signal (signal-ticks signal) (signal-seconds signal) (signal-autos signal) (append (signal-los signal) (list keyEvent)))]
        [else
         signal]))







(check-expect (remove-last (list "a" "b")) (list "a"))
(check-expect (remove-last (list "a" "b" "c")) (list "a" "b"))
(check-expect (remove-last (list "a")) empty)
(check-expect (remove-last empty) empty)

(define (remove-last los)
  (cond [(empty? los) empty]
        [else
         (reverse (rest (reverse los)))]))