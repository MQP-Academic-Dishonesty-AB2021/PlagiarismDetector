

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Jennings-Itty Part1 Assignment2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)











(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT))
(define STOPPOINT (/ WIDTH 2))

(define TIMER-X (+ (/ WIDTH 2) 100))
(define TIMER-Y (/ HEIGHT 4))

(define BILLBOARD-X (/ WIDTH 4))
(define BILLBOARD-Y (/ HEIGHT 4))

(define TEXT-SIZE 18)
(define TEXT-COLOUR "black")

(define TICKS-SECOND 28) 


(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 

(define LIGHT-INTERVAL (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))

(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)




(define GENCHANCE 20)

(define MINSPEED 3)
(define MAXSPEED 10)

(define REDSPEED .5)
(define YELLOWSPEED .75)





(define-struct trafficLight (ticks autos billboards))






(define START (make-trafficLight 0 empty empty))

 






 
 

(define-struct auto (type x y speed))









   
  








        





(define LOA-1 empty)
(define LOA-2 (cons (make-auto 0 100 200 5) empty))
(define LOA-3 (cons (make-auto 1 400 200 0)
                    (cons (make-auto 2 320 400 10) empty)))

  













 
 






(define LOP-1 empty)
(define LOP-2 (cons (make-posn 100 200) empty))
(define LOP-3 (cons (make-posn 400 200)
                    (cons (make-posn 320 400) empty)))

  






(define LOS-1 empty)
(define LOS-2 (cons "a" empty))
(define LOS-3 (cons "b" (cons "c" empty)))

  










(define (main tfL)
  (big-bang tfL                   
    (on-tick   update)            
    (to-draw   render-scene)      
    (on-key    handle-key)))      
  











(define (update tfL) (make-trafficLight
                      (+ (trafficLight-ticks tfL) 1)
                      (if (= (random GENCHANCE) 1)
                          (cons (make-auto (random 3)
                                           0
                                           (+ (random (/ HEIGHT 2))
                                              (/ HEIGHT 2))
                                           (+ MINSPEED (random MAXSPEED)))
                            
                            
                            
                            
                                (update-autos tfL))
                          
                          (update-autos tfL))
                      (trafficLight-billboards tfL)))




(check-expect (update-autos (make-trafficLight 0 empty empty)) empty)
(check-expect (update-autos (make-trafficLight (* 8 28) LOA-3 empty))
              (cons (make-auto 1 400 200 0)
                    (cons (make-auto 2 325 400 10) empty)))
(check-expect
 (update-autos (make-trafficLight 0 (list (make-auto 0 99999 0 0)) empty))
 empty)



(define (update-autos tfL)
  (cond [(empty? (trafficLight-autos tfL)) empty] 

        
        [(>= (auto-x (first (trafficLight-autos tfL))) WIDTH) 
         (update-autos (make-trafficLight (trafficLight-ticks tfL)
                                          (rest (trafficLight-autos tfL))
                                          (trafficLight-billboards tfL)))]
        
        [else (cons (light-behavior (choose-traffic-light tfL)
                                    (first (trafficLight-autos tfL))) 
                    (update-autos (make-trafficLight
                                   (trafficLight-ticks tfL)
                                   (rest (trafficLight-autos tfL))
                                   (trafficLight-billboards tfL))))])) 








(check-expect (light-behavior "green" (make-auto 0 40 500 9))
              (make-auto 0 (+ 40 9) 500 9))
(check-expect (light-behavior "yellow" (make-auto 0 (+ STOPPOINT 10) 500 9))
              (make-auto 0 (+ (+ STOPPOINT 10) 9) 500 9))
(check-expect (light-behavior "yellow" (make-auto 0 (- STOPPOINT 10) 500 10))
              (make-auto 0 (+ (- STOPPOINT 10) (* 10 YELLOWSPEED)) 500 10))
(check-expect (light-behavior "red" (make-auto 0 STOPPOINT 500 10))
              (make-auto 0 STOPPOINT 500 10))
(check-expect (light-behavior "red" (make-auto 0 (/ STOPPOINT 5) 500 10))
              (make-auto 0 (+ 5 (/ STOPPOINT 5)) 500 10))



(define (light-behavior ls auto)
  (if (>= STOPPOINT (auto-x auto))
      (cond [(string=? "red" ls)
             (if (will-pass? auto)
                 (move-auto "halfway" auto)
                 (move-auto "1/2" auto))]
            [(string=? "yellow" ls)
             (move-auto "3/4" auto)]
            [(string=? "green" ls) (move-auto "normal" auto)])
      (move-auto "normal" auto)))






(check-expect (will-pass? (make-auto 0 STOPPOINT 0 10))
              true)

(check-expect (will-pass? (make-auto 0 (- STOPPOINT (* 10 REDSPEED)) 0 10))
              true)

(check-expect (will-pass? (make-auto 0 (- STOPPOINT 40) 0 10))
              false)

(check-expect (will-pass? (make-auto 0 (+ STOPPOINT 40) 0 10))
              true)



(define (will-pass? auto)
  (<= STOPPOINT (+ (auto-x auto) (* (auto-speed auto) REDSPEED))))






(check-expect (move-auto "halfway" (make-auto 0 0 0 0))
              (make-auto 0 STOPPOINT 0 0))
(check-expect (move-auto "1/2" (make-auto 0 50 400 10))
              (make-auto 0 (+ 50 (/ 10 2)) 400 10))
(check-expect (move-auto "3/4" (make-auto 0 50 400 10))
              (make-auto 0 (+ 50 (* 10 YELLOWSPEED)) 400 10))
(check-expect (move-auto "normal" (make-auto 0 400 400 50))
              (make-auto 0 (+ 400 50) 400 50))




(define (move-auto mm auto)
  (cond [(string=? "halfway" mm)
         (make-auto (auto-type auto)
                    STOPPOINT
                    (auto-y auto)
                    (auto-speed auto))]
        
        [(string=? "1/2" mm)
         (make-auto (auto-type auto)
                    (+ (auto-x auto) (* (auto-speed auto) REDSPEED))
                    (auto-y auto)
                    (auto-speed auto))]
        
        [(string=? "3/4" mm)
         (make-auto (auto-type auto)
                    (+ (auto-x auto) (* (auto-speed auto) YELLOWSPEED))
                    (auto-y auto)
                    (auto-speed auto))]
        
        [(string=? "normal" mm)
         (make-auto (auto-type auto)
                    (+ (auto-x auto) (auto-speed auto))
                    (auto-y auto)
                    (auto-speed auto))]))






(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)



(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))








(define (render-scene tfL)
  (place-images (append (list (renderLight (choose-traffic-light tfL))
                              (render-timer tfL)
                              (render-advertisement tfL))
                        (chooseAutoList (trafficLight-autos tfL)))
                (append (list (make-posn X-POS Y-POS)
                              (make-posn TIMER-X TIMER-Y)
                              (make-posn BILLBOARD-X BILLBOARD-Y))
                        (find-auto-posns (trafficLight-autos tfL)))
                MTS))





(check-expect (choose-traffic-light (make-trafficLight 0 empty empty)) "green")
(check-expect (choose-traffic-light (make-trafficLight 168 empty empty)) "yellow")
(check-expect (choose-traffic-light (make-trafficLight 280 empty empty)) "red")
(check-expect (choose-traffic-light (make-trafficLight 308 empty empty)) "green")



(define (choose-traffic-light tfL)
  (cond [(< (modulo (ticks->seconds (trafficLight-ticks tfL)) LIGHT-INTERVAL)
            GREEN-LENGTH)
         "green"]
        
        [(< (modulo (ticks->seconds (trafficLight-ticks tfL)) LIGHT-INTERVAL)
            (+ GREEN-LENGTH YELLOW-LENGTH))
         "yellow"]
        
        [(< (modulo (ticks->seconds (trafficLight-ticks tfL)) LIGHT-INTERVAL)
            LIGHT-INTERVAL)
         "red"]))



(check-expect (renderLight "red") (above
                                   (circle LIGHT-RADIUS "solid" "red")
                                   (circle LIGHT-RADIUS "outline" "yellow")
                                   (circle LIGHT-RADIUS "outline" "green")))
(check-expect (renderLight "green") (above
                                     (circle LIGHT-RADIUS "outline" "red")
                                     (circle LIGHT-RADIUS "outline" "yellow")
                                     (circle LIGHT-RADIUS "solid" "green")))
(check-expect (renderLight "yellow") (above
                                      (circle LIGHT-RADIUS "outline" "red")
                                      (circle LIGHT-RADIUS "solid" "yellow")
                                      (circle LIGHT-RADIUS "outline" "green")))



(define (renderLight ls)
  (cond [(string=? "red" ls) (above
                              (circle LIGHT-RADIUS "solid" "red")
                              (circle LIGHT-RADIUS "outline" "yellow")
                              (circle LIGHT-RADIUS "outline" "green"))]
        [(string=? "yellow" ls) (above
                                 (circle LIGHT-RADIUS "outline" "red")
                                 (circle LIGHT-RADIUS "solid" "yellow")
                                 (circle LIGHT-RADIUS "outline" "green"))]
        [(string=? "green" ls) (above
                                (circle LIGHT-RADIUS "outline" "red")
                                (circle LIGHT-RADIUS "outline" "yellow")
                                (circle LIGHT-RADIUS "solid" "green"))]))

  


(check-expect (render-timer (make-trafficLight 0 empty empty))
              (text (number->string (ticks->seconds 0)) TEXT-SIZE TEXT-COLOUR))
(check-expect (render-timer
               (make-trafficLight 527934512 empty empty))
              (text (number->string
                     (ticks->seconds  527934512)) TEXT-SIZE TEXT-COLOUR))



(define (render-timer tfL)
  (text (number->string
         (ticks->seconds (trafficLight-ticks tfL))) TEXT-SIZE TEXT-COLOUR))






(check-expect (chooseAutoList LOA-1) empty)
(check-expect (chooseAutoList LOA-2)
              (cons AUTO-IMAGE1 empty))
(check-expect (chooseAutoList LOA-3)
              (cons AUTO-IMAGE2 (cons AUTO-IMAGE3 empty)))




(define (chooseAutoList loa)
  (cond [(empty? loa) empty]                               
        [else (cons (pick-image (auto-type (first loa)))   
                    (chooseAutoList (rest loa)))]))        





(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)

(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))




(check-expect (find-auto-posns LOA-1) empty)
(check-expect (find-auto-posns LOA-2) (cons (make-posn 100 200) empty))
(check-expect (find-auto-posns LOA-3)
              (cons (make-posn 400 200) (cons (make-posn 320 400) empty)))



(define (find-auto-posns loa)
  (cond [(empty? loa) empty]                                             
        [else (cons (make-posn (auto-x (first loa)) (auto-y (first loa)))
                    (find-auto-posns (rest loa)))]))                      

              











(check-expect (handle-key START "\t")
              (make-trafficLight
               (trafficLight-ticks START)
               (trafficLight-autos START)
               (cons "" empty)))
(check-expect (handle-key (make-trafficLight 0 LOA-2 (cons "ab" empty)) "\t")
              (make-trafficLight 0 LOA-2 (cons "" empty)))

(check-expect (handle-key (make-trafficLight 0 LOA-2 (cons "ab" empty)) "\r")
              (make-trafficLight 0 LOA-2 (cons "ab\n" empty)))

(check-expect (handle-key (make-trafficLight 0 LOA-2 (cons "ab" empty)) "\b")
              (make-trafficLight 0 LOA-2 (cons "a" empty)))
(check-expect (handle-key (make-trafficLight 0 LOA-2 (cons "" empty)) "\b")
              (make-trafficLight 0 LOA-2 (cons "" empty)))

(check-expect (handle-key (make-trafficLight 0 LOA-2 (cons "ab" empty)) "next")
              (make-trafficLight 0 LOA-2 (list "" "ab")))

(check-expect (handle-key (make-trafficLight 0 LOA-2 (list "ab" "")) "prior")
              (make-trafficLight 0 LOA-2 (list "")))

(check-expect (handle-key (make-trafficLight 0 LOA-2 (cons "ab" empty)) "c")
              (make-trafficLight 0 LOA-2 (list "abc")))

(define (handle-key tfL kE)
  (cond [(empty? (trafficLight-billboards tfL))
         (make-trafficLight
          (trafficLight-ticks tfL)
          (trafficLight-autos tfL)
          (cons "" empty))]
        
        [(key=? kE "\t") 
         (make-trafficLight (trafficLight-ticks tfL)
                            (trafficLight-autos tfL)
                            (cons ""
                                  (rest
                                   (trafficLight-billboards tfL))))]
        
        [(key=? kE "\r") 
         (make-trafficLight (trafficLight-ticks tfL)
                            (trafficLight-autos tfL)
                            (cons (string-append
                                   (first (trafficLight-billboards tfL))
                                   "\n")
                                  (rest (trafficLight-billboards tfL))))]
        
        [(key=? kE "\b") 
         (if (= (string-length (first (trafficLight-billboards tfL))) 0)
             
             (make-trafficLight (trafficLight-ticks tfL)
                                (trafficLight-autos tfL)
                                (cons "" (rest (trafficLight-billboards tfL))))
             
             
             (make-trafficLight (trafficLight-ticks tfL) (trafficLight-autos tfL)
                                (cons
                                 (substring (first (trafficLight-billboards tfL))
                                            0
                                            (sub1 (string-length
                                                   (first (trafficLight-billboards tfL)))))
                                    
                                 (rest (trafficLight-billboards tfL)))))]
        
        [(key=? kE "next") 
         (make-trafficLight (trafficLight-ticks tfL)
                            (trafficLight-autos tfL)
                            (cons "" (trafficLight-billboards tfL)))]
        
        [(key=? kE "prior") 
         (make-trafficLight (trafficLight-ticks tfL)
                            (trafficLight-autos tfL)
                            (rest (trafficLight-billboards tfL)))]
        
        [else (make-trafficLight (trafficLight-ticks tfL)
                                 (trafficLight-autos tfL)
                                 (cons (string-append (first (trafficLight-billboards tfL)) kE)
                                       (rest (trafficLight-billboards tfL))))]))








(check-expect (render-advertisement (make-trafficLight 0 empty empty))
              (text "" TEXT-SIZE TEXT-COLOUR))
(check-expect (render-advertisement (make-trafficLight 0 empty
                                                       (list "aaaa" "bbbb" "ccc")))
              (text "aaaa" TEXT-SIZE TEXT-COLOUR))
              
(define (render-advertisement tfL)
  (cond [(empty? (trafficLight-billboards tfL)) (text "" TEXT-SIZE TEXT-COLOUR)]
        
        [else (text (first (trafficLight-billboards tfL)) TEXT-SIZE TEXT-COLOUR)]))

