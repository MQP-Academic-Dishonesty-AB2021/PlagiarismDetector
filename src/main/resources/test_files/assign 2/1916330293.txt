

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Lab2P2011) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




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


(define CYCLE-LENGTH (+ GREEN-LENGTH
                        YELLOW-LENGTH
                        RED-LENGTH))






(check-expect (bulbs 0)
              (above
               (circle LIGHT-RADIUS
                       "outline"
                       "red")
               (circle LIGHT-RADIUS
                       "outline"
                       "yellow")
               (circle LIGHT-RADIUS                      
                       "solid"                       
                       "green")))
(check-expect (bulbs 1)
              (above
               (circle LIGHT-RADIUS
                       "outline"
                       "red")
               (circle LIGHT-RADIUS
                       "solid"
                       "yellow")
               (circle LIGHT-RADIUS                      
                       "outline"                       
                       "green")))
(check-expect (bulbs 2)
              (above
               (circle LIGHT-RADIUS
                       "solid"
                       "red")
               (circle LIGHT-RADIUS
                       "outline"
                       "yellow")
               (circle LIGHT-RADIUS                      
                       "outline"                       
                       "green")))
  

(define (bulbs lit)
  (above
   (circle LIGHT-RADIUS
           (if (= lit 2)
               "solid"
               "outline"
               )
           "red")
   (circle LIGHT-RADIUS
           (if (= lit 1)
               "solid"
               "outline"
               )
           "yellow")
   (circle LIGHT-RADIUS
           (if (= lit 0)
               "solid"
               "outline"
               )
           "green"))) 



(define-struct world (tick active-light lok ad))

(define (fn-for-world world)
  (... (world-tick world)
       (world-active-light world)
       (world-lok world)
       (world-ad world)))

(define START (make-world 0 0 empty ""))

(define (main world)
  (big-bang world
    (on-tick world-work)
    (on-draw render)
    (on-key key-hit)))




(check-random (world-work START)
              (make-world (+ (world-tick START) 1)
                          (check-light (world-tick START))
                          (update-kars (world-lok START) START) ""))
(check-random (world-work (make-world 100 2 empty "")) 
              (make-world (+ 100 1)
                          (check-light 100)
                          (update-kars empty (make-world 100 2 empty "")) ""))
                                 



(define (world-work world)
  (make-world (+ (world-tick world) 1)
              (check-light (world-tick world))
              (update-kars (world-lok world) world) (world-ad world)))



(check-expect (render START)
              (place-image (bulbs (world-active-light START))
                           X-POS
                           Y-POS
                           (place-image
                            (text (world-ad START) 20 "black")
                            (/ X-POS 2)
                            (/ Y-POS 2)
                            (second-text START))))
(check-expect (render (make-world 100 2
                                  (list (make-kar 200 200 4 3 AUTO-IMAGE1)
                                        (make-kar 350 300.2 8 6 AUTO-IMAGE2))
                                  ""))
              (place-image (bulbs 2)
                           X-POS
                           Y-POS
                           (place-image
                            (text "" 20 "black")
                            (/ X-POS 2)
                            (/ Y-POS 2)
                            (second-text
                             (make-world
                              100 2
                              (list
                               (make-kar 200 200 4 3 AUTO-IMAGE1)
                               (make-kar 350 300.2 8 6 AUTO-IMAGE2)) "")))))
                            

  
(define (render world)
  (place-image (bulbs (world-active-light world))
               X-POS
               Y-POS
               (place-image
                (text (world-ad world) 20 "black")
                (/ X-POS 2)
                (/ Y-POS 2)
                (second-text world))))



(check-expect (second-text START)
              (place-image (text (string-append
                                  "Seconds: "
                                  (number->string
                                   (ticks->seconds (world-tick START))))
                                 20
                                 "red")
                           60
                           24
                           (render-kars (world-lok START))))
(check-expect (second-text (make-world
                            100 2
                            (list (make-kar 200 200 4 3 AUTO-IMAGE1)
                                  (make-kar 350 300.2 8 6 AUTO-IMAGE2)) ""))
              (place-image (text (string-append
                                  "Seconds: "
                                  (number->string
                                   (ticks->seconds 100)))
                                 20
                                 "red")
                           60
                           24
                           (render-kars
                            (list (make-kar 200 200 4 3 AUTO-IMAGE1)
                                  (make-kar 350 300.2 8 6 AUTO-IMAGE2)))))

  

(define (second-text world)
  (place-image (text (string-append
                      "Seconds: "
                      (number->string (ticks->seconds (world-tick world))))
                     20
                     "red")
               60
               24
               (render-kars (world-lok world))))



(check-expect (render-kars empty) MTS)
(check-expect (render-kars (list
                            (make-kar 200 200 8 4 AUTO-IMAGE1) 
                            (make-kar 350 300.2 7 3.5 AUTO-IMAGE2)))
              (place-image AUTO-IMAGE1 200 200
                           (place-image AUTO-IMAGE2 350 300.2 MTS))) 
                           

  
(define (render-kars lok) 
  (cond [(empty? lok) MTS]
        [else
         (place-image (kar-image (first lok))
                      (kar-x (first lok))
                      (kar-y (first lok))
                      (render-kars (rest lok)))]))



(check-expect (check-light 0) 0)
(check-expect (check-light (* GREEN-LENGTH TICKS-SECOND)) 1)
(check-expect (check-light (* (+ GREEN-LENGTH YELLOW-LENGTH) TICKS-SECOND)) 2)
(check-expect (check-light
               (- (* CYCLE-LENGTH TICKS-SECOND) 1)) 2)
(check-expect (check-light
               (* CYCLE-LENGTH TICKS-SECOND)) 0)

 
(define (check-light tick)
  (local [(define second (remainder (ticks->seconds tick) CYCLE-LENGTH))]
    (cond [(< second GREEN-LENGTH) 0]
          [(< second (+ GREEN-LENGTH
                        YELLOW-LENGTH)) 1]
          [else 2]))) 
          








(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)



(define-struct kar (x y base-dx dx image))

(define (fn-for-kar kar)
  (... (kar-x kar)
       (kar-y kar)
       (kar-base-dx kar)
       (kar-dx kar)
       (kar-image kar)))





(define (rand-kar throwaway)
  (local [(define start-dx (+ 5 (random 5)))]
    (make-kar 0
              (+ (/ HEIGHT 2) (random (/ HEIGHT 2)))
              start-dx
              start-dx
              (pick-image (random 3)))))








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





(define (should-spawn? min)
  (< (random 100) min))





(define (update-kars lok world)
  (cond [(empty? lok) (if (should-spawn? 5)    
                          (list (rand-kar -1))  
                          empty)]
        [(> (kar-x (first lok)) WIDTH)   
         (update-kars (rest lok) world)]
        [else
         (cons (update-kar (first lok) world)
               (update-kars (rest lok) world))]))



(check-expect (update-kar (make-kar 200 200 8 8 AUTO-IMAGE1) START)
              (make-kar (+ 200 8)
                        200
                        8
                        8
                        AUTO-IMAGE1))
(check-expect (update-kar (make-kar 350 300.2 8 6 AUTO-IMAGE2)
                          (make-world
                           (- (*(+ GREEN-LENGTH YELLOW-LENGTH) TICKS-SECOND) 1)
                           1
                           (list (make-kar 200 200 4 3 AUTO-IMAGE1)
                                 (make-kar 350 300.2 8 6 AUTO-IMAGE2)) ""))
              (make-kar (+ 350 6)
                        300.2
                        8
                        6
                        AUTO-IMAGE2))
  

(define (update-kar kar world)
  (local [(define light (world-active-light world))
          (define pre-light? (< (kar-x kar) X-POS))
          (define base-dx (kar-base-dx kar))
          
          
          
          (define at-light? 
            (< (abs (- X-POS (kar-x kar))) base-dx))]                  
    (make-kar (+ (kar-x kar) (kar-dx kar))
              (kar-y kar)
              base-dx
              (cond [(= light 0) base-dx] 
                    [(and (= light 1)     
                          pre-light?)     
                     (* base-dx (/ 3 4))] 
                    [(and (= light 2)     
                          pre-light?)     
                     (* base-dx (/ 1 2))] 
                    [(and (= light 2)     
                          at-light?)      
                     0]                   
                    [else
                     base-dx])            
              (kar-image kar))))






(define (key-hit world k)
  (cond [(key=? "\r" k) (addString world "\n")]
        [(key=? "shift" k) world]    
        [(key=? "rshift" k) world]   
        [(key=? "control" k) world]  
        [(key=? "rcontrol" k) world] 
        [(key=? "\b" k) (delString world)]
        [(key=? "\t" k) (removeString world)]
        [else (addString world k)]))


(define (addString world ad)
  (make-world
   (world-tick world)
   (world-active-light world)
   (world-lok world)
   (string-append (world-ad world) ad)))



(define (delString world)
  (make-world
   (world-tick world)
   (world-active-light world)
   (world-lok world)
   (if (> (string-length (world-ad world)) 0)
        (substring (world-ad world) 0 (- (string-length (world-ad world)) 1))
        "")))



(define (removeString world)
  (make-world
   (world-tick world)
   (world-active-light world)
   (world-lok world)
   ""))