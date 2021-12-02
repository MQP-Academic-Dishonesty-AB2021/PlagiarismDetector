

#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment2_MicahVargas_ChonladaDiMascolo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





(require 2htdp/image)
(require 2htdp/universe)



(define HEIGHT 600)                               
(define WIDTH 800)                                
(define COW-IMG .)               
(define MTS (empty-scene WIDTH HEIGHT))           



(define-struct cow (x y speed laps rotation))     



(define START (make-cow 50 75 10 0 1))            




(define (main cow)
  (big-bang cow
    (on-tick update-with-time)
    (on-key update-on-key)
    (on-mouse update-on-click)
    (to-draw render-cow)))








(define (update-with-time c)
  (cond
     [(> (cow-x c) WIDTH)
      (make-cow
       0
       (cow-y c)
       (cow-speed c)
       (+ (cow-laps c) 1)
       (* (cow-rotation c) -1))]
     [else 
      (make-cow
       (+ (cow-x c) (cow-speed c))
       (cow-y c)
       (cow-speed c)
       (cow-laps c)
       (* (cow-rotation c) -1))]))
    




(define (update-on-key c key-pressed)
  (cond
     [(key=? key-pressed "s")
      (make-cow
       (cow-x c)
       (cow-y c)
       (+ (cow-speed c) 1)
       (cow-laps c)
       (if (= (cow-speed c) 0)
           (+ (cow-rotation c) 1)
           (cow-rotation c)))]
     [(key=? key-pressed "a") 
      (cond
        [(> (cow-speed c) 0)
          (make-cow
           (cow-x c)
           (cow-y c)
           (- (cow-speed c) 1)
           (cow-laps c)
           (if (= (cow-speed c) 1)
               0
               (cow-rotation c)))]
        [else c])]
     [else c]))




(define (update-on-click c x y mouse)
  (cond
    [(mouse=? mouse "button-down")
      (make-cow
       x
       y
       (cow-speed c)
       (cow-laps c)
       (cow-rotation c))]
    [else c]))






(define (render-cow c)
  (place-images
   (list (text (string-append "Speed: " (number->string (cow-speed c)) "\n"
                              "Laps: " (number->string (cow-laps c)))
               20
               "red")
         (rotate (cow-rotation c) COW-IMG))
   (list (make-posn 50 25)
         (make-posn (cow-x c) (cow-y c)))
   MTS))





(require 2htdp/image)
(require 2htdp/universe)



(define HEIGHT 600)                              
(define WIDTH 800)                               
(define X-POS (/ WIDTH 2))                       
(define Y-POS (/ HEIGHT 4))                      
(define MTS (empty-scene WIDTH HEIGHT))          

(define TICKS-SECOND 28) 

(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 






(define (build-signal state)
  (cond
    [(string=? state "green")
     (above
      (circle LIGHT-RADIUS "solid" "green")
      (circle LIGHT-RADIUS "outline" "yellow")
      (circle LIGHT-RADIUS "outline" "red"))]
    [(string=? state "yellow")
     (above
      (circle LIGHT-RADIUS "outline" "green")
      (circle LIGHT-RADIUS "solid" "yellow")
      (circle LIGHT-RADIUS "outline" "red"))]
    [(string=? state "red")
     (above
      (circle LIGHT-RADIUS "outline" "green")
      (circle LIGHT-RADIUS "outline" "yellow")
      (circle LIGHT-RADIUS "solid" "red"))]
    [else
     (above
      (circle LIGHT-RADIUS "outline" "green")
      (circle LIGHT-RADIUS "outline" "yellow")
      (circle LIGHT-RADIUS "outline" "red"))]))



(define-struct signal (state ticks-since-switch total-ticks visual)) 







(define (ticks->seconds t)
  (floor (/ t TICKS-SECOND)))
(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)



(define START (make-signal "green" 0 0 (build-signal "green"))) 



(define (main signal)
  (big-bang signal
    (on-tick update-signal)
    (to-draw render-signal)))





(define (update-signal s)
  (cond
    [(and
      (string=? (signal-state s) "green")
      (>= (ticks->seconds (signal-ticks-since-switch s)) (- GREEN-LENGTH 1/28)))
     (make-signal
      "yellow"
      0
      (+ (signal-total-ticks s) 1)
      (build-signal "yellow"))]
    [(and
      (string=? (signal-state s) "yellow")
      (>= (ticks->seconds (signal-ticks-since-switch s)) (- YELLOW-LENGTH 1/28)))
     (make-signal
      "red"
      0
      (+ (signal-total-ticks s) 1)
      (build-signal "red"))]
    [(and
      (string=? (signal-state s) "red")
      (>= (ticks->seconds (signal-ticks-since-switch s)) (- RED-LENGTH 1/28)))
     (make-signal
      "green"
      0
      (+ (signal-total-ticks s) 1)
      (build-signal "green"))]
    [else (make-signal
           (signal-state s)
           (+ (signal-ticks-since-switch s) 1)
           (+ (signal-total-ticks s) 1)
           (signal-visual s))]))


(define (render-signal s)
  (place-images
   (list (text (string-append "Seconds: " (number->string (ticks->seconds (signal-total-ticks s))))
               20
               "red")
         (signal-visual s))
   (list (make-posn 50 25)
         (make-posn X-POS Y-POS))
   MTS))





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

(define AUTO-IMAGE1 .)
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)







(define (ticks->seconds t)
  (floor (/ t TICKS-SECOND)))
(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)





(define (build-signal state)
  (cond
    [(string=? state "green")
     (above
      (circle LIGHT-RADIUS "solid" "green")
      (circle LIGHT-RADIUS "outline" "yellow")
      (circle LIGHT-RADIUS "outline" "red"))]
    [(string=? state "yellow")
     (above
      (circle LIGHT-RADIUS "outline" "green")
      (circle LIGHT-RADIUS "solid" "yellow")
      (circle LIGHT-RADIUS "outline" "red"))]
    [(string=? state "red")
     (above
      (circle LIGHT-RADIUS "outline" "green")
      (circle LIGHT-RADIUS "outline" "yellow")
      (circle LIGHT-RADIUS "solid" "red"))]
    [else
     (above
      (circle LIGHT-RADIUS "outline" "green")
      (circle LIGHT-RADIUS "outline" "yellow")
      (circle LIGHT-RADIUS "outline" "red"))]))





(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))
(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)



(define-struct signal (state ticks-since-switch total-ticks visual)) 
(define-struct auto (x y speed ogSpeed visual))                      
(define-struct simulation (signal loa))                              



(define START (make-simulation (make-signal "green" 0 0 (build-signal "green")) (list (make-auto 0 300 3 3 .) (make-auto 1 350 2 2 .)))) 




(define (main simulation)
  (big-bang simulation
    (on-tick update-simulation)
    (to-draw render-simulation)))







(define (update-auto-speeds speed-change loa)
  (cond
    [(empty? loa) empty]
    [else
     (cons
      (if (<= (auto-x (first loa)) (/ WIDTH 2))
          (make-auto
           (auto-x (first loa))
           (auto-y (first loa))
           (* (auto-ogSpeed (first loa)) speed-change)
           (auto-ogSpeed (first loa))
           (auto-visual (first loa)))
          (make-auto 
           (auto-x (first loa))
           (auto-y (first loa))
           (auto-ogSpeed(first loa))
           (auto-ogSpeed (first loa))
           (auto-visual (first loa))))
      (update-auto-speeds speed-change (rest loa)))]))







(define (time-to-switch? s state-check state-length)
  (and
   (string=? (signal-state (simulation-signal s)) state-check)
   (>= (ticks->seconds (signal-ticks-since-switch (simulation-signal s))) (- state-length 1/28))))






(define (switch-state s to-state speed-change)
  (make-simulation
   (make-signal
    to-state
    0
    (+ (signal-total-ticks (simulation-signal s)) 1)
    (build-signal to-state))
   (update-auto-speeds speed-change (simulation-loa s))))




(define (update-auto-positions state loa)
  (cond
    [(empty? loa) empty]
    [else
     (if (<= (auto-x (first loa)) WIDTH)
         (cons
          (cond
            [(and (= (auto-x (first loa)) (/ WIDTH 2)) (string=? state "red"))
             (make-auto
              (auto-x (first loa))
              (auto-y (first loa))
              0
              (auto-ogSpeed (first loa))
              (auto-visual (first loa)))]
            [else
             (make-auto
              (+ (auto-x (first loa)) (auto-speed (first loa)))
              (auto-y (first loa))
              (if (<= (auto-x (first loa)) (/ WIDTH 2))
                  (cond
                    [(string=? state "green") (auto-ogSpeed (first loa))]
                    [(string=? state "yellow") (* (auto-ogSpeed (first loa)) 0.75)]
                    [(string=? state "red") (* (auto-ogSpeed (first loa)) 0.5)]
                    [else (auto-ogSpeed (first loa))])
                  (auto-ogSpeed (first loa)))
              (auto-ogSpeed (first loa))
              (auto-visual (first loa)))])
          (update-auto-positions state (rest loa)))
         (update-auto-positions state (rest loa)))]))




(define (update-simulation s)
  (cond
    [(time-to-switch? s "green" GREEN-LENGTH)
     (switch-state s "yellow" 0.75)]
    [(time-to-switch? s "yellow" YELLOW-LENGTH) 
     (switch-state s "red" 0.5)]
    [(time-to-switch? s "red" RED-LENGTH)
     (switch-state s "green" 1)]
    [else
     (make-simulation
      (make-signal
       (signal-state (simulation-signal s))
       (+ (signal-ticks-since-switch (simulation-signal s)) 1)
       (+ (signal-total-ticks (simulation-signal s)) 1)
       (signal-visual (simulation-signal s)))
      (if (<= (random 500) 5)
          (append (update-auto-positions (signal-state (simulation-signal s)) (simulation-loa s))
                  (list (make-auto 0 (+ (random 200) 400) (+ (random 3) 3) (+ (random 3) 3) (pick-image (random 3)))))
          (update-auto-positions (signal-state (simulation-signal s)) (simulation-loa s))))]))




(define (render-auto-imgs loa)
  (cond
    [(empty? loa) empty]
    [else
     (cons
      (auto-visual (first loa))
      (render-auto-imgs (rest loa)))]))




(define (render-auto-xys loa)
  (cond
    [(empty? loa) empty]
    [else
     (cons
      (make-posn (auto-x (first loa)) (auto-y (first loa)))
      (render-auto-xys (rest loa)))]))







(define (render-simulation s)
  (place-images
   (append
    (list (text (string-append "Seconds: " (number->string (ticks->seconds (signal-total-ticks (simulation-signal s)))))
               20
               "red")
         (signal-visual (simulation-signal s)))
    (render-auto-imgs (simulation-loa s)))
   (append
    (list (make-posn 50 25)
         (make-posn X-POS Y-POS))
    (render-auto-xys (simulation-loa s)))
   MTS))