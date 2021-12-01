

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Blake Mcleod Albert Lewis |) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)
(require 2htdp/universe)











(define-struct cow (x y degree speed lap))












(define HEIGHT 600)
(define WIDTH 800)
(define COW .)
(define SPEED 10)
(define ADDSPEED 10)
(define SUBSPEED 10)
(define MTS (empty-scene WIDTH HEIGHT)) 
(define LAP 0) 
(define ANGLEB -2)
(define ANGLEA 2) 
(define CNTY (/ HEIGHT 2))
(define CNTX (/ WIDTH 2))
(define STARTC (make-cow CNTX CNTY ANGLEA SPEED LAP ))
(define IC STARTC)
(define EC (make-cow WIDTH CNTY ANGLEA SPEED LAP ))





 
(define (mainC STARTCOW)
  (big-bang STARTCOW                      
    (on-tick   move-cow)
    (to-draw   render-cow)
    (on-key    change-speed)      
    (on-mouse  change-position)))








(check-expect (move-cow IC) (make-cow (+ (cow-x IC) (cow-speed IC)) 
                                      (cow-y IC) 
                                      ANGLEB
                                      (cow-speed IC)
                                      (cow-lap IC)))
(check-expect (move-cow EC) (make-cow 0 (cow-y EC)
                                      (cow-degree EC)
                                      (cow-speed EC)
                                      (add1 (cow-lap EC))))
(check-expect (move-cow (make-cow  (cow-x IC)
                                   (cow-y IC) 
                                   ANGLEB
                                   (cow-speed IC)
                                   (cow-lap IC))) (make-cow (+ (cow-x IC) (cow-speed IC)) 
                                                            (cow-y IC) 
                                                            ANGLEA
                                                            (cow-speed IC)
                                                            (cow-lap IC)))
(check-expect (move-cow (make-cow  (cow-x IC)
                                   (cow-y IC) 
                                   ANGLEB
                                   0
                                   (cow-lap IC)))(make-cow  (cow-x IC)
                                                            (cow-y IC) 
                                                            ANGLEB
                                                            0
                                                            (cow-lap IC)))
(define (move-cow c)
  (cond
    [(= (cow-speed c) 0) c]
    [(>= (cow-x c) WIDTH) (make-cow 0 (cow-y c) (cow-degree c) (cow-speed c) (add1 (cow-lap c)))]
    [else (cond [(positive? (cow-degree c)) (make-cow (+ (cow-x c) (cow-speed c)) 
                                                      (cow-y c) 
                                                      ANGLEB
                                                      (cow-speed c)
                                                      (cow-lap c))]
                [else
                 (make-cow (+ (cow-x c) (cow-speed c))
                           (cow-y c) 
                           ANGLEA
                           (cow-speed c)
                           (cow-lap c))])]))








      
        
            
        








(check-expect (render-cow IC)
              (place-image
               (rotate (cow-degree IC) COW) (cow-x IC) (cow-y IC)
               (place-image
                (text (string-append "Laps " (number->string (cow-lap IC))) 20 "Blue") 50 30
                (place-image
                 (text (string-append
                        (number->string (cow-speed IC)) " Pixels Per Tick") 20 "Blue")
                 100 50 MTS))))
(check-expect (render-cow EC)
              (place-image
               (rotate (cow-degree EC) COW) (cow-x EC) (cow-y EC)
               (place-image
                (text (string-append "Laps " (number->string (cow-lap EC))) 20 "Blue") 50 30
                (place-image
                 (text (string-append
                        (number->string (cow-speed EC)) " Pixels Per Tick") 20 "Blue")
                 100 50 MTS))))


(define (render-cow c)
  (place-image  (rotate (cow-degree c) COW) (cow-x c) (cow-y c)
                (place-image  (text (string-append "Laps " (number->string (cow-lap c))) 20 "Blue") 50 30
                              (place-image
                               (text (string-append (number->string (cow-speed c)) " Pixels Per Tick") 20 "Blue") 100 50 MTS))))
	




(check-expect (change-speed IC "a") (make-cow (cow-x IC) (cow-y IC) (cow-degree IC) (- (cow-speed IC) SUBSPEED) (cow-lap IC)))
(check-expect (change-speed EC "a") (make-cow (cow-x EC) (cow-y EC) (cow-degree EC) (- (cow-speed EC) SUBSPEED) (cow-lap EC)))
(check-expect (change-speed IC "s") (make-cow (cow-x IC) (cow-y IC) (cow-degree IC) (+ (cow-speed IC) SUBSPEED) (cow-lap IC)))
(check-expect (change-speed EC "s") (make-cow (cow-x EC) (cow-y EC) (cow-degree EC) (+ (cow-speed EC) SUBSPEED) (cow-lap EC)))
(check-expect (change-speed (make-cow (cow-x EC) (cow-y EC) (cow-degree EC) 0 (cow-lap EC))  "s")
              (make-cow (cow-x EC) (cow-y EC) (cow-degree EC) (+ 0 SUBSPEED) (cow-lap EC)))
(check-expect (change-speed (make-cow (cow-x EC) (cow-y EC) (cow-degree EC) 0 (cow-lap EC))  "a")
              (make-cow (cow-x EC) (cow-y EC) (cow-degree EC) 0 (cow-lap EC)))
(check-expect (change-speed IC "i") IC)

(define (change-speed c Kevent)
  (cond [(key=? Kevent "a") 
         (cond [(<= (cow-speed c) 0) (make-cow (cow-x c) (cow-y c) (cow-degree c) 0 (cow-lap c))]
               [else
                (make-cow (cow-x c) (cow-y c) (cow-degree c) (- (cow-speed c) SUBSPEED) (cow-lap c))])]
        [(key=? Kevent "s") (make-cow (cow-x c) (cow-y c) (cow-degree c) (+ (cow-speed c) ADDSPEED) (cow-lap c))]
        [else c]))
              	
              	
                




(check-expect (change-position IC 50 100 "button-down") (make-cow 50 100 (cow-degree IC) (cow-speed IC) (cow-lap IC)))
(check-expect (change-position EC 25 140 "button-down") (make-cow 25 140 (cow-degree IC) (cow-speed IC) (cow-lap IC)))
(check-expect (change-position EC 25 140 "button-up") EC)

(define (change-position c x-cor y-cor me)
  (cond [(mouse=? me "button-down") (make-cow x-cor y-cor (cow-degree c) (cow-speed c) (cow-lap c))]
        [else c]))













(require 2htdp/image)
(require 2htdp/universe)


 

(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
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



(define-struct TraffL (time state totaltime loa))




(define-struct auto (x y speed image))




 













  
(define START (make-TraffL 0 "green" 0 empty))





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






(define (main START)
  (big-bang START                      
    (on-tick   track-time)
    (to-draw   render)))








(define (track-time signal)
  (cond [(string=? (TraffL-state signal) "red")
        (cond [(= RED-LENGTH (ticks->seconds(TraffL-time signal))) (make-TraffL 0 "green" (add-tsec signal) (cd (TraffL-loa signal)))]
              [else
               (make-TraffL (add-sec signal) "red" (add-tsec signal) (stop-autos(cd (TraffL-loa signal))))])]
        [(string=? (TraffL-state signal) "yellow")
        (cond [(= YELLOW-LENGTH (ticks->seconds(TraffL-time signal))) (make-TraffL 0 "red" (add-tsec signal) (cd (TraffL-loa signal)))]
              [else
               (make-TraffL (add-sec signal) "yellow" (add-tsec signal)(slow-autos (cd (TraffL-loa signal))))])]
        [(string=? (TraffL-state signal) "green")
        (cond [(= GREEN-LENGTH (ticks->seconds(TraffL-time signal))) (make-TraffL 0 "yellow" (add-tsec signal) (cd (TraffL-loa signal)))]
              [else
               (make-TraffL (add-sec signal) "green" (add-tsec signal) (move-autos (cd (TraffL-loa signal))))])]))

 






(define (add-sec signal)
  (+  1 (TraffL-time signal)))






(define (add-tsec signal)
  (+  1 (TraffL-totaltime signal)))  




(define (render signal)
  (cond [(string=? (TraffL-state signal) "green")
        (place-image
   (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "solid" "green")) X-POS Y-POS
                                          (place-image
                                           (text (number->string(ticks->seconds(TraffL-totaltime signal)))
                                                 20 "blue")(+ 100 X-POS) Y-POS (render-auto (TraffL-loa signal))))]
        
   [(string=? (TraffL-state signal) "yellow")
        (place-image
   (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "solid" "yellow") 
   (circle LIGHT-RADIUS "outline" "green")) X-POS Y-POS
                                            (place-image
                                             (text (number->string(ticks->seconds(TraffL-totaltime signal)))
                                                   20 "blue")(+ 100 X-POS) Y-POS (render-auto (TraffL-loa signal))))]
   [else
        (place-image
   (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "outline" "green")) X-POS Y-POS
                                            (place-image
                                             (text (number->string(ticks->seconds(TraffL-totaltime signal)))
                                                   20 "blue")(+ 100 X-POS) Y-POS (render-auto (TraffL-loa signal))))]))






(define (render-auto loa)
  (cond [ (empty? loa) MTS]
        [ else
         (place-image (auto-image (first loa)) (auto-x (first loa)) (auto-y (first loa)) (render-auto (rest loa)))]))
 




(define (cd loa)
  (delete-auto (create-auto loa)))





  




(define (create-auto loa)
  (cond [(= (random 10) 1)
         (cons (make-auto 0 (+  (/ HEIGHT 2) (random (/ HEIGHT 2))) (+ 10 (random 10)) (pick-image (random 3))) loa)]
        [else loa]))





(define (delete-auto loa)
  (cond [(empty? loa) empty]
        [else
         (if (>= (auto-x (first loa)) WIDTH)
             (delete-auto (rest loa))
         (cons (first loa) (delete-auto (rest loa))))]))



(define (move-autos loa)
  (cond [(empty? loa) empty]
        [else
         (cons (move-auto (first loa)) (move-autos (rest loa)))]))




(define (slow-autos loa)
  (cond [(empty? loa) empty]
        [(< X-POS (auto-x(first loa))) (cons (move-auto (first loa)) (slow-autos (rest loa))) ]
        [else
         (cons (slow-auto (first loa)) (slow-autos (rest loa)))]))





(define (stop-autos loa)
  (cond [(empty? loa) empty]
        [(or (and (> X-POS (auto-x(first loa))) (< X-POS (+ (auto-x(first loa)) (auto-speed(first loa))))) (= X-POS (auto-x(first loa))))
         (cons (stop-auto (first loa)) (stop-autos (rest loa)))]
        [(> (auto-x(first loa)) X-POS) (cons (move-auto (first loa)) (stop-autos (rest loa)))]                                       
        [else
         (cons (make-auto (+ (* .5 (auto-speed (first loa))) (auto-x (first loa))) (auto-y (first loa)) (auto-speed (first loa)) (auto-image (first loa))) (stop-autos (rest loa)))])) 

(define (move-auto a)
   (make-auto (+ (auto-x a) (auto-speed a)) (auto-y a) (auto-speed a) (auto-image a)))

(define (slow-auto a)
   (make-auto (+ (* .75 (auto-speed a)) (auto-x a)) (auto-y a) (auto-speed a) (auto-image a)))

(define (stop-auto a)
   (make-auto X-POS (auto-y a) (auto-speed a) (auto-image a)))






        



















