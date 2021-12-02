

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Assignment2_Final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)

(define MTS (empty-scene WIDTH HEIGHT))


(define COW .)

(define COW- (rotate -2 COW))
(define COW+ (rotate  2 COW))





(define-struct cow (x y dx laps ticks))







(define START (make-cow 0 (/ HEIGHT 2) 10 0 0))

  








(define (main c)
  (big-bang c
    (on-tick next-cow)       
    (to-draw render-cow)     
    (on-key  handle-key)     
    (on-mouse handle-mouse)))   



(check-expect (next-cow (make-cow 0 200 1 0 0)) (make-cow 1 200 1 0 1)) 
(check-expect (next-cow (make-cow 50 200 10 0 0)) (make-cow 60 200 10 0 1))

(check-expect (next-cow (make-cow (- WIDTH 5) 200 10 0 0)) (make-cow 5 200 10 1 1)) 


(define (next-cow c)
  (cond [(> (next-x c) WIDTH) (make-cow (- (next-x c) WIDTH) (cow-y c) (cow-dx c) (add1 (cow-laps c)) (add1 (cow-ticks c)))]
        [else
         (make-cow (next-x c) (cow-y c) (cow-dx c) (cow-laps c) (add1 (cow-ticks c)))]))



(define (next-x c)
  (+ (cow-x c) (cow-dx c)))



(define (render-cow c) 
  (place-image (text (string-append "Speed: " (number->string (cow-dx c))) 20 "blue") 100 50
               (place-image (text (string-append "Laps: " (number->string (cow-laps c))) 20 "blue") 100 75
                            (place-image (choose-image c) (cow-x c) (cow-y c) MTS))))



(define (choose-image c)
  (cond [(= (cow-dx c) 0) COW]
        [else
         (if (odd? (cow-ticks c)) COW-
             COW+)]))



(define (handle-key c ke)
  (cond [(and (key=? ke "a") (> (cow-dx c) 0))
         (make-cow (cow-x c) (cow-y c)
                   (sub1 (cow-dx c)) (cow-laps c) (cow-ticks c))]
        [(key=? ke "s")
         (make-cow (cow-x c) (cow-y c)
                   (add1 (cow-dx c)) (cow-laps c) (cow-ticks c))]
        [else c]))



(define (handle-mouse c x-cor y-cor me)
  (cond [(mouse=? me "button-down") (make-cow x-cor y-cor (cow-dx c) (cow-laps c) (cow-ticks c))]
        [else c]))







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



(define RON
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))


(define YON
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))


(define GON
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








(define-struct world-state (t loa))






(define-struct auto (img x y full-dx))




(define START (make-world-state 0 (list (make-auto AUTO-IMAGE1 0 600 4))))



(define (main ws)
  (big-bang ws
    (on-tick next-tick)   
    (to-draw render-screen))) 


(define autoUnderLight (make-auto AUTO-IMAGE1 X-POS 500 10)) 
(define autoLeftLight (make-auto AUTO-IMAGE1 (- X-POS 200) 500 10)) 
(define autoRightLight (make-auto AUTO-IMAGE1 (+ X-POS 200) 500 10)) 


(define RED-ws (make-world-state (+ (* YELLOW-LENGTH 28) (* GREEN-LENGTH 28)) (list empty))) 
(define YELLOW-ws (make-world-state (* GREEN-LENGTH 28) (list empty))) 
(define GREEN-ws (make-world-state 0 (list empty))) 








(check-expect (move-auto autoUnderLight RED-ws) (make-auto AUTO-IMAGE1 X-POS 500 10)) 
(check-expect (move-auto autoLeftLight RED-ws) (make-auto AUTO-IMAGE1 (+ (- X-POS 200) 5) 500 10))
(check-expect (move-auto autoRightLight RED-ws) (make-auto AUTO-IMAGE1 (+ (+ X-POS 200) 10) 500 10))

(check-expect (move-auto autoLeftLight YELLOW-ws) (make-auto AUTO-IMAGE1 (+ (- X-POS 200) 7.5) 500 10))
(check-expect (move-auto autoRightLight YELLOW-ws) (make-auto AUTO-IMAGE1 (+ (+ X-POS 200) 10) 500 10))

(check-expect (move-auto autoRightLight GREEN-ws) (make-auto AUTO-IMAGE1 (+ (+ X-POS 200) 10) 500 10))

(define (move-auto auto ws)
   (make-auto (auto-img auto)
              (cond [(or (equal? (light-status ws) "green") (> (auto-x auto) X-POS)) (+ (auto-x auto) (auto-full-dx auto))]
                    [(and (equal? (light-status ws) "yellow") (<= (auto-x auto) X-POS)) (+ (auto-x auto) (* (/ 3 4) (auto-full-dx auto)))]
                    [(and (> (auto-x auto) (- X-POS LIGHT-RADIUS)) (< (auto-x auto) (+ X-POS LIGHT-RADIUS))) (auto-x auto)]
                    [else
                     (+ (auto-x auto) (* (/ 1 2) (auto-full-dx auto)))])
              (auto-y auto) (auto-full-dx auto)))



(define (move-autos loa ws)
  (cond [(empty? loa)
         (if (< (random 100) 5) (list (make-auto (pick-image (random 3)) 0 (+ (/ HEIGHT 2) (random (/ HEIGHT 2))) (+ 4 (random 17)))) 
             empty)]
        [else
         (cons (move-auto (first loa) ws)
                 (move-autos (rest loa) ws))]))
  


(define (next-tick ws)
  (make-world-state (add1 (world-state-t ws)) (move-autos (world-state-loa ws) ws)))




(define (light-status ws)
  (local [(define total-secs (floor (/ (world-state-t ws) 28)))
          (define range-secs (modulo total-secs (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)))]
    (cond [(>= range-secs (+ GREEN-LENGTH YELLOW-LENGTH)) "red"]
          [(>= range-secs GREEN-LENGTH) "yellow"]
          [else "green"])))



(define (render-autos loa)
  (cond [(empty? loa)
         MTS]
        [else
         (place-image (auto-img (first loa)) (auto-x (first loa)) (auto-y (first loa))
                      (render-autos (rest loa)))]))



(define (render-screen ws)
  (local [(define total-secs (floor (/ (world-state-t ws) 28)))
          (define range-secs (modulo total-secs (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)))]
    (place-image (cond [(equal? (light-status ws) "red") RON]
                       [(equal? (light-status ws) "yellow") YON]
                       [else GON]) X-POS Y-POS
                                   (place-image (text (number->string total-secs) 24 "black") (+ X-POS 100) Y-POS
                                                (render-autos (world-state-loa ws))))))