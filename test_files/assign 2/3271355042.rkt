

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assignment-2-traffic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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


(define MIN-SPEED 10) 
(define MAX-SPEED 20) 
(define 2001-HONDA-CIVIC  .)
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .) 





(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))

(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)



(define (seconds->ticks secs)
  (* secs TICKS-SECOND))

(check-expect (seconds->ticks 0) 0)
(check-expect (seconds->ticks 1) 28)
(check-expect (seconds->ticks 10) 280)
(check-expect (seconds->ticks 9) 252)
(check-expect (seconds->ticks 11) 308)




(define (pick-image val)
  (cond
    [(= val 0) 2001-HONDA-CIVIC]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))
(check-expect (pick-image 0) 2001-HONDA-CIVIC)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)







(define-struct auto (img speed xpos ypos))


(define (fn-for-auto auto)
  (... (fn-for-img (auto-img auto))
       (auto-speed auto)
       (auto-xpos auto)
       (auto-ypos auto)))


 


(define A1 (make-auto 2001-HONDA-CIVIC 20 0 0))
(define A2 (make-auto AUTO-IMAGE2 10 900 100))
(define A3 (make-auto AUTO-IMAGE3 5 500 200))
(define A4 (make-auto 2001-HONDA-CIVIC 30 800 300))






(define-struct ws (ticks color sincechange loa))
 


(define WS1 (make-ws 0 "green" 0 (list A1)))    
(define WS2 (make-ws                            
             (seconds->ticks GREEN-LENGTH)      
             "green"                            
             (seconds->ticks GREEN-LENGTH)
             (list A1 A3)))
(define WS3 (make-ws
             (seconds->ticks (+ GREEN-LENGTH    
                                YELLOW-LENGTH)) 
             "yellow"
             (seconds->ticks YELLOW-LENGTH)
             (list A1 A3)))
(define WS4 (make-ws
             (seconds->ticks (+ GREEN-LENGTH    
                                YELLOW-LENGTH   
                                RED-LENGTH))
             "red"
             (seconds->ticks RED-LENGTH)
             (list A1 A3)))
(define WS5 (make-ws
             (seconds->ticks (+ GREEN-LENGTH    
                                YELLOW-LENGTH   
                                RED-LENGTH
                                GREEN-LENGTH))
             "green"
             (seconds->ticks GREEN-LENGTH)
             (list A1 A3)))



(define (main ws)
  (big-bang ws
    (on-tick update)
    (to-draw render)))



(check-expect (remove-autos (list A1)) (list A1))
(check-expect (remove-autos (list A1 A2)) (list A1))
(check-expect (remove-autos (list A1 A2 A3 A4)) (list A1 A3))

(define (remove-autos loa)
  (cond
    [(empty? loa) loa]
    [else
     (if (>= (auto-xpos (first loa)) WIDTH)
         (remove-autos (rest loa))
         (cons (first loa) (remove-autos (rest loa))))]))




(check-expect (increment-x WS2) 
              (list (make-auto (auto-img (first (ws-loa WS2)))
                               (auto-speed (first (ws-loa WS2)))
                               (+ (auto-xpos (first (ws-loa WS2)))
                                  (auto-speed (first (ws-loa WS2))))
                               (auto-ypos (first (ws-loa WS2))))
                    (make-auto (auto-img (first (rest (ws-loa WS2))))
                               (auto-speed (first (rest (ws-loa WS2))))
                               (+ (auto-xpos (first (rest (ws-loa WS2))))
                                  (auto-speed (first (rest (ws-loa WS2)))))
                               (auto-ypos (first (rest (ws-loa WS2)))))))
(check-expect (increment-x WS3) 
              (list (make-auto (auto-img (first (ws-loa WS3)))
                               (auto-speed (first (ws-loa WS3)))
                               (+ (auto-xpos (first (ws-loa WS3)))
                                  (* (auto-speed (first (ws-loa WS3))) 0.75))
                               (auto-ypos (first (ws-loa WS3))))
                    (make-auto (auto-img (first (rest (ws-loa WS3))))
                               (auto-speed (first (rest (ws-loa WS3))))
                               (+ (auto-xpos (first (rest (ws-loa WS3))))
                                  (auto-speed (first (rest (ws-loa WS3)))))
                               (auto-ypos (first (rest (ws-loa WS3)))))))
(check-expect (increment-x WS4)  
              (list (make-auto (auto-img (first (ws-loa WS4)))
                               (auto-speed (first (ws-loa WS4)))
                               (+ (auto-xpos (first (ws-loa WS4)))
                                  (* (auto-speed (first (ws-loa WS4))) 0.5))
                               (auto-ypos (first (ws-loa WS4))))
                    (make-auto (auto-img (first (rest (ws-loa WS4))))
                               (auto-speed (first (rest (ws-loa WS4))))
                               (+ (auto-xpos (first (rest (ws-loa WS4))))
                                  (auto-speed (first (rest (ws-loa WS4)))))
                               (auto-ypos A3))))

(define (increment-x ws)
  (cond
    [(empty? (ws-loa ws)) empty] 
    [else 
     (cons (make-auto (auto-img (first (ws-loa ws))) 
                      (auto-speed (first (ws-loa ws))) 
                      (cond 
                        [(equal? "red" (ws-color ws))
                         (cond 
                           [(< (auto-xpos (first (ws-loa ws))) X-POS) 
                            (+ (* (auto-speed (first (ws-loa ws))) 0.5)
                               (auto-xpos (first (ws-loa ws))))]
                           [(and
                             (>= (auto-xpos (first (ws-loa ws))) 
                                 (- X-POS MAX-SPEED))
                             (<= (auto-xpos (first (ws-loa ws)))
                                 (+ X-POS MAX-SPEED)))
                            X-POS]
                           [else (+ (auto-speed (first (ws-loa ws))) 
                                    (auto-xpos (first (ws-loa ws))))])]
                        [(equal? "yellow" (ws-color ws))
                         (cond 
                           [(< (auto-xpos (first (ws-loa ws))) X-POS) 
                            (+ (* (auto-speed (first (ws-loa ws))) 0.75)
                               (auto-xpos (first (ws-loa ws))))]
                           [else (+ (auto-speed (first (ws-loa ws)))  
                                    (auto-xpos (first (ws-loa ws))))])]
                        [else (+ (auto-speed (first (ws-loa ws))) 
                                 (auto-xpos (first (ws-loa ws))))])
                      (auto-ypos (first (ws-loa ws)))) 
           (increment-x (make-ws (ws-ticks ws) 
                                 (ws-color ws)
                                 (ws-sincechange ws)
                                 (rest (ws-loa ws)))))]))








(define (update ws)
  (make-ws (+ (ws-ticks ws) 1) 
           (cond 
             [(and (equal? "green" (ws-color ws))
                   (= (ticks->seconds (+ (ws-sincechange ws) 1)) GREEN-LENGTH))
              "yellow"]
             [(and (equal? "yellow" (ws-color ws))
                   (= (ticks->seconds (+ (ws-sincechange ws) 1)) YELLOW-LENGTH))
              "red"]
             [(and (equal? "red" (ws-color ws))
                   (= (ticks->seconds (+ (ws-sincechange ws) 1)) RED-LENGTH))
              "green"]
             [else (ws-color ws)])
           (if (or (and (equal? "green" (ws-color ws)) 
                        (= (ticks->seconds (+ (ws-sincechange ws) 1)) GREEN-LENGTH))
                   (and (equal? "yellow" (ws-color ws))
                        (= (ticks->seconds (+ (ws-sincechange ws) 1)) YELLOW-LENGTH))
                   (and (equal? "red" (ws-color ws))
                        (= (ticks->seconds (+ (ws-sincechange ws) 1)) RED-LENGTH)))
               0
               (add1 (ws-sincechange ws))
               )
           (if (= (random 20) 0) 
               (cons (make-auto (pick-image (random 3)) 
                                (+ (random (- MAX-SPEED MIN-SPEED)) MIN-SPEED) 
                                0 
                                (+ (modulo (random HEIGHT) 
                                           (- HEIGHT (* 7 LIGHT-RADIUS)))
                                   (* 7 LIGHT-RADIUS)))
                     (increment-x ws)) 
               (increment-x ws))))



(check-expect (autos-imgs empty) empty) 
(check-expect (autos-imgs (list A1 A2 A3)) 
              (list (auto-img A1) (auto-img A2) (auto-img A3)))

(define (autos-imgs loa)
  (cond
    [(empty? loa) empty] 
    [else 
     (cons (auto-img (first loa)) (autos-imgs (rest loa)))]))



(check-expect (autos-posns empty) empty) 
(check-expect (autos-posns (list A1 A2 A3)) 
              (list (make-posn (auto-xpos A1) (auto-ypos A1))
                    (make-posn (auto-xpos A2) (auto-ypos A2))
                    (make-posn (auto-xpos A3) (auto-ypos A3))))

(define (autos-posns loa)
  (cond
    [(empty? loa) empty] 
    [else 
     (cons (make-posn (auto-xpos (first loa))
                      (auto-ypos (first loa)))
           (autos-posns (rest loa)))]))




(define (render ws)
  (place-images (append 
                 (list (above
                        (if (equal? (ws-color ws) "red")
                            (circle LIGHT-RADIUS "solid" "red")
                            (circle LIGHT-RADIUS "outline" "red"))
                        (if (equal? (ws-color ws) "yellow")
                            (circle LIGHT-RADIUS "solid" "yellow")
                            (circle LIGHT-RADIUS "outline" "yellow"))
                        (if (equal? (ws-color ws) "green")
                            (circle LIGHT-RADIUS "solid" "green")
                            (circle LIGHT-RADIUS "outline" "green")))
                       
                       (text (string-append "Time (s): "
                                            (number->string
                                             (ticks->seconds (ws-ticks ws))))
                             24
                             "red"))
                
                 (autos-imgs (ws-loa ws)))
                (append (list (make-posn X-POS Y-POS) 
                              (make-posn (+ X-POS (* 3 LIGHT-RADIUS)) Y-POS))
                        (autos-posns (ws-loa ws)))
                MTS)) 

(define START (make-ws 0 "green" 0 empty))
(main START)