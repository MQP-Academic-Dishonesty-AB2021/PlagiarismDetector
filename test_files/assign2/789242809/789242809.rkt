

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |assignment 2 Traffic signal|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)


(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)
(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT))
(define TICKS-SECOND 28) 
(define RANDOM-MIN 5)  
(define RANDOM-MAX 15) 
(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 
(define CYCLE-LENGTH (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)) 
(define YELLOW-SPEED 0.75) 
(define RED-SPEED 0.5) 
(define CAR-CHANCE-DENOMINATOR 20) 
(define MIN-Y-POS-AUTO (+ (/ HEIGHT 4) (* 5 LIGHT-RADIUS))) 
(define FONT-SIZE 24) 
(define FONT-COLOR "black") 
(define FONT-X (+ X-POS (* 2 LIGHT-RADIUS)))
(define FONT-Y (/ (+ Y-POS (* 4 LIGHT-RADIUS)) 2))
(define BILLBOARD-X (/ WIDTH 4))
(define BILLBOARD-Y (/ (+ Y-POS (* 4 LIGHT-RADIUS)) 2))
(define LINE-BREAK-CHARACTER #\~)











(define-struct auto (x y dx color))
(define A1 (make-auto 0 400 5 0))
(define A2 (make-auto 20 500 10 2))
(define A3 (make-auto WIDTH 450 7 1))
(define A4 (make-auto 600 425 15 0))

 

       






(define LA1 empty)
(define LA2 (list A1 A2))
(define LA3 (list A4 A3 A2))

 



(define-struct ws (tick autos billboards))



(define WS1 (make-ws 0 empty (cons "" empty)))
(define WS2 (make-ws 30
                     (list (make-auto 0 400 10 2)
                           (make-auto 30 450 8 1))
                     (cons "" empty)))
(define WS3 (make-ws 0
                     (list (make-auto 600 500 5 0)
                           (make-auto X-POS 425 7 2))
                     (cons "" empty)))
(define WS4 (make-ws 0
                     (list (make-auto 600 500 5 0)
                           (make-auto X-POS 425 7 2))
                     (cons "" (cons "aa" empty))))

    






(define (main ws)
  (big-bang ws                   
    (on-tick   tock)     
    (to-draw   render)   
    (on-key    key-detect)))    

(define START WS1)









(define (tock ws)
  (make-ws (+ 1 (ws-tick ws))     
           (advance-autos (ws-tick ws) (ws-autos ws))        
           (ws-billboards ws)))
        
        






(define (advance-autos tick loa)  
  (modifier--loa tick loa))
 
 



(check-expect (modifier--auto (* TICKS-SECOND (floor (/ GREEN-LENGTH 2))) A1)
              (make-auto
               (+ (auto-dx A1) (auto-x A1))
               (auto-y A1)
               (auto-dx A1)
               (auto-color A1)))
(check-expect (modifier--auto (* TICKS-SECOND GREEN-LENGTH) A4)
              (make-auto
               (+ (auto-dx A4) (auto-x A4))
               (auto-y A4)
               (auto-dx A4)
               (auto-color A4)))
(check-expect (modifier--auto (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH)) A2)
              (make-auto
               (auto-move-red (auto-x A2) (auto-dx A2))
               (auto-y A2)
               (auto-dx A2)
               (auto-color A2)))



(define (modifier--auto tick auto) 
  (cond [(<= (auto-x auto) X-POS)
         (if (string=? (current-light tick) "red")
             (make-auto
              (auto-move-red (auto-x auto) (auto-dx auto))
              (auto-y auto)
              (auto-dx auto)
              (auto-color auto))
             (make-auto
              (+ (auto-x auto) (* (modifier-speed tick) (auto-dx auto)))
              (auto-y auto)
              (auto-dx auto)
              (auto-color auto)))]       
        [else 
         (make-auto
          (+ (auto-dx auto) (auto-x auto))
          (auto-y auto)
          (auto-dx auto)
          (auto-color auto))]))








(define (modifier--loa tick loa)
  (cond [(and (empty? loa) 
              (= (random CAR-CHANCE-DENOMINATOR) 0)) 
         (cons (make-auto 0
                          (+ MIN-Y-POS-AUTO (random (- HEIGHT MIN-Y-POS-AUTO LIGHT-RADIUS)))
                          (+ RANDOM-MIN (random (- RANDOM-MAX RANDOM-MIN)))
                          (random 3))
               empty)]
        [(empty? loa) empty]
        [(> (auto-x (first loa)) WIDTH) (modifier--loa tick (rest loa))]
        [else 
         (cons (modifier--auto tick (first loa)) (modifier--loa tick (rest loa)))]))








(define (auto-move-red x dx)
  (cond [(>= (+ x (* RED-SPEED dx)) X-POS) X-POS]
        [else (+ x (* RED-SPEED dx))]))

(check-expect (auto-move-red (- X-POS 2) 5) X-POS)
(check-expect (auto-move-red (- X-POS 100) 5) (+ (- X-POS 100) (* RED-SPEED 5)))







(define (modifier-speed tick)
  (cond [(string=? "green" (current-light tick)) 1]
        [(string=? "yellow" (current-light tick)) YELLOW-SPEED]
        [else RED-SPEED]))

(check-expect (modifier-speed (* TICKS-SECOND (floor (/ GREEN-LENGTH 2)))) 1)
(check-expect (modifier-speed (* TICKS-SECOND (floor (+ CYCLE-LENGTH (/ GREEN-LENGTH 2))))) 1)
(check-expect (modifier-speed 0) 1)
(check-expect (modifier-speed (* TICKS-SECOND GREEN-LENGTH)) YELLOW-SPEED)
(check-expect (modifier-speed (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH))) RED-SPEED)







(define (current-light tick)
  (cond [(< (cycle-second tick) GREEN-LENGTH) "green"]
        [(< (cycle-second tick) (+ GREEN-LENGTH YELLOW-LENGTH)) "yellow"]
        [else "red"]))

(check-expect (current-light (* TICKS-SECOND (floor (/ GREEN-LENGTH 2)))) "green")
(check-expect (current-light (* TICKS-SECOND (floor (+ CYCLE-LENGTH (/ GREEN-LENGTH 2))))) "green")
(check-expect (current-light 0) "green")
(check-expect (current-light (* TICKS-SECOND GREEN-LENGTH)) "yellow")
(check-expect (current-light (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH))) "red")







(define (cycle-second tick)
  (remainder (ticks->seconds tick)
             CYCLE-LENGTH))

(check-expect (cycle-second (* TICKS-SECOND (floor (/ GREEN-LENGTH 2)))) (floor (/ GREEN-LENGTH 2)))
(check-expect (cycle-second (* TICKS-SECOND (floor (+ CYCLE-LENGTH (/ GREEN-LENGTH 2))))) (floor (/ GREEN-LENGTH 2)))
(check-expect (cycle-second 0) 0)
(check-expect (cycle-second (* TICKS-SECOND GREEN-LENGTH)) GREEN-LENGTH)
(check-expect (cycle-second (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH))) (+ YELLOW-LENGTH GREEN-LENGTH))




(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))

(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)








(define (render ws)
  (place-image (billboard-text (ws-billboards ws))
               BILLBOARD-X
               BILLBOARD-Y
               (place-image   (timer-on (ws-tick ws))
                              FONT-X
                              FONT-Y
                              (place-image   (BULBS (ws-tick ws))
                                             X-POS
                                             Y-POS
                                             (auto-on (ws-autos ws) MTS)))))

(check-expect (render WS1)
              (place-image (text "0" FONT-SIZE FONT-COLOR)
                           FONT-X
                           FONT-Y
                           (place-image
                            (above (circle LIGHT-RADIUS "outline" "red")
                                   (circle LIGHT-RADIUS "outline" "yellow")
                                   (circle LIGHT-RADIUS "solid" "green"))
                            X-POS
                            Y-POS
                            MTS)))
(check-expect (render WS2)
              (place-image (text "1" FONT-SIZE FONT-COLOR)
                           FONT-X
                           FONT-Y
                           (place-image
                            (above (circle LIGHT-RADIUS "outline" "red")
                                   (circle LIGHT-RADIUS "outline" "yellow")
                                   (circle LIGHT-RADIUS "solid" "green"))
                            X-POS
                            Y-POS
                            (place-image 
                             (pick-image (auto-color (make-auto 0 400 10 2)))
                             (auto-x (make-auto 0 400 10 2))
                             (auto-y (make-auto 0 400 10 2))
                             (place-image 
                              (pick-image (auto-color (make-auto 30 450 8 1)))
                              (auto-x (make-auto 30 450 8 1))
                              (auto-y (make-auto 30 450 8 1))
                              MTS)))))






(define (billboard-text los)
  (cond [(empty? los) (text "" FONT-SIZE FONT-COLOR)]
        [(empty? (rest los))
         (line-breaker (split-string (append (string->list (first los)) (cons LINE-BREAK-CHARACTER empty))))]
        [else (billboard-text (rest los))]))

(check-expect (billboard-text (cons "hi" empty))
              (text "hi


" FONT-SIZE FONT-COLOR))
(check-expect (billboard-text (cons "" empty))
              (text "



" FONT-SIZE FONT-COLOR))
(check-expect (billboard-text (cons "alright" (cons "nah" empty)))
              (text "nah


" FONT-SIZE FONT-COLOR))
(check-expect (billboard-text (cons "alright" (cons "nah~nah" empty)))
              (above (text "nah" FONT-SIZE FONT-COLOR)
                     (text "nah


" FONT-SIZE FONT-COLOR)))



(check-expect (line-breaker (list "hi" "world"))
      (above (text "hi" FONT-SIZE FONT-COLOR)
             (text "world
" FONT-SIZE FONT-COLOR)))
(check-expect (line-breaker empty)
      (text "" FONT-SIZE FONT-COLOR))

      


(define (line-breaker los)
  (cond [(empty? los) (text "" FONT-SIZE FONT-COLOR)]
        [else (above (text (first los) FONT-SIZE FONT-COLOR)
                     (line-breaker (rest los)))]))






               
(define (split-string loc)
  (cond [(empty? loc) (cons "" empty)]
        [else (cons (substring (list->string loc) 0 (line-pos loc))
                    (split-string (string->list (substring (list->string (rest loc)) (+ 0 (line-pos loc))))))]))

(check-expect (split-string '(#\h #\i #\~)) (cons "hi" (cons "" empty)))
(check-expect (split-string '(#\h #\i #\~ #\w #\h #\a #\t #\s #\u #\p #\~)) (cons "hi" (cons "whatsup" (cons "" empty))))
(check-expect (split-string '()) (cons "" empty))
(check-expect (split-string '(#\~)) (cons "" (cons "" empty)))


(define (line-pos loc0)
  (local [(define (line-pos loc acc)
            (cond [(empty? loc) 0]
                  [else
                   (if (char=? (first loc) LINE-BREAK-CHARACTER)
                       acc
                       (line-pos (rest loc)
                                 (add1 acc)))]))]
    (line-pos loc0 0)))


(check-expect (line-pos (append '() '(#\~))) 0)
(check-expect (line-pos (append '(#\~) '(#\~))) 0)
(check-expect (line-pos (append '(#\h #\i #\~) '(#\~))) 2)

  
  






(define (BULBS ticks)
  (cond [(string=? (current-light ticks) "green")
         (above (circle LIGHT-RADIUS "outline" "red")
                (circle LIGHT-RADIUS "outline" "yellow")
                (circle LIGHT-RADIUS "solid" "green"))]
        [(string=? (current-light ticks) "yellow")
         (above (circle LIGHT-RADIUS "outline" "red")
                (circle LIGHT-RADIUS "solid" "yellow")
                (circle LIGHT-RADIUS "outline" "green"))]
        [else
         (above (circle LIGHT-RADIUS "solid" "red")
                (circle LIGHT-RADIUS "outline" "yellow")
                (circle LIGHT-RADIUS "outline" "green"))]))
                
(check-expect (BULBS (* TICKS-SECOND (floor (/ GREEN-LENGTH 2)))) 
              (above (circle LIGHT-RADIUS "outline" "red")
                     (circle LIGHT-RADIUS "outline" "yellow")
                     (circle LIGHT-RADIUS "solid" "green")))
(check-expect (BULBS (* TICKS-SECOND (floor (+ CYCLE-LENGTH (/ GREEN-LENGTH 2))))) 
              (above (circle LIGHT-RADIUS "outline" "red")
                     (circle LIGHT-RADIUS "outline" "yellow")
                     (circle LIGHT-RADIUS "solid" "green")))
(check-expect (BULBS 0) 
              (above (circle LIGHT-RADIUS "outline" "red")
                     (circle LIGHT-RADIUS "outline" "yellow")
                     (circle LIGHT-RADIUS "solid" "green")))
(check-expect (BULBS (* TICKS-SECOND GREEN-LENGTH)) 
              (above (circle LIGHT-RADIUS "outline" "red")
                     (circle LIGHT-RADIUS "solid" "yellow")
                     (circle LIGHT-RADIUS "outline" "green")))
(check-expect (BULBS (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH))) 
              (above (circle LIGHT-RADIUS "solid" "red")
                     (circle LIGHT-RADIUS "outline" "yellow")
                     (circle LIGHT-RADIUS "outline" "green")))
                





(define (timer-on tick)
  (text (number->string (ticks->seconds tick)) FONT-SIZE FONT-COLOR))

(check-expect (timer-on 0) (text (number->string 0) FONT-SIZE FONT-COLOR))
(check-expect (timer-on 281) (text (number->string 10) FONT-SIZE FONT-COLOR))







(define (auto-on loa background-image)
  (cond [(empty? loa) background-image]
        [else 
         (auto-on (rest loa) (place-image 
                              (pick-image (auto-color (first loa)))
                              (auto-x (first loa))
                              (auto-y (first loa))
                              background-image))]))

(check-expect (auto-on LA1 MTS) MTS)
(check-expect (auto-on LA2 MTS)
              (place-image 
               (pick-image (auto-color (first LA2)))
               (auto-x (first LA2))
               (auto-y (first LA2))
               (place-image 
                (pick-image (auto-color (first (rest LA2))))
                (auto-x (first (rest LA2)))
                (auto-y (first (rest LA2)))
                MTS)))




(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))
(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)
           












(define (key-detect ws ke)
  (make-ws (ws-tick ws)
           (ws-autos ws)
           (append
            (all-but-last (ws-billboards ws))
            (cond [(string=? ke "\r")
                   (cons (string-append (last (ws-billboards ws)) "~") empty)]
                  [(string=? ke "\b")
                   (cons (substring (last (ws-billboards ws)) 0 (max 0 (- (string-length (last (ws-billboards ws))) 1))) empty)]
                  [(string=? ke "\t")
                   (cons "" empty)]
                  [(string=? ke "prior") 
                   (if (empty? (all-but-last (ws-billboards ws)))
                       (cons "" empty)
                       empty)] 
                  [(string=? ke "next") 
                   (cons (last (ws-billboards ws)) (cons "" empty))]
                  [else (cons (string-append (last (ws-billboards ws)) ke) empty)]))))

(check-expect (key-detect WS4 "\r")
              (make-ws 0
                       (list (make-auto 600 500 5 0)
                             (make-auto X-POS 425 7 2))
                       (cons "" (cons "aa~" empty))))
(check-expect (key-detect WS4 "\b")
              (make-ws 0
                       (list (make-auto 600 500 5 0)
                             (make-auto X-POS 425 7 2))
                       (cons "" (cons "a" empty))))
(check-expect (key-detect WS4 "\t")
              (make-ws 0
                       (list (make-auto 600 500 5 0)
                             (make-auto X-POS 425 7 2))
                       (cons "" (cons "" empty))))
(check-expect (key-detect WS4 "prior")
              (make-ws 0
                       (list (make-auto 600 500 5 0)
                             (make-auto X-POS 425 7 2))
                       (cons "" empty)))
(check-expect (key-detect WS4 "next")
              (make-ws 0
                       (list (make-auto 600 500 5 0)
                             (make-auto X-POS 425 7 2))
                       (cons "" (cons "aa" (cons "" empty)))))
(check-expect (key-detect WS4 "b")
              (make-ws 0
                       (list (make-auto 600 500 5 0)
                             (make-auto X-POS 425 7 2))
                       (cons "" (cons "aab" empty))))
(check-expect (key-detect WS1 "prior")
              (make-ws 0 empty (cons "" empty)))






(define (last lox)
  (cond [(empty? lox) empty]
        [(empty? (rest lox)) (first lox)]
        [else (last (rest lox))]))

(check-expect (last empty) empty)
(check-expect (last (cons 3 empty)) 3)
(check-expect (last (cons 3 (cons 4 empty))) 4)






(define (all-but-last lox)
  (cond [(empty? lox) empty]
        [(empty? (rest lox)) empty]
        [else (cons (first lox) (all-but-last (rest lox)))]))

(check-expect (all-but-last empty) empty)
(check-expect (all-but-last (cons 3 empty)) empty)
(check-expect (all-but-last (cons 3 (cons 4 empty))) (cons 3 empty))
(check-expect (all-but-last (cons 3 (cons 4 (cons 5 empty)))) (cons 3 (cons 4 empty)))