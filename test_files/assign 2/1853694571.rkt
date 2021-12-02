

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Jason_Angie_Amazing_Cow) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)




(define HEIGHT 600)
(define WIDTH 800)
(define COW-IMG .)
(define CTR-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))
(define SpeedStart 1)





(define-struct COW (x dx y laps frame))
(define START (make-COW (/ WIDTH 2) SpeedStart (/ HEIGHT 2) 0 0))

(define cow1 (make-COW 117 3 300 1 0))
(define cow2 (make-COW 63 63 300 6 1))







(define (main COW)
  (big-bang COW                   
    (on-tick   move-cow)     
    (to-draw   render-cow)   
    (on-mouse  teleport-cow)      
    (on-key    change-speed) ))    






 

(check-expect (move-cow cow1) (make-COW 120 3 300 1 1))
(check-expect (move-cow cow2) (make-COW 126 63 300 6 2))


(define (move-cow ws)
  (if (< (COW-x ws) WIDTH)
      (make-COW (+ (COW-dx ws) (COW-x ws)) (COW-dx ws) (COW-y ws) (COW-laps ws) (advance-frame ws))
      (make-COW 0 (COW-dx ws) (COW-y ws) (add1 (COW-laps ws)) (advance-frame ws))))






(check-expect (advance-frame cow1) 1)
(check-expect (advance-frame cow2) 2)

(define (advance-frame ws)
  (cond [(zero? (COW-dx ws)) 0]
        [(= (COW-frame ws) 3) 0]
        [else (+ (COW-frame ws) 1)]))



 

(define (render-cow ws)
  (overlay/align "left" "top" (get-text ws)
                 (place-image (rotate (get-degree ws) COW-IMG) (COW-x ws) (COW-y ws) MTS)))




(check-expect (get-text START) (above/align "left" (text (string-append "Distance: " "0" " laps") 24 "red")
                                            (text (string-append "Speed: " "1" " pixels/tick") 24 "red")))

(check-expect (get-text cow1) (above/align "left" (text (string-append "Distance: " "1" " laps") 24 "red")
                                            (text (string-append "Speed: " "3" " pixels/tick") 24 "red")))

              
(define (get-text ws)
  (above/align "left" (text (string-append "Distance: " (number->string (COW-laps ws)) " laps") 24 "red")
               (text (string-append "Speed: " (number->string (COW-dx ws)) " pixels/tick") 24 "red")))



(check-expect (get-degree cow1) 0)
(check-expect (get-degree cow2) 5)


(define (get-degree ws)
  (cond [(= 0 (COW-frame ws)) 0]
        [(= 1 (COW-frame ws)) 5]
        [(= 2 (COW-frame ws)) 0]
        [(= 3 (COW-frame ws)) -5]))





(check-expect (teleport-cow cow1 300 20 "button-down") (make-COW 300 3 20 1 0))
(check-expect (teleport-cow cow2 256 172 "button-down") (make-COW 256 63 172 6 1))

(define (teleport-cow ws mx my click)
  (cond [(mouse=? click "button-down")
         (make-COW mx
                   (COW-dx ws)
                   my
                   (COW-laps ws)
                   (COW-frame ws))]
        [else ws]))






  
 

(check-expect (change-speed cow1 "a") (make-COW 117 2 300 1 0))
(check-expect (change-speed cow1 "s") (make-COW 117 4 300 1 0))

(check-expect (change-speed cow2 "a") (make-COW 63 62 300 6 1))
(check-expect (change-speed cow2 "s") (make-COW 63 64 300 6 1))

              
(define (change-speed cow key)
  (cond [(key=? "s" key)
         (make-COW (COW-x cow)
                   (add1 (COW-dx cow))
                   (COW-y cow)
                   (COW-laps cow)
                   (COW-frame cow))]
        [(and (key=? "a" key) (not (= (COW-dx cow) 0)))
         (make-COW (COW-x cow)
                   (sub1 (COW-dx cow))
                   (COW-y cow)
                   (COW-laps cow)
                   (COW-frame cow))]
        [else cow]))