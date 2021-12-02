

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Lab 02 Part 0 CS1102 - Benjamin Antupit and Grace Phillips|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600) 
(define WIDTH 800)  
(define COW .)

(define ROTATION_STEP 1) 
(define INIT_SPEED 1) 

(define MTS (empty-scene WIDTH HEIGHT))

(define TEXT_SIZE 20)



(define-struct ws (x y rot speed laps))

(define (fn-for-ws ws)
  (...
   (ws-x ws) 
   (ws-y ws) 
   (ws-rot ws) 
   (ws-speed WS1) 
   (ws-laps WS1)))











(define WS0 (make-ws 0  0  0 0 0)) 
(define WS1 (make-ws 20 20 1 INIT_SPEED 0)) 
(define WS2 (make-ws 30 30 1 1 5)) 
(define WS3 (make-ws 50 90 -1 1 5)) 
(define WS4 (make-ws 40 60 -1 1 1024781)) 
(define WS5 (make-ws (- WIDTH 1) 90 1 1 0)) 
(define WS6 (make-ws (- WIDTH 1) 90 -1 2 0)) 

(define START (make-ws (/ WIDTH 2) (/ HEIGHT 2) 1 1 0))



(define (move-cow ws) 
  (cond 
   [(= (ws-speed ws) 0) ws] 
   [(>= (+ (ws-speed ws) (ws-x ws)) WIDTH) 
    (make-ws
     0 
     (ws-y ws) 
     (- 0 (ws-rot ws))
     (ws-speed ws)
     ( + 1 (ws-laps ws)))]
   [else (make-ws 
          (+ (ws-speed ws) (ws-x ws))
          (ws-y ws) 
          (- 0 (ws-rot ws))
          (ws-speed ws)
          (ws-laps ws))]))

(check-expect (move-cow WS0) WS0)
(check-expect (move-cow WS1)
              (make-ws (+ 20 (ws-speed WS1)) 20 (- 0 (ws-rot WS1))
                       (ws-speed WS1) (ws-laps WS1)))
(check-expect (move-cow WS2)
              (make-ws (+ 30 (ws-speed WS2)) 30 (- 0 (ws-rot WS2))
                       (ws-speed WS2) (ws-laps WS2)))
(check-expect (move-cow WS3)
              (make-ws (+ 50 (ws-speed WS3)) 90 (- 0 (ws-rot WS3))
                       (ws-speed WS3) (ws-laps WS3)))
(check-expect (move-cow WS4)
              (make-ws (+ 40 (ws-speed WS4)) 60 (- 0 (ws-rot WS4))
                       (ws-speed WS4) (ws-laps WS4)))
(check-expect (move-cow WS5)
              (make-ws 0 90 (- 0 (ws-rot WS5)) (ws-speed WS5)
                       (+ 1 (ws-laps WS5))))
(check-expect (move-cow WS6)
              (make-ws 0 90 (- 0 (ws-rot WS6)) (ws-speed WS6)
                       (+ 1 (ws-laps WS6))))



(define (text-image ws) 
  (text (string-append "Distance: " (number->string (ws-laps ws)) 
                       " laps\nSpeed: " (number->string (ws-speed ws))
                       " pixels/sec" ) TEXT_SIZE "indigo"))





(check-expect (text-image WS1)
              (text (string-append "Distance: " (number->string (ws-laps WS1))
                                   " laps\nSpeed: "
                                   (number->string (ws-speed WS1))
                                   " pixels/sec" ) TEXT_SIZE "indigo"))
(check-expect (text-image WS0)
              (text (string-append "Distance: " (number->string (ws-laps WS0))
                                   " laps\nSpeed: "
                                   (number->string (ws-speed WS0))
                                   " pixels/sec" ) TEXT_SIZE "indigo"))



(define (render ws) 
  (underlay/align/offset 
   "left" "top"	
   (put-image (rotate (* ROTATION_STEP (ws-rot ws)) COW) 
    (ws-x ws) (ws-y ws) MTS) 
   10 10
   (text-image ws)))

(check-expect (render WS0) (underlay/align/offset "left" "top"
                                (put-image COW 0 0 MTS) 10 10 (text-image WS0)))
(check-expect (render WS1) (underlay/align/offset "left" "top"
                                (put-image
                                  (rotate (* ROTATION_STEP (ws-rot WS1)) COW)
                                  (ws-x WS1) (ws-y WS1) MTS) 10 10
                                  (text-image WS1)))
(check-expect (render WS2) (underlay/align/offset "left" "top"
                                (put-image
                                  (rotate (* ROTATION_STEP (ws-rot WS2)) COW)
                                  (ws-x WS2) (ws-y WS2) MTS) 10 10
                                  (text-image WS2)))
(check-expect (render WS3) (underlay/align/offset "left" "top"
                                (put-image
                                  (rotate (* ROTATION_STEP (ws-rot WS3)) COW)
                                  (ws-x WS3) (ws-y WS3) MTS) 10 10
                                  (text-image WS3)))
(check-expect (render WS4) (underlay/align/offset "left" "top"
                                (put-image
                                  (rotate (* ROTATION_STEP (ws-rot WS4)) COW)
                                  (ws-x WS4) (ws-y WS4) MTS) 10 10
                                  (text-image WS4)))
(check-expect (render WS5) (underlay/align/offset "left" "top"
                                (put-image
                                  (rotate (* ROTATION_STEP (ws-rot WS5)) COW)
                                  (ws-x WS5) (ws-y WS5) MTS) 10 10
                                  (text-image WS5)))
(check-expect (render WS6) (underlay/align/offset "left" "top"
                                (put-image
                                  (rotate (* ROTATION_STEP (ws-rot WS6)) COW)
                                  (ws-x WS6) (ws-y WS6) MTS) 10 10
                                  (text-image WS6)))





(define (teleport ws x y mouse-event)
  (if (string=? mouse-event "button-down")
        (make-ws
         x
         (- HEIGHT y)
         (ws-rot ws)
         (ws-speed ws)
         (ws-laps ws))
        ws))

(check-expect (teleport WS1 45 -45 "button-down")
              (make-ws 45 (- HEIGHT -45) (ws-rot WS1)
                       (ws-speed WS1) (ws-laps WS1)))
(check-expect (teleport WS1 45 -45 "button-down")
              (make-ws 45 (- HEIGHT -45) (ws-rot WS1)
                       (ws-speed WS1) (ws-laps WS1)))
(check-expect (teleport WS1 45 -45 "left-up") WS1)
(check-expect (teleport WS2 90 -180 "button-down")
              (make-ws 90 (- HEIGHT -180) (ws-rot WS2)
                       (ws-speed WS2) (ws-laps WS2)))





(define (change-speed ws key-event) 
  (cond
   [(string=? key-event "s") 
    (make-ws 
          (ws-x ws)
          (ws-y ws) 
          (ws-rot ws)
          (+ (ws-speed ws) 1)
          (ws-laps ws))]
   [(and (string=? key-event "a") (> (ws-speed ws) 0))
    (make-ws 
          (ws-x ws)
          (ws-y ws) 
          (ws-rot ws)
          (- (ws-speed ws) 1)
          (ws-laps ws))]
   [else ws]))

(check-expect (change-speed WS1 "w") WS1)
(check-expect (change-speed WS1 "s") (make-ws (ws-x WS1) (ws-y WS1) (ws-rot WS1)
                                              (+ 1 INIT_SPEED) (ws-laps WS1)))
(check-expect (change-speed WS1 "a") (make-ws (ws-x WS1) (ws-y WS1) (ws-rot WS1)
                                              (- 1 INIT_SPEED) (ws-laps WS1)))
(check-expect (change-speed WS0 "a") WS0)
(check-expect (change-speed WS2 "s") (make-ws (ws-x WS2) (ws-y WS2) (ws-rot WS2)
                                              (+ 1 INIT_SPEED) (ws-laps WS2)))
              


(define (main ws)
  (big-bang ws
  (on-tick move-cow)
  (to-draw render)
  (on-key change-speed)
  (on-mouse teleport)))

