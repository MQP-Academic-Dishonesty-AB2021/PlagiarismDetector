

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Assignment2_Cow) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)





(define HEIGHT 600)
(define WIDTH 800)
(define COW .)
(define COW-EDGE (/ (image-width COW) 2))
(define COW-START (- 0 COW-EDGE))
(define COW-END (+ WIDTH COW-EDGE))

(define MTS (empty-scene WIDTH HEIGHT))

(define ROTATE 2) 
(define TEXT-COLOR "red")
(define TEXT-SIZE 24)
(define TEXT-X 140)
(define TEXT-Y 40)
(define SPEED 1)




(define-struct cow (x y rotation speed lap))







(define COW1 (make-cow (- 0 COW-EDGE) (/ HEIGHT 2) ROTATE 1 0)) 
(define COW2 (make-cow (/ WIDTH 2) (/ HEIGHT 2) ROTATE 1 0)) 
(define COW3 (make-cow COW-END (/ HEIGHT 2) ROTATE 1 0)) 
(define COW4 (make-cow (+ COW-END 5) (/ HEIGHT 2) (* -1 ROTATE) 5 1)) 

(define START (make-cow (- 0 COW-EDGE) (/ HEIGHT 2) ROTATE 1 0))

 



(define (main cow)
  (big-bang cow
    (on-tick update-cow)
    (to-draw render)
    (on-key handle-key)
    (on-mouse move-cow)))





(check-expect (update-cow COW1) (make-cow (+ COW-START 1) (/ HEIGHT 2) (* -1 ROTATE) 1 0)) 
(check-expect (update-cow COW2) (make-cow (+ (/ WIDTH 2) 1) (/ HEIGHT 2) (* -1 ROTATE) 1 0)) 
(check-expect (update-cow COW3) (make-cow COW-START (/ HEIGHT 2) (* -1 ROTATE) 1 (+ 0 1))) 
(check-expect (update-cow (make-cow (- COW-END 1) (/ HEIGHT 2) ROTATE 1 0))
              (make-cow (+ (- COW-END 1) 1) (/ HEIGHT 2) (* -1 ROTATE) 1 0)) 


(define (update-cow cow)
  (if (lap? cow)
      (make-cow (- 0 COW-EDGE)
                (cow-y cow)
                (* -1 (cow-rotation cow))
                (cow-speed cow)
                (+ 1 (cow-lap cow)))
      (make-cow (+ (cow-x cow) (cow-speed cow))
                (cow-y cow)
                (* -1 (cow-rotation cow))
                (cow-speed cow)
                (cow-lap cow))))







(check-expect (rotation COW1) (* -1 ROTATE)) 
(check-expect (rotation COW4) ROTATE) 
(check-expect (rotation (make-cow 1 1 1 0 1)) 0) 



(define (rotation cow)
  (if (= (cow-speed cow) 0)
      0
      (* -1 (cow-rotation cow))))






(check-expect (lap? COW1) false) 
(check-expect (lap? COW3) true) 



(define (lap? cow)
  (> (+ (cow-x cow) (cow-speed cow)) COW-END))






(check-expect (render COW1) (place-image (stats COW1) TEXT-X TEXT-Y
                                         (place-image (rotate (rotation COW1) COW) (cow-x COW1) (cow-y COW1) MTS)))
(check-expect (render COW2) (place-image (stats COW1) TEXT-X TEXT-Y
                                         (place-image (rotate (rotation COW2) COW) (cow-x COW2) (cow-y COW2) MTS)))
(check-expect (render COW3) (place-image (stats COW1) TEXT-X TEXT-Y
                                         (place-image (rotate (rotation COW3) COW) (cow-x COW3) (cow-y COW3) MTS)))



(define (render cow)
  (place-image (stats cow) TEXT-X TEXT-Y
               (place-image (rotate (rotation cow) COW) (cow-x cow) (cow-y cow) MTS)))





(check-expect (stats COW1) (above/align "left" (text (string-append "Laps: " (number->string(cow-lap COW1))) TEXT-SIZE TEXT-COLOR)  
                                        (text (string-append "Speed: " (number->string(cow-speed COW1)) " pixels/tick") TEXT-SIZE TEXT-COLOR)))
(check-expect (stats COW2) (above/align "left" (text (string-append "Laps: " (number->string(cow-lap COW2))) TEXT-SIZE TEXT-COLOR)  
                                        (text (string-append "Speed: " (number->string(cow-speed COW2)) " pixels/tick") TEXT-SIZE TEXT-COLOR)))
(check-expect (stats (make-cow 0 0 ROTATE 10 4)) (above/align "left" (text (string-append "Laps: " (number->string 4)) TEXT-SIZE TEXT-COLOR)  
                                        (text (string-append "Speed: " (number->string 10) " pixels/tick") TEXT-SIZE TEXT-COLOR)))



(define (stats cow)
  (above/align "left"
               (text (string-append "Laps: " (number->string(cow-lap cow))) TEXT-SIZE TEXT-COLOR)  
               (text (string-append "Speed: " (number->string(cow-speed cow)) " pixels/tick") TEXT-SIZE TEXT-COLOR)))





(check-expect (handle-key COW1 "s") (make-cow COW-START (/ HEIGHT 2) ROTATE (+ 1 SPEED) 0)) 
(check-expect (handle-key COW1 "a") (make-cow COW-START (/ HEIGHT 2) ROTATE (- 1 SPEED) 0)) 
(check-expect (handle-key (make-cow 0 (/ HEIGHT 2) ROTATE 0 0) "a") (make-cow 0 (/ HEIGHT 2) ROTATE 0 0)) 
(check-expect (handle-key COW1 "e") COW1) 



(define (handle-key cow key)
  (cond [(string=? key "s")
         (make-cow (cow-x cow) (cow-y cow) (cow-rotation cow) (+ (cow-speed cow) SPEED) (cow-lap cow))]
        [(string=? key "a")
         (if (> (- (cow-speed cow) SPEED) 0)
             (make-cow (cow-x cow) (cow-y cow) (cow-rotation cow) (- (cow-speed cow) SPEED) (cow-lap cow))
             (make-cow (cow-x cow) (cow-y cow) (cow-rotation cow) 0 (cow-lap cow)))]
        [else cow])) 





(check-expect (move-cow COW1 (- COW-END (cow-speed COW1)) (/ HEIGHT 2) "button-down") 
              (make-cow (- COW-END (cow-speed COW1)) (/ HEIGHT 2) (cow-rotation COW1) (cow-speed COW1) (cow-lap COW1))) 
(check-expect (move-cow COW1 (/ WIDTH 2) (/ HEIGHT 2)  "button-down") (make-cow (/ WIDTH 2) (/ HEIGHT 2) (cow-rotation COW1) (cow-speed COW1) (cow-lap COW1))) 
(check-expect (move-cow COW1 WIDTH (/ HEIGHT 2) "button-down") (make-cow WIDTH (/ HEIGHT 2) (cow-rotation COW1) (cow-speed COW1) (cow-lap COW1))) 
(check-expect (move-cow COW1 WIDTH (/ HEIGHT 2) "button-up") COW1) 


(define (move-cow cow x y mouse)
  (cond [(string=? "button-down" mouse)
         (make-cow x y (cow-rotation cow) (cow-speed cow) (cow-lap cow))]
        [else cow]))