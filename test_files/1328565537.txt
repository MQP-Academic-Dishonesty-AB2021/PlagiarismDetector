

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1102 assignment 2_warmup|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





(require 2htdp/image)
(require 2htdp/universe)





(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))

(define SPEED 4)
(define ROTATION 20)




(define-struct cow (x y speed laps rotate?))




(define START (make-cow 0 (/ HEIGHT 2) SPEED 0 false))


 


(define C1 (make-cow 100 300 2 0 false))
(define C2 (make-cow 0 300 3 1 true))
(define C3 (make-cow (+ WIDTH 1) 300 2 0 false))
(define C4 (make-cow 100 300 0 2 true))
(define C5 (make-cow 100 300 2 0 false))










(define (main c)
  (big-bang c                       
    (on-tick   advance-cow) 
    (to-draw   render)      
    (on-key    handle-key)  
    (on-mouse mouse-click)))


(check-expect (advance-cow C1) (make-cow (+ (cow-speed C1) (cow-x C1)) (cow-y C1) (cow-speed C1) (cow-laps C1) (not (cow-rotate? C1))))
(check-expect (advance-cow C3) (make-cow 0 (cow-y C3) (cow-speed C3) (+ 1 (cow-laps C3)) (not (cow-rotate? C3))))
(check-expect (advance-cow C2) (make-cow (+ (cow-speed C2) (cow-x C2)) (cow-y C2) (cow-speed C2) (cow-laps C2) (not (cow-rotate? C2))))






(define (advance-cow cow)
  (local [(define past (> (cow-x cow) WIDTH))]
    (make-cow
     (if past
         0
         (+ (cow-speed cow) (cow-x cow))) 
     (cow-y cow) 
     (cow-speed cow) 
     (if past 
         (+ (cow-laps cow) 1)
         (cow-laps cow))
     (not (cow-rotate? cow))))) 



(define (render cow)
  (place-image 
   (render-header cow)
   (/ WIDTH 8)
   (/ HEIGHT 8)
           
   (place-image 
    (render-cow cow)
    (cow-x cow)
    (cow-y cow)
    MTS)))




(define (render-header cow)
  (above/align "left" 
               (text (string-append "Total laps: " (number->string (cow-laps cow))) 20 "red") 
               (text (string-append "Speed: " (number->string (cow-speed cow))) 20 "red")))




(define (render-cow cow)
  (if (not (= 0 (cow-speed cow))) 
      (if (cow-rotate? cow) 
          (rotate ROTATION COW)
          (rotate (- ROTATION) COW))
      COW))


(check-expect (render C1)
              (place-image
               (above/align "left" 
                            (text (string-append "Total laps: " (number->string (cow-laps C1))) 20 "red") 
                            (text (string-append "Speed: " (number->string (cow-speed C1))) 20 "red"))
               (/ WIDTH 8)
               (/ HEIGHT 8)

               (place-image
                (rotate (- ROTATION) COW)
                (cow-x C1)
                (cow-y C1)
                MTS)))
               
(check-expect (render C2)
              (place-image
               (above/align "left" 
                            (text (string-append "Total laps: " (number->string (cow-laps C2))) 20 "red") 
                            (text (string-append "Speed: " (number->string (cow-speed C2))) 20 "red"))
               (/ WIDTH 8)
               (/ HEIGHT 8)

               (place-image
                (rotate ROTATION COW)
                (cow-x C2)
                (cow-y C2)
                MTS)))

(check-expect (render C4)
              (place-image
               (above/align "left" 
                            (text (string-append "Total laps: " (number->string (cow-laps C4))) 20 "red") 
                            (text (string-append "Speed: " (number->string (cow-speed C4))) 20 "red"))
               (/ WIDTH 8)
               (/ HEIGHT 8)

               (place-image
                COW
                (cow-x C4)
                (cow-y C4)
                MTS)))


(check-expect (mouse-click C1 100 100 "button-down") (make-cow 100 100 (cow-speed C1) (cow-laps C1) (cow-rotate? C1)))
(check-expect (mouse-click C2 100 100 "button-up") C2)



(define (mouse-click cow x y me)
  (if (mouse=? "button-down" me)
      (make-cow x y (cow-speed cow) (cow-laps cow) (cow-rotate? cow))
      cow))


(check-expect (handle-key C1 "a") (make-cow (cow-x C1) (cow-y C1) (- (cow-speed C1) 1) (cow-laps C1) (cow-rotate? C1)))
(check-expect (handle-key C4 "a") (make-cow (cow-x C4) (cow-y C4) (cow-speed C4) (cow-laps C4) (cow-rotate? C4)))
(check-expect (handle-key C2 "s") (make-cow (cow-x C2) (cow-y C2) (+ (cow-speed C2) 1) (cow-laps C2) (cow-rotate? C2)))
(check-expect (handle-key C5 "b") C5)




(define (handle-key cow ke)
  (cond [(key=? ke "a")
         (make-cow (cow-x cow) (cow-y cow) (decrease-speed cow) (cow-laps cow) (cow-rotate? cow))]
        [(key=? ke "s")
         (make-cow (cow-x cow) (cow-y cow) (+ (cow-speed cow) 1) (cow-laps cow) (cow-rotate? cow))]
        [else
         cow]))




(define (decrease-speed cow)
  (if (> (cow-speed cow) 0)
      (- (cow-speed cow) 1)
      (cow-speed cow)))


