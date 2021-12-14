

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |cow starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))









(require 2htdp/image)
(require 2htdp/universe)


(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define ROTATIONFACTOR 1)

(define MTS (empty-scene WIDTH HEIGHT))








(define-struct the-cow(rotation x y speed amountLaps))

(define C1 (make-the-cow 0  0           (/ HEIGHT 2) 1 0))
(define C2 (make-the-cow 0  WIDTH       (/ HEIGHT 2) 1 0))
(define C3 (make-the-cow ROTATIONFACTOR (/ WIDTH 2) (/ HEIGHT 2) 4 1))
(define C4 (make-the-cow 0  0           (/ HEIGHT 2) 0 0))





(define finalCow (make-the-cow ROTATIONFACTOR 0 (/ HEIGHT 2) 1 0))
(define START finalCow)


 
(define (main c)
  (big-bang c
    (on-tick advance-cow)    
    (to-draw render)         
    (on-key alter-speed)     
    (on-mouse place-cow)))   








(define (advance-cow c) 
  (if (<= WIDTH (+ (the-cow-speed c) (the-cow-x c)))
      (make-the-cow (the-cow-rotation c) 0 (the-cow-y c) (the-cow-speed c) (+ 1 (the-cow-amountLaps c)))

      (if (< 0 (the-cow-speed c))
          (make-the-cow (* -1 (the-cow-rotation c)) (+(the-cow-x c) (the-cow-speed c)) (the-cow-y c) (the-cow-speed c)      (the-cow-amountLaps c))
          (make-the-cow (the-cow-rotation c) (the-cow-x c) (the-cow-y c) (the-cow-speed c) (the-cow-amountLaps c)))))






(check-expect (render C1) (overlay/align "left" "top"
                                         (above (text (string-append "Speed of cow: " (number->string (the-cow-speed C1))) 16 "black")
                                         (text (string-append "Laps completed: " (number->string (the-cow-amountLaps C1))) 16 "black"))
  (place-image (rotate (the-cow-rotation C1) COW) (the-cow-x C1) (the-cow-y C1) MTS)))
 
(define (render c)  
  (overlay/align "left" "top"
     (above (text (string-append "Speed of cow: " (number->string (the-cow-speed c))) 16 "black")
     (text (string-append "Laps completed: " (number->string (the-cow-amountLaps c))) 16 "black"))
     (place-image (rotate (the-cow-rotation c) COW)
               (the-cow-x c)
               (the-cow-y c) MTS))) 

                

 



(check-expect (alter-speed C1 "s") (make-the-cow (the-cow-rotation C1) (the-cow-x C1) (the-cow-y C1) (+ (the-cow-speed C1) 1) (the-cow-amountLaps C1)))
(check-expect (alter-speed C1 "a") (make-the-cow (the-cow-rotation C1) (the-cow-x C1) (the-cow-y C1) (- (the-cow-speed C1) 1) (the-cow-amountLaps C1)))
(check-expect (alter-speed C4 "a") (make-the-cow (the-cow-rotation C4) (the-cow-x C4) (the-cow-y C4) 0                        (the-cow-amountLaps C4)))
(check-expect (alter-speed C2 "j") C2)

(define (alter-speed c ke)
  (cond [(key=? ke "s") (make-the-cow (the-cow-rotation c) (the-cow-x c) (the-cow-y c) (+ (the-cow-speed c) 1) (the-cow-amountLaps c))]
        [(key=? ke "a")
          (if( = 0 (the-cow-speed c))
           c
           (make-the-cow (the-cow-rotation c) (the-cow-x c) (the-cow-y c) (- (the-cow-speed c) 1) (the-cow-amountLaps c))
           )]
        [else
          c])) 
                      


(check-expect (place-cow C1 10 10 "button-down") (make-the-cow (the-cow-rotation C1) 10 10 (the-cow-speed C1) (the-cow-amountLaps C1)))
(check-expect (place-cow C2 50 10 "button-down") (make-the-cow (the-cow-rotation C2) 50 10 (the-cow-speed C2) (the-cow-amountLaps C2)))
(check-expect (place-cow C2 50 10 "button-up") C2)

 

(define (place-cow c mouseX mouseY m)
  (cond [(mouse=? m "button-down") (make-the-cow (the-cow-rotation c) mouseX mouseY (the-cow-speed c) (the-cow-amountLaps c))]
        [else
         c]))




 




    