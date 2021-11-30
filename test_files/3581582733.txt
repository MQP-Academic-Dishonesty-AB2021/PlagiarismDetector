

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Cow_Jandus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)


(define HEIGHT 600)
(define WIDTH 1100)
(define COW .)
(define SHAKE 15) 

(define MTS (empty-scene WIDTH HEIGHT))


(define TEXT-X 120)
(define TEXT-Y 40)
(define TEXT-SIZE 24)
(define TEXT-COLOR "indigo")



(define-struct cow (x y speed laps angle))







(define C1 (make-cow 100 100 2 0 false))
(define C2 (make-cow 200 50 0 4 false))

(define START (make-cow 0 (/ HEIGHT 2) 20 0 false))

(define (fn-for-cow cw)
  (... (cow-x cw)       
       (cow-y cw)       
       (cow-speed cw)   
       (cow-laps cw)    
       (cow-angle cw))) 





(define (main cw)
  (big-bang cw
    (on-tick next-cow)
    (to-draw render)
    (on-key key-handler)
    (on-mouse mouse-handler)))








(check-expect (next-cow C1) (make-cow (+ (cow-x C1) (cow-speed C1)) (cow-y C1) (cow-speed C1) (cow-laps C1) (not (cow-angle C1))))
(check-expect (next-cow C2) (make-cow (cow-x C2) (cow-y C2) (cow-speed C2) (cow-laps C2) (not (cow-angle C2))))
(check-expect (next-cow (make-cow (sub1 WIDTH) 50 3 0 false)) (make-cow 0 50 3 1 (not false)))


(define (next-cow cw)
  (if (cow-off-screen? cw)
      (make-cow 0 (cow-y cw) (cow-speed cw) (+ (cow-laps cw) 1) (not (cow-angle cw)))
      (make-cow (+ (cow-x cw) (cow-speed cw))      
                (cow-y cw)       
                (cow-speed cw)   
                (cow-laps cw)    
                (not (cow-angle cw)))))





(check-expect (cow-off-screen? C1) false)
(check-expect (cow-off-screen? (make-cow (- WIDTH 1) 50 3 0 false)) true)


(define (cow-off-screen? cw)
  (> (+ (cow-x cw) (cow-speed cw)) WIDTH))
      

  





(check-expect (render C1) (place-image (make-text C1) TEXT-X TEXT-Y (place-image (rotate (- SHAKE) COW) (cow-x C1) (cow-y C1) MTS)))



(define (render cw)
  (place-image (make-text cw) TEXT-X TEXT-Y
               (place-image (tilt-cow cw) (cow-x cw) (cow-y cw) MTS)))






(check-expect (make-text (make-cow 100 200 3 5 false))
              (above
               (text (string-append "Speed: " "3" " pixels/tick") TEXT-SIZE TEXT-COLOR)
               (text (string-append "Distance: " "5" " laps") TEXT-SIZE TEXT-COLOR)))


(define (make-text cw)
  (above
   (text (string-append "Speed: " (number->string (cow-speed cw)) " pixels/tick") TEXT-SIZE TEXT-COLOR)
   (text (string-append "Distance: " (number->string (cow-laps cw)) " laps") TEXT-SIZE TEXT-COLOR)))
  




(check-expect (tilt-cow (make-cow 100 200 3 0 false)) (rotate (- SHAKE) COW))
(check-expect (tilt-cow (make-cow 100 200 3 0 true)) (rotate SHAKE COW))
(check-expect (tilt-cow (make-cow 100 200 0 0 false)) COW)
(check-expect (tilt-cow (make-cow 100 200 0 0 true)) COW)


(define (tilt-cow cw)
  (cond [(= (cow-speed cw) 0) COW]
        [(boolean=? (cow-angle cw) true) (rotate SHAKE COW)]
        [(boolean=? (cow-angle cw) false) (rotate (- SHAKE) COW)]))






(check-expect (key-handler C1 "s") (make-cow (cow-x C1) (cow-y C1) (+ (cow-speed C1) 1) (cow-laps C1) (cow-angle C1)))
(check-expect (key-handler C1 "a") (make-cow (cow-x C1) (cow-y C1) (- (cow-speed C1) 1) (cow-laps C1) (cow-angle C1)))
(check-expect (key-handler C1 " ") C1)
(check-expect (key-handler (make-cow 100 100 1 4 false) "a") (make-cow 100 100 0 4 false))
(check-expect (key-handler (make-cow 100 100 0 4 false) "a") (make-cow 100 100 0 4 false))



(define (key-handler cw key)
  (cond [(key=? key "s") (make-cow (cow-x cw) (cow-y cw) (+ (cow-speed cw) 1) (cow-laps cw) (cow-angle cw))]
        [(and (key=? key "a") (> (cow-speed cw) 0))
         (make-cow (cow-x cw) (cow-y cw) (- (cow-speed cw) 1) (cow-laps cw) (cow-angle cw))]
        [else cw]))






(check-expect (mouse-handler (make-cow 100 100 1 4 false) 200 50 "button-down") (make-cow 200 50 1 4 false))
(check-expect (mouse-handler (make-cow 100 100 1 4 false) 200 50 "button-up") (make-cow 100 100 1 4 false))


(define (mouse-handler cw mouse-x mouse-y m-event)
  (cond [(mouse=? m-event "button-down")
         (make-cow mouse-x mouse-y (cow-speed cw) (cow-laps cw) (cow-angle cw))]
        [else cw]))

