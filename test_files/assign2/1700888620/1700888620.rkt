

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment 2, Kevin & Kylie|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)




(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define UCOW (rotate 2 COW))
(define DCOW (rotate -2 COW))

(define TEXT-SIZE 24)
(define TEXT-COLOR "black")

(define SPEED-X 50)
(define SPEED-Y 50)
(define DIST-X 50)
(define DIST-Y 75)

(define MTS (empty-scene WIDTH HEIGHT))




(define-struct cow (x y dx distance angle))








(define C1 (make-cow WIDTH (/ WIDTH 2) 1 0 2)) 

(define C2 (make-cow (/ HEIGHT 2) (/ WIDTH 2) 1 0 -2))

(define C3 (make-cow WIDTH 0 0 0 0))

(define START (make-cow 0 (/ HEIGHT 2) 1 0 0))
 











(define (main c)
  (big-bang c
    (on-tick update-pos)     
    (on-key  key-handler)  
    (on-mouse mouse-handler) 
    (to-draw render-image)))     





(check-expect (update-pos C2)
              (make-cow (+ (cow-dx C2) (/ HEIGHT 2)) (cow-y C2) (cow-dx C2) (cow-distance C2) (update-angle C2)))

(check-expect (update-pos C1)
              (make-cow (+ (cow-dx C1) (cow-x C1)) (cow-y C1) (cow-dx C1) (cow-distance C1) (update-angle C1)))

(check-expect (update-pos(make-cow (+ 1 WIDTH) (cow-y C1) (cow-dx C1) (cow-distance C1) (cow-angle C1)))
              (make-cow 0 (cow-y C1) (cow-dx C1) (+ 1 (cow-distance C1)) (update-angle C1)))

                           
(define (update-pos cow)
  (if (> (cow-x cow) WIDTH)
      (make-cow 0 (cow-y cow) (cow-dx cow) (+ (cow-distance cow) 1) (update-angle cow)) 
      (make-cow (+ (cow-x cow) (cow-dx cow)) (cow-y cow) (cow-dx cow) (cow-distance cow) (update-angle cow)))) 




(check-expect (render-image C1)
              (place-image
               (rotate (cow-angle C1) COW) (cow-x C1) (cow-y C1) 
               (speed-text C1)))

(check-expect (render-image C2)
              (place-image
               (rotate (cow-angle C2) COW) (cow-x C2) (cow-y C2) 
               (speed-text C2)))



(define (render-image c)  
  (place-image
   (rotate (cow-angle c) COW) (cow-x c) (cow-y c) 
   (speed-text c))) 


(check-expect (speed-text C2) (place-image/align
   (text (string-append  "Speed: " (number->string (cow-dx C2))) TEXT-SIZE TEXT-COLOR)
   SPEED-X SPEED-Y "left" "center"
   (place-image/align (text (string-append "Laps: " (number->string (cow-distance C2))) TEXT-SIZE TEXT-COLOR)
   DIST-X DIST-Y "left" "center" MTS)))

(define (speed-text c)
  (place-image/align
   (text (string-append  "Speed: " (number->string (cow-dx c))) TEXT-SIZE TEXT-COLOR)
   SPEED-X
   SPEED-Y
   "left"
   "center"
   (distance-text c)))


(check-expect (distance-text C1) (place-image/align
   (text (string-append "Laps: " (number->string (cow-distance C1))) TEXT-SIZE TEXT-COLOR)
   DIST-X DIST-Y "left" "center" MTS))


(define (distance-text c)
  (place-image/align
   (text (string-append "Laps: " (number->string (cow-distance c))) TEXT-SIZE TEXT-COLOR)
   DIST-X
   DIST-Y
   "left"
   "center"
   MTS))





(check-expect (update-angle C1) -2) 
(check-expect (update-angle C2) 2) 
(check-expect (update-angle C3) 0) 

(define (update-angle cow)
  (if (= (cow-dx cow) 0) 0
      (if (= (cow-angle cow) 2)
          -2
          2)))






(check-expect (key-handler C1 "s") (make-cow (cow-x C1) (cow-y C1) (+ 1 (cow-dx C1)) (cow-distance C1) (cow-angle C1))) 
(check-expect (key-handler C1 "a") (make-cow (cow-x C1) (cow-y C1) (- 1 (cow-dx C1)) (cow-distance C1) (cow-angle C1))) 
(check-expect (key-handler C3 "a") C3) 
(check-expect (key-handler C3 " ") C3) 

(define (key-handler cow key)
  (cond [(key=? key "s")
         (make-cow (cow-x cow) (cow-y cow) (+ (cow-dx cow) 1) (cow-distance cow) (cow-angle cow))]
        [(key=? key "a") (if (= (cow-dx cow) 0) cow
                             (make-cow (cow-x cow) (cow-y cow) (- (cow-dx cow) 1) (cow-distance cow) (cow-angle cow)))]
        [else cow]))
       




(check-expect (mouse-handler C1 100 110 "button-down") (make-cow 100 110 (cow-dx C1) (cow-distance C1) (cow-angle C1)))
(check-expect (mouse-handler C1 100 110 "drag") C1)

(define (mouse-handler cow x y mouse)
  (if (string=? mouse "button-down")
      (make-cow x y (cow-dx cow) (cow-distance cow) (cow-angle cow))
      cow))