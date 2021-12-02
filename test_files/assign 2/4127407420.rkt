

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname FINALAssignment#2_0) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define COW .)
(define LCOW (rotate -1 COW)) 
(define RCOW (rotate 1 COW)) 

(define TEXT-SIZE 20)
(define TEXT-COLOR "red")

(define MTS (empty-scene WIDTH HEIGHT))



(define-struct cow (x y speed laps rotation))








(define (fn-for-cow cow)
  (... (cow-x cow)
       (cow-y cow)
       (cow-speed cow)
       (cow-laps cow)
       (cow-rotation cow)))


(define C1 (make-cow 0 0 1 0 0))
(define C2 (make-cow 0 (/ HEIGHT 2) 1 0 0))
(define C3 (make-cow 100 300 5 1 1))
(define C4 (make-cow 799 (/ HEIGHT 2) 1 0 2))
(define C5 (make-cow 0 (/ HEIGHT 2) 5 0 0))
(define C6 (make-cow 0 0 0 0 0))

(define START (make-cow 0 (/ HEIGHT 2) 1 0 0))


(define (main cow)
  (big-bang cow
    (on-tick next-cow)
    (to-draw render-cow)
    (on-key change-speed)
    (on-mouse teleport-cow)))






(check-expect (rotate-cow C1) 1)
(check-expect (rotate-cow C2) 1)
(check-expect (rotate-cow C3) 2)
(check-expect (rotate-cow C4) 1)



(define (rotate-cow cow)
  (cond [(zero? (cow-speed cow)) 0]
        [else
         (cond [(= (cow-rotation cow) 1) 2]
               [(= (cow-rotation cow) 2) 1]
               [(= (cow-rotation cow) 0) 1])]))



(check-expect (next-cow C1)
              (make-cow
               (+ (cow-x C1) (cow-speed C1))
               (cow-y C1)
               (cow-speed C1)
               (cow-laps C1)
               (rotate-cow C1)))
(check-expect (next-cow C2)
              (make-cow
               (+ (cow-x C2) (cow-speed C2))
               (cow-y C2)
               (cow-speed C2)
               (cow-laps C2)
               (rotate-cow C2)))
(check-expect (next-cow C3)
              (make-cow
               (+ (cow-x C3) (cow-speed C3))
               (cow-y C3)
               (cow-speed C3)
               (cow-laps C3)
               (rotate-cow C3)))
(check-expect (next-cow C4)
              (make-cow
               0
               (cow-y C4)
               (cow-speed C4)
               (add1 (cow-laps C4))
               (rotate-cow C4)))
(check-expect (next-cow C5)
              (make-cow
               (+ (cow-x C5) (cow-speed C5))
               (cow-y C5)
               (cow-speed C5)
               (cow-laps C5)
               (rotate-cow C5)))
               


(define (next-cow cow)
  (cond [(>= (add1 (cow-x cow)) WIDTH)
         (make-cow 0
                   (cow-y cow)
                   (cow-speed cow)
                   (add1 (cow-laps cow))
                   (rotate-cow cow))]
        [else
         (make-cow
          (+ (cow-x cow) (cow-speed cow))
          (cow-y cow)
          (cow-speed cow)
          (cow-laps cow)
          (rotate-cow cow))]))



(check-expect (render-cow C3)
              (place-image
               (text
                (string-append
                 "Speed: "
                 (number->string(cow-speed C3))
                 " pixels/tick")
                TEXT-SIZE
                TEXT-COLOR)
               95
               40
               (place-image
                (text
                 (string-append
                  "Distance: "
                  (number->string (cow-laps C3))
                  " laps")
                 TEXT-SIZE
                 TEXT-COLOR)
                80
                20
                (place-image
                 LCOW
                 (cow-x C3)
                 (cow-y C3)
                 MTS))))

(check-expect (render-cow C4)
              (place-image
               (text
                (string-append
                 "Speed: "
                 (number->string(cow-speed C4))
                 " pixels/tick")
                TEXT-SIZE
                TEXT-COLOR)
               95
               40
               (place-image
                (text
                 (string-append
                  "Distance: "
                  (number->string (cow-laps C4))
                  " laps")
                 TEXT-SIZE
                 TEXT-COLOR)
                80
                20
                (place-image
                 RCOW
                 (cow-x C4)
                 (cow-y C4)
                 MTS))))
(check-expect (render-cow C5)
              (place-image
               (text
                (string-append
                 "Speed: "
                 (number->string(cow-speed C5))
                 " pixels/tick")
                TEXT-SIZE
                TEXT-COLOR)
               95
               40
               (place-image
                (text
                 (string-append
                  "Distance: "
                  (number->string (cow-laps C5))
                  " laps")
                 TEXT-SIZE
                 TEXT-COLOR)
                80
                20
                (place-image
                 COW
                 (cow-x C5)
                 (cow-y C5)
                 MTS))))


(define (render-cow cow)
  (place-image
   (text
    (string-append "Speed: " (number->string(cow-speed cow)) " pixels/tick")
    TEXT-SIZE
    TEXT-COLOR)
   95
   40
   (place-image
    (text
     (string-append "Distance: " (number->string (cow-laps cow)) " laps")
     TEXT-SIZE
     TEXT-COLOR)
    80
    20
    (place-image
     (cond [(= (cow-rotation cow) 0) COW]
           [(= (cow-rotation cow) 1) LCOW]
           [(= (cow-rotation cow) 2) RCOW])
     (cow-x cow)
     (cow-y cow)
     MTS))))



(check-expect (change-speed C1 "a")
              (make-cow
               (cow-x C1)
               (cow-y C1)
               (sub1 (cow-speed C1))
               (cow-laps C1)
               (cow-rotation C1)))
(check-expect (change-speed C1 "s")
              (make-cow
               (cow-x C1)
               (cow-y C1)
               (add1 (cow-speed C1))
               (cow-laps C1)
               (cow-rotation C1)))
(check-expect (change-speed C2 "a")
              (make-cow
               (cow-x C2)
               (cow-y C2)
               (sub1 (cow-speed C2))
               (cow-laps C2)
               (cow-rotation C2)))
(check-expect (change-speed C6 "a")
              (make-cow
               (cow-x C6)
               (cow-y C6)
               (cow-speed C6)
               (cow-laps C6)
               (cow-rotation C6)))
(check-expect (change-speed C5 "s")
              (make-cow
               (cow-x C5)
               (cow-y C5)
               (add1 (cow-speed C5))
               (cow-laps C5)
               (cow-rotation C5)))
               


(define (change-speed cow ke)
  (cond [(key=? ke "s")
         (make-cow
          (cow-x cow)
          (cow-y cow)
          (add1 (cow-speed cow))
          (cow-laps cow)
          (cow-rotation cow))]
        [else
         (cond [(zero? (cow-speed cow))
                (make-cow
                 (cow-x cow)
                 (cow-y cow)
                 (cow-speed cow)
                 (cow-laps cow)
                 (cow-rotation cow))]
               [(key=? ke "a")
                (make-cow
                 (cow-x cow)
                 (cow-y cow)
                 (sub1 (cow-speed cow))
                 (cow-laps cow)
                 (cow-rotation cow))])]))
           


(check-expect (teleport-cow C1 200 200 "button-down")
              (make-cow
               200
               200
               (cow-speed C1)
               (cow-laps C1)
               (cow-rotation C1)))
(check-expect (teleport-cow C1 400 600 "button-down")
              (make-cow
               400
               600
               (cow-speed C1)
               (cow-laps C1)
               (cow-rotation C1)))
(check-expect (teleport-cow C1 500 400 "button-down")
              (make-cow
               500
               400
               (cow-speed C1)
               (cow-laps C1)
               (cow-rotation C1)))
(check-expect (teleport-cow C1 200 200 "drag")
              (make-cow
               (cow-x C1)
               (cow-y C1)
               (cow-speed C1)
               (cow-laps C1)
               (cow-rotation C1)))



(define (teleport-cow cow mouse-x mouse-y me)
  (if (mouse=? me "button-down")
      (make-cow
       mouse-x
       mouse-y
       (cow-speed cow)
       (cow-laps cow)
       (cow-rotation cow))
      cow))




