

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |cow starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)

(define SPEED 1)
(define ROTATIONSTEP 3.5)

(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))








(define-struct cow (x y speed rotation laps))

(define c0 (make-cow 0 (/ HEIGHT 2) 1 (/ ROTATIONSTEP 2) 0))
(define c1 (make-cow WIDTH (/ HEIGHT 2) 3 (/ (* -1 ROTATIONSTEP) 2) 0))
(define c2 (make-cow 0 (/ HEIGHT 2) 0 (/ ROTATIONSTEP 2) 1))

 

(define START c0)


(define (main c)
  (big-bang c
    (on-tick cow-tick)     
    (on-mouse handle-mouse)  
    (on-key handle-key)      
    (to-draw render)))   






(define (cow-tick c)
  (make-cow (change-x c) (cow-y c) (cow-speed c) (change-rotation c) (change-laps c)))


(check-expect (cow-tick c0) (make-cow (cow-speed c0) (cow-y c0) (cow-speed c0) (- (cow-rotation c0) ROTATIONSTEP) (cow-laps c0)))

(check-expect (cow-tick c1) (make-cow 0 (cow-y c1) (cow-speed c1) (+ (cow-rotation c1) ROTATIONSTEP) (+(cow-laps c0) 1)))




(define (change-x c)
  (if (< (cow-x c) WIDTH)
      (+ (cow-x c) (cow-speed c))
      0))

(check-expect (change-x c0) (+ (cow-x c0) (cow-speed c0)))
(check-expect (change-x c1) 0)




(define (change-rotation c)
  (if (>(cow-speed c) 0)
      (if (positive? (cow-rotation c))
          (- (cow-rotation c) ROTATIONSTEP)
          (+ (cow-rotation c) ROTATIONSTEP))
      (cow-rotation c)))

(check-expect (change-rotation c0) (- (cow-rotation c0) ROTATIONSTEP))
(check-expect (change-rotation c1) (+ (cow-rotation c1) ROTATIONSTEP))
(check-expect (change-rotation c2) (cow-rotation c2))




(define (change-laps c)
  (if (< (cow-x c) WIDTH)
      (cow-laps c)
      (+ (cow-laps c) 1)))

(check-expect (change-laps c0) (cow-laps c0))
(check-expect (change-laps c1) (+ (cow-laps c1) 1))




(define (handle-mouse c x-cor y-cor me)
  (cond [(mouse=? me "button-down") (make-cow x-cor y-cor (cow-speed c) (cow-rotation c) (cow-laps c))]
        [else c]))

(check-expect (handle-mouse c0 10 10 "button-down") (make-cow 10 10 (cow-speed c0) (cow-rotation c0) (cow-laps c0)))
(check-expect (handle-mouse c0 10 10 "button-up") c0)




(define (handle-key c ke)
  (cond [(key=? ke "s") (make-cow (cow-x c) (cow-y c) (+ (cow-speed c) 1) (cow-rotation c) (cow-laps c))]
        [(and (key=? ke "a") (> (cow-speed c) 0)) (make-cow (cow-x c) (cow-y c) (- (cow-speed c) 1) (cow-rotation c) (cow-laps c))]
        [else c]))

(check-expect (handle-key c0 " ") c0)
(check-expect (handle-key c0 "a") (make-cow (cow-x c0) (cow-y c0) (- (cow-speed c0) 1) (cow-rotation c0) (cow-laps c0)))
(check-expect (handle-key c0 "s") (make-cow (cow-x c0) (cow-y c0) (+ (cow-speed c0) 1) (cow-rotation c0) (cow-laps c0)))
(check-expect (handle-key c2 "a") c2)





(define (render c)
  (place-images (list (rotate (cow-rotation c) COW)
                      (text (string-append "Distance: " (number->string (cow-laps c)) " laps") 48 "red")
                      (text (string-append "Speed: " (number->string (cow-speed c)) " pixels/tick") 48 "red"))
                (list (make-posn (cow-x c) (cow-y c))
                      (make-posn 170 30)
                      (make-posn 210 80))
                MTS))

(check-expect (render c0) (place-images (list (rotate (cow-rotation c0) COW)
                                              (text (string-append "Distance: " (number->string (cow-laps c0)) " laps") 48 "red")
                                              (text (string-append "Speed: " (number->string (cow-speed c0)) " pixels/tick")48 "red"))
                                        (list (make-posn (cow-x c0) (cow-y c0))
                                              (make-posn 170 30)
                                              (make-posn 210 80))
                                        MTS))
(check-expect (render c1) (place-images (list (rotate (cow-rotation c1) COW)
                                              (text (string-append "Distance: " (number->string (cow-laps c1)) " laps") 48 "red")
                                              (text (string-append "Speed: " (number->string (cow-speed c1)) " pixels/tick") 48 "red"))
                                        (list (make-posn (cow-x c1) (cow-y c1))
                                              (make-posn 170 30)
                                              (make-posn 210 80))
                                        MTS))
                                                    

      