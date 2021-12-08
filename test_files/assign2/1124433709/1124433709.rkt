

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |cow starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))








(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))
(define SHAKE_AMOUNT 5)
(define TEXT_SIZE 25)
(define TEXT_COLOR "RED")











(define-struct cow-state (x y speed laps shake))

(define (fn-for-cow-state cow-state)
  (... (cow-state-x cow-state)
       (cow-state-y cow-state)
       (cow-state-speed cow-state)
       (cow-state-laps cow-state)
       (cow-state-shake cow-state)))

(define START (make-cow-state (/ WIDTH 2) (/ HEIGHT 2) 1 0 0))







(check-expect (move-cow
               (make-cow-state 300 300 1 0 0))
              (make-cow-state 301 300 1 0 (calc-shake 300 1)))

(check-expect (move-cow
               (make-cow-state
                (+ WIDTH (quotient (image-width COW) 2)) 300 3 0 0))
              (make-cow-state
               (- 0 (quotient (image-width COW) 2)) 300 3 1
               (calc-shake (+ WIDTH (quotient (image-width COW) 2)) 3)))

(check-expect (move-cow
               (make-cow-state
                (+ (+ WIDTH (quotient (image-width COW) 2)) 10) 300 10 0 0))
              (make-cow-state
               (- 0 (quotient (image-width COW) 2)) 300 10 1
               (calc-shake (+ (+ WIDTH (quotient (image-width COW) 2)) 10) 10)))



 

(define (move-cow cow-state)
  (if (>= (cow-state-x cow-state) (+ WIDTH (quotient (image-width COW) 2)))
      (make-cow-state
       (- 0 (quotient (image-width COW) 2))
       (cow-state-y cow-state)
       (cow-state-speed cow-state)
       (+ (cow-state-laps cow-state) 1)
      (calc-shake (cow-state-x cow-state) (cow-state-speed cow-state)))
      (make-cow-state
       (+ (cow-state-x cow-state) (cow-state-speed cow-state))
       (cow-state-y cow-state)
       (cow-state-speed cow-state)
       (cow-state-laps cow-state)
       (calc-shake (cow-state-x cow-state) (cow-state-speed cow-state)))))












(check-expect (calc-shake 301 2)
              (- (* (modulo (quotient 301 2) 2)
                    (* SHAKE_AMOUNT 2)) SHAKE_AMOUNT)) 
(check-expect (calc-shake 301 2) (* -1 SHAKE_AMOUNT))
              
(check-expect (calc-shake 303 2)
              (- (* (modulo (quotient 303 2) 2)
                    (* SHAKE_AMOUNT 2)) SHAKE_AMOUNT)) 
(check-expect (calc-shake 303 2) (* 1 SHAKE_AMOUNT))


(check-expect (calc-shake 300 6)
              (- (* (modulo (quotient 300 6) 2)
                    (* SHAKE_AMOUNT 2)) SHAKE_AMOUNT)) 
(check-expect (calc-shake 300 6) (* -1 SHAKE_AMOUNT))

(check-expect (calc-shake 306 6)
              (- (* (modulo (quotient 306 6) 2)
                    (* SHAKE_AMOUNT 2)) SHAKE_AMOUNT)) 
(check-expect (calc-shake 306 6) (* 1 SHAKE_AMOUNT))


(check-expect (calc-shake 308 0) 0)

 

(define (calc-shake pos speed)
  (if (zero? speed)
      0
      (- (* (modulo (quotient pos speed) 2) (* SHAKE_AMOUNT 2)) SHAKE_AMOUNT)))






(check-expect (draw-cow-info 30 5)
              (above
               (text "The speed is 30" TEXT_SIZE TEXT_COLOR)
               (text "The number of laps is 5" TEXT_SIZE TEXT_COLOR)))

(check-expect (draw-cow-info 0 0)
              (above
               (text "The speed is 0" TEXT_SIZE TEXT_COLOR)
               (text "The number of laps is 0" TEXT_SIZE TEXT_COLOR)))

 

(define (draw-cow-info speed laps)
  (above
   (text
    (string-append "The speed is "
                   (number->string speed)) TEXT_SIZE TEXT_COLOR)
   (text
    (string-append "The number of laps is "
                   (number->string laps)) TEXT_SIZE TEXT_COLOR)))


(define TEST_COW_STATE (make-cow-state 300 500 1 0 (calc-shake 300 1)))






(check-expect (render-cow TEST_COW_STATE)
              (place-image (rotate (calc-shake 300 1) COW) 300 500
               (place-image (draw-cow-info 1 0) (/ WIDTH 5) (/ HEIGHT 8) MTS)))

 

(define (render-cow cow-state)  
  (place-image (rotate (cow-state-shake cow-state) COW)
               (cow-state-x cow-state)
               (cow-state-y cow-state)
               (place-image
                (draw-cow-info
                 (cow-state-speed cow-state)
                 (cow-state-laps cow-state)) (/ WIDTH 5) (/ HEIGHT 8) MTS)))






(check-expect (handle-key TEST_COW_STATE "s")
              (make-cow-state 300 500 2 0 (calc-shake 300 1)))
(check-expect (handle-key TEST_COW_STATE "a")
              (make-cow-state 300 500 0 0 (calc-shake 300 1)))
(check-expect (handle-key (make-cow-state 300 500 0 0 (calc-shake 300 0)) "a")
              (make-cow-state 300 500 0 0 (calc-shake 300 0)))



(define (handle-key cow-state ke)
  (cond [(key=? ke "s") 
         (make-cow-state
          (cow-state-x cow-state)
          (cow-state-y cow-state)
          (+ 1 (cow-state-speed cow-state))
          (cow-state-laps cow-state)
          (cow-state-shake cow-state))]
        [(key=? ke "a")
         (if (> (cow-state-speed cow-state) 0)
             (make-cow-state
              (cow-state-x cow-state)
              (cow-state-y cow-state)
              (- (cow-state-speed cow-state) 1)
              (cow-state-laps cow-state)
              (cow-state-shake cow-state))
             (make-cow-state
              (cow-state-x cow-state)
              (cow-state-y cow-state)
              0
              (cow-state-laps cow-state)
              (cow-state-shake cow-state)))]
        [else cow-state]))






(check-expect (handle-mouse TEST_COW_STATE 600 834 "button-down")
              (make-cow-state 600 834 1 0 (calc-shake 300 1)))
(check-expect (handle-mouse TEST_COW_STATE 0 0 "button-down")
              (make-cow-state 0 0 1 0 (calc-shake 300 1)))



(define (handle-mouse cow1 x y mE)
  (if (mouse=? mE "button-down")
      (make-cow-state
       x
       y
       (cow-state-speed cow1)
       (cow-state-laps cow1)
       (cow-state-shake cow1))
      cow1))







(define (main c)
  (big-bang c
            (on-tick move-cow)       
            (to-draw render-cow)    
            (on-key  handle-key)
            (on-mouse handle-mouse)))   


