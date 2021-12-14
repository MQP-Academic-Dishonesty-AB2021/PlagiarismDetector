

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Traffic_Signal_Jandus_v3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)











(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT "white"))

(define TICKS-SECOND 28) 






(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 


(define GREEN->YELLOW GREEN-LENGTH) 
(define YELLOW->RED (+ GREEN-LENGTH YELLOW-LENGTH))
(define RED->GREEN (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))


(define TOTAL-LIGHT-LENGTH-TICKS (* (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH) TICKS-SECOND))


(define TEXT-SIZE 24)
(define TEXT-COLOR "indigo")
(define TEXT-X (+ X-POS 150))
(define TEXT-Y Y-POS)


(define Y-POSCARS (+ Y-POS (* 3 LIGHT-RADIUS)))
(define MAX-V 20) 
(define SPAWN-CHANCE 5) 


(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))

(define AUTO-IMAGE0 .)
(define AUTO-IMAGE1 .)
(define AUTO-IMAGE2 .)



(define-struct auto (x y speed orig-speed image))








   
    

(define A0 (make-auto 50 (+ Y-POSCARS 150) 2 2 AUTO-IMAGE0))
(define A1 (make-auto 10 (+ Y-POSCARS 20) 1 1 AUTO-IMAGE1))
(define A2 (make-auto 300 (+ Y-POSCARS 60) 3 3 AUTO-IMAGE2))








 


(define LOA (list A0))
(define LOA2 (list A0 A1 A2))





(define-struct intersection (ticks LOA))





   
    

(define START (make-intersection 0 empty))
(define I1 (make-intersection 10 LOA2))









 






(define (main cw)
  (big-bang cw
    (on-tick next)
    (to-draw render)))











(define (fn-for-intersection ints)
  (... (intersection-ticks ints)               
       (fn-for-LOA (intersection-LOA ints))))  

(define (next ints) 
  (make-intersection (add1 (intersection-ticks ints))
                     (update-automobiles (append (new-ride MAX-V) (intersection-LOA ints))
                                         (intersection-ticks ints)))) 





(check-expect (render (make-intersection 10 empty))
              (place-image (make-text 10) TEXT-X TEXT-Y
                           (place-image (light-image (signal-light 10)) X-POS Y-POS (render-autos empty)))) 
(check-expect (render (make-intersection 0 LOA))
              (place-image (make-text 0) TEXT-X TEXT-Y
                           (place-image (light-image (signal-light 0))  X-POS Y-POS (render-autos LOA))))  




   

(define (render ints)
  (place-image (make-text (intersection-ticks ints)) TEXT-X TEXT-Y
               (place-image (light-image (signal-light (intersection-ticks ints))) X-POS Y-POS
                            (render-autos (intersection-LOA ints)))))





(check-expect (render-autos empty) MTS)
(check-expect (render-autos (list (make-auto 50 (+ Y-POSCARS 150) 10 10 AUTO-IMAGE0)))
              (place-image AUTO-IMAGE0 50 (+ Y-POSCARS 150) MTS))
(check-expect (render-autos (list (make-auto 50 (+ Y-POSCARS 150) 10 10 AUTO-IMAGE0)
                                  (make-auto 200 (+ Y-POSCARS 250) 10 10 AUTO-IMAGE1)))
              (place-image AUTO-IMAGE1 200 (+ Y-POSCARS 250)
                           (place-image AUTO-IMAGE0 50 (+ Y-POSCARS 150) MTS)))





 

(define (render-autos LOA)
  (cond [(empty? LOA) MTS]
        [else
         (place-image (auto-image (first LOA))
                      (auto-x (first LOA))
                      (auto-y (first LOA))
                      (render-autos (rest LOA)))])) 








(check-expect (signal-light 0)0)
(check-expect (signal-light (sub1 (* GREEN->YELLOW TICKS-SECOND))) 0)
(check-expect (signal-light (* GREEN->YELLOW TICKS-SECOND)) 1)
(check-expect (signal-light (sub1 (* YELLOW->RED TICKS-SECOND))) 1)
(check-expect (signal-light (* YELLOW->RED TICKS-SECOND)) 2)
(check-expect (signal-light (sub1 (* RED->GREEN TICKS-SECOND)))2)
(check-expect (signal-light (* RED->GREEN TICKS-SECOND)) 0)









 

(define (signal-light total-ticks)
  (local [(define ticks (modulo total-ticks TOTAL-LIGHT-LENGTH-TICKS))] 
            (cond [(< ticks (* GREEN->YELLOW TICKS-SECOND)) 0]
                  [(< ticks (* YELLOW->RED TICKS-SECOND)) 1]
                  [(< ticks (* RED->GREEN TICKS-SECOND)) 2])))
                  







(check-expect (light-image 0)
              (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "solid" "green")))

(check-expect (light-image 1)
              (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "solid" "yellow")
               (circle LIGHT-RADIUS "outline" "green")))

(check-expect (light-image 2)
              (above
               (circle LIGHT-RADIUS "solid" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "outline" "green")))





 

(define (light-image light-number)
  (cond [(= light-number 0)
         (above
          (circle LIGHT-RADIUS "outline" "red")
          (circle LIGHT-RADIUS "outline" "yellow")
          (circle LIGHT-RADIUS "solid" "green"))]
        [(= light-number 1)
         (above
          (circle LIGHT-RADIUS "outline" "red")
          (circle LIGHT-RADIUS "solid" "yellow")
          (circle LIGHT-RADIUS "outline" "green"))]
        [(= light-number 2)
         (above
          (circle LIGHT-RADIUS "solid" "red")
          (circle LIGHT-RADIUS "outline" "yellow")
          (circle LIGHT-RADIUS "outline" "green"))]
        ))






(check-expect (make-text 100)
              (text (string-append (number->string (ticks->seconds 100)) " seconds")
                    TEXT-SIZE TEXT-COLOR))

(check-expect (make-text 0)
              (text (string-append (number->string (ticks->seconds 0)) " seconds")
                    TEXT-SIZE TEXT-COLOR))




 

(define (make-text ticks)
  (text (string-append (number->string (ticks->seconds ticks)) " seconds")
        TEXT-SIZE TEXT-COLOR))









(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))

(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)











 

(define (new-ride max-speed)
  (if (< (random 100) SPAWN-CHANCE)
      (list (make-auto 0 (- HEIGHT (random Y-POSCARS) (/ (image-height AUTO-IMAGE0) 2)) 
                       0 (+ (random max-speed) 1) (pick-image (random 3))))
      empty))







(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE0]
    [(= val 1) AUTO-IMAGE1]
    [else AUTO-IMAGE2]))
(check-expect (pick-image 0) AUTO-IMAGE0)
(check-expect (pick-image 1) AUTO-IMAGE1)
(check-expect (pick-image 2) AUTO-IMAGE2)







(check-expect (update-automobiles LOA 0) (list (update-auto A0 0)))
(check-expect (update-automobiles empty 50) empty)
(check-expect (update-automobiles (list (make-auto WIDTH Y-POSCARS 10 10 AUTO-IMAGE2)) 5) empty)
(check-expect (update-automobiles (list (make-auto (add1 WIDTH) Y-POSCARS 10 10 AUTO-IMAGE2)) 5) empty)




 

(define (update-automobiles LOA ticks)
  (cond [(empty? LOA) empty]       
        [else
         (if (> (+ (auto-x (first LOA)) (auto-speed (first LOA))) WIDTH)
             (update-automobiles (rest LOA) ticks)
             (cons (update-auto (first LOA) (signal-light ticks))
                   (update-automobiles (rest LOA) ticks)))]))







(define A3 (make-auto (- X-POS 50) (+ Y-POSCARS 50) 10 10 AUTO-IMAGE1)) 
(define A4 (make-auto (+ X-POS 50)  (+ Y-POSCARS 50) 10 10 AUTO-IMAGE2)) 
(check-expect (update-auto A3 0)
              (make-auto (+ (auto-x A3) (auto-speed A3)) (auto-y A3)
                         (auto-speed A3) (auto-orig-speed A3) (auto-image A3)))
(check-expect (update-auto A4 0)
              (make-auto (+ (auto-x A4) (auto-speed A4)) (auto-y A4)
                         (auto-speed A4) (auto-orig-speed A4) (auto-image A4)))

(check-expect (update-auto A3 1)
              (make-auto (+ (auto-x A3) (auto-speed A3)) (auto-y A3)
                         (* (auto-orig-speed A3) 3/4) (auto-orig-speed A3) (auto-image A3)))
(check-expect (update-auto A4 1)
              (make-auto (+ (auto-x A4) (auto-speed A4)) (auto-y A4)
                         (auto-orig-speed A4) (auto-orig-speed A4) (auto-image A4)))


(check-expect (update-auto A3 2)
              (make-auto (+ (auto-x A3) (auto-speed A3)) (auto-y A3)
                         (* (auto-orig-speed A3) 1/2) (auto-orig-speed A3) (auto-image A3)))

(check-expect (update-auto A4 2)
              (make-auto (+ (auto-x A4) (auto-speed A4)) (auto-y A4)
                         (auto-orig-speed A4) (auto-orig-speed A4) (auto-image A4)))

(check-expect (update-auto (make-auto (- X-POS 2) (+ Y-POSCARS 50) 5 10 AUTO-IMAGE0) 2)
              (make-auto X-POS (+ Y-POSCARS 50) 0 10 AUTO-IMAGE0))

(check-expect (update-auto (make-auto X-POS (+ Y-POSCARS 50) 5 10 AUTO-IMAGE0) 2)
              (make-auto X-POS (+ Y-POSCARS 50) 0 10 AUTO-IMAGE0))




 

(define (update-auto auto signal)
  (cond [(= signal 0) (green-signal auto)]
        [(= signal 1) (yellow-signal auto)]
        [(= signal 2) (red-signal auto)]))






(check-expect (green-signal A3)
              (make-auto (+ (auto-x A3) (auto-speed A3)) (auto-y A3)
                         (auto-speed A3) (auto-orig-speed A3) (auto-image A3)))
(check-expect (green-signal A4)
              (make-auto (+ (auto-x A4) (auto-speed A4)) (auto-y A4)
                         (auto-speed A4) (auto-orig-speed A4) (auto-image A4)))




   

(define (green-signal auto)
  (make-auto (+ (auto-x auto) (auto-speed auto)) (auto-y auto)
             (auto-orig-speed auto) (auto-orig-speed auto) (auto-image auto)))






(check-expect (yellow-signal A3)
              (make-auto (+ (auto-x A3) (auto-speed A3)) (auto-y A3)
                         (* (auto-orig-speed A3) 3/4) (auto-orig-speed A3) (auto-image A3)))
(check-expect (yellow-signal A4)
              (make-auto (+ (auto-x A4) (auto-speed A4)) (auto-y A4)
                         (auto-orig-speed A4) (auto-orig-speed A4) (auto-image A4)))



   

(define (yellow-signal auto) 
  (if (< (+ (auto-x auto) (auto-speed auto)) X-POS)
      (make-auto (+ (auto-x auto) (auto-speed auto)) (auto-y auto)
                 (* (auto-orig-speed auto) 3/4) (auto-orig-speed auto) (auto-image auto))
      (make-auto (+ (auto-x auto) (auto-speed auto)) (auto-y auto)
                 (auto-orig-speed auto) (auto-orig-speed auto) (auto-image auto))))






(check-expect (red-signal A3)
              (make-auto (+ (auto-x A3) (auto-speed A3)) (auto-y A3)
                         (* (auto-orig-speed A3) 1/2) (auto-orig-speed A3) (auto-image A3)))
(check-expect (red-signal A4)
              (make-auto (+ (auto-x A4) (auto-speed A4)) (auto-y A4)
                         (auto-orig-speed A4) (auto-orig-speed A4) (auto-image A4)))

(check-expect (red-signal (make-auto (- X-POS 2) (+ Y-POSCARS 50) 5 10 AUTO-IMAGE1))
              (make-auto X-POS (+ Y-POSCARS 50) 0 10 AUTO-IMAGE1))

(check-expect (red-signal (make-auto X-POS (+ Y-POSCARS 50) 5 10 AUTO-IMAGE1))
              (make-auto X-POS (+ Y-POSCARS 50) 0 10 AUTO-IMAGE1))




   

(define (red-signal auto)
  (if (<= (auto-x auto) X-POS)
      (if (< (+ (auto-x auto) (auto-speed auto)) X-POS)
          (make-auto (+ (auto-x auto) (auto-speed auto)) (auto-y auto)
                     (* (auto-orig-speed auto) 1/2) (auto-orig-speed auto) (auto-image auto))
          (make-auto X-POS (auto-y auto) 0 (auto-orig-speed auto) (auto-image auto)))
      (make-auto (+ (auto-x auto) (auto-speed auto)) (auto-y auto)
                 (auto-orig-speed auto) (auto-orig-speed auto) (auto-image auto))))