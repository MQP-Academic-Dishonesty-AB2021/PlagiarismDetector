

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Ellys Gorodisch, Ian Poulsen, Daiwik Pal - Assignment #2 - Warmup|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))








(require 2htdp/image)
(require 2htdp/universe)




(define HEIGHT 600)
(define WIDTH 800)

(define MTS (empty-scene WIDTH HEIGHT))
(define CTR-Y (/ HEIGHT 2))

(define COW-IMAGE .) 
(define SPEED-CHANGE 1)
(define MINIMUM_SPEED 0)





(define-struct cow (x y speed laps rot))









(define START (make-cow (/ WIDTH 8) CTR-Y 1 0 5))

(define C1 (make-cow 20 30 3 0 5))       
(define C2 (make-cow WIDTH 100 2 2 -3))  


  








(define (main c)
  (big-bang c               
    (on-tick tock)          
    (to-draw render-cow)    
    (on-mouse handle-mouse) 
    (on-key handle-key)))   






(check-expect (tock C1) (make-cow (+ 20 3) 30
                                  3 0 (* -1 5)))
(check-expect (tock C2) (make-cow 0 100
                                  2 (add1 2) (* -1 3)))



(define (tock c)	
  (cond [(> (+ (cow-x c) (cow-speed c)) WIDTH)
         (make-cow
          0                    
          (cow-y c)            
          (cow-speed c)        
          (add1 (cow-laps c))  
          (cow-rot c))]        
        
        [else
         (make-cow
          (+ (cow-x c) (cow-speed c))  
          (cow-y c)                    
          (cow-speed c)                
          (cow-laps c)                 
          (* (cow-rot c) -1))]))       





(check-expect
 (render-cow C1)
 (place-image
  (rotate 5 COW-IMAGE) 20 30

  (place-image
   (text (string-append "Distance: "
                        (number->string 0) " laps")
         24 "black")
   200 50
   
   (place-image
    (text (string-append "Speed: "
                         (number->string 3) " pixels/tick")
          24 "black")
    200 100 MTS))))




(check-expect
 (render-cow C2)
 (place-image
  (rotate -3 COW-IMAGE) WIDTH 100
  
  (place-image
   (text
    (string-append "Distance: "
                   (number->string 2) " laps")
    24 "black")
   200 50
   
   (place-image
    (text
     (string-append "Speed: "
                    (number->string 2) " pixels/tick")
     24 "black")
    200 100 MTS))))



(define (render-cow c)
  (place-image    
   (rotate (cow-rot c) COW-IMAGE) (cow-x c) (cow-y c)
   (place-image   
    (text
     (string-append "Distance: "
                    (number->string
                     (cow-laps c)) " laps")
     24 "black")
    200 50
    (place-image  
     (text
      (string-append
       "Speed: "
       (number->string
        (cow-speed c)) " pixels/tick")
      24 "black")
     200 100 MTS))))







(check-expect (handle-mouse C1 30 50 "button-down")
              (make-cow 30 50
                        3 0 5))
(check-expect (handle-mouse C2 20 300 "button-down")
              (make-cow 20 300
                        2 2 -3))



(define (handle-mouse c x y me)
  (cond [(mouse=? me "button-down")
         (make-cow x y (cow-speed c) (cow-laps c) (cow-rot c))]
        [else c]))








(check-expect (handle-key C1 "s")
              (make-cow 20 30
                        (add1 3) 0 5))
(check-expect (handle-key C2 "a")
              (make-cow WIDTH 100
                        (sub1 2) 2 -3))



(define (handle-key c ke)
  (cond [(key=? ke "s")
         (if (positive? (cow-speed c))
             (make-cow (cow-x c) (cow-y c)
                       (add1 (cow-speed c))
                       (cow-laps c) (cow-rot c))
             (make-cow (cow-x c) (cow-y c)
                       (add1 (cow-speed c))
                       (cow-laps c) 5))]
        
        
        
        
        [(key=? ke "a")
         (cond [(> (cow-speed c) 1)
                (make-cow (cow-x c) (cow-y c)
                          (sub1 (cow-speed c))
                          (cow-laps c) (cow-rot c))]
               [(= (cow-speed c) 1)
                (make-cow
                 (cow-x c) (cow-y c)
                 (sub1 (cow-speed c)) (cow-laps c) 0)]
               [else (make-cow (cow-x c) (cow-y c)
                               (cow-speed c) (cow-laps c) 0)])]
        [else c]))