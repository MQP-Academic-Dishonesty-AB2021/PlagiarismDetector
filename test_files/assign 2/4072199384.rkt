

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |assignment 2 Cow|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)





























(define COW-IMAGE .) 

(define HEIGHT 600)
(define WIDTH 800)

(define SPEED-TEXT-FRONT "Speed: " )
(define SPEED-TEXT-BACK " pixels/tick")
(define DISTANCE-TEXT-FRONT "Distance: " )
(define DISTANCE-TEXT-BACK " laps")

(define ROTATION-AMOUNT 1)

(define MTS (empty-scene WIDTH HEIGHT))

(define FONT-SIZE 24)
(define FONT-COLOR "red")
(define FONT-OFFSET-X 50)
(define FONT-OFFSET-Y 50)
(define FONT-OFFSET-BETWEEN 30)









(define-struct cow (x y dx laps rotation))
(define C1 (make-cow (/ WIDTH 2) (/ HEIGHT 2) 1 0 0)) 
(define C2 (make-cow 0 0 0 100 180)) 
(define C3 (make-cow (+ WIDTH 1) 0 3 0 20)) 
(define C4 (make-cow 0 0 0 0 0))

  






(define (main ws)
  (big-bang ws                   
    (on-tick   tock)     
    (to-draw   render)   
    (on-mouse  clicked)    
    (on-key    key-detect)))    

(define START C1)





(check-expect (tock C1) 
              (make-cow 
               (+ (cow-dx C1) (cow-x C1))
               (cow-y C1)
               (cow-dx C1)
               (cow-laps C1)
               ROTATION-AMOUNT))
                  
(check-expect (tock C2) 
              (make-cow 
               (+ (cow-dx C2) (cow-x C2))
               (cow-y C2)
               (cow-dx C2)
               (cow-laps C2)
               (cow-rotation C2)))
                  
(check-expect (tock C3) 
              (make-cow 
               0
               (cow-y C3)
               (cow-dx C3)
               (+ 1 (cow-laps C3))
               (cow-rotation C3)))

                                 



(define (tock ws)
  (cond 
    [(> (cow-x ws) WIDTH) 
     (make-cow 0          
               (cow-y ws)          
               (cow-dx ws)        
               (+ 1 (cow-laps ws))       
               (cow-rotation ws))]     
    [(= (cow-dx ws) 0)
     (make-cow (cow-x ws)          
               (cow-y ws)          
               (cow-dx ws)        
               (cow-laps ws)       
               (cow-rotation ws))]
    [else 
     (make-cow (+ (cow-x ws) (cow-dx ws))
               (cow-y ws)
               (cow-dx ws)
               (cow-laps ws)
               (rotate-cow ws))]))



  

      



(check-expect (rotate-cow C1) ROTATION-AMOUNT)
(check-expect (rotate-cow C2) 0)
(check-expect (rotate-cow C3) 0)



(define (rotate-cow ws)
  (cond 
    [(= 0 (cow-rotation ws)) ROTATION-AMOUNT]
    [else 0]))

 
    
    


(check-expect (render C1) 
              (place-image
               (rotate (cow-rotation C1) COW-IMAGE)
               (cow-x C1)
               (cow-y C1)
               (background-text C1)))
(check-expect (render C2) 
              (place-image
               (rotate (cow-rotation C2) COW-IMAGE)
               (cow-x C2)
               (cow-y C2)
               (background-text C2)))
(check-expect (render C3) 
              (place-image
               (rotate (cow-rotation C3) COW-IMAGE)
               (cow-x C3)
               (cow-y C3)
               (background-text C3)))



(define (render ws)
  (place-image 
   (rotate (cow-rotation ws) COW-IMAGE)
   (cow-x ws)
   (cow-y ws)
   (background-text ws)))





(check-expect (background-text C1)
              (underlay/align/offset
               "left"
               "top"
               (underlay/align/offset
                "left"
                "top"
                MTS
                FONT-OFFSET-X
                (+ FONT-OFFSET-Y FONT-OFFSET-BETWEEN)
                (text
                 (string-append
                  DISTANCE-TEXT-FRONT 
                  (number->string (cow-laps C1))
                  DISTANCE-TEXT-BACK) 
                 FONT-SIZE 
                 FONT-COLOR))
               FONT-OFFSET-X
               FONT-OFFSET-Y
               (text
                (string-append
                 SPEED-TEXT-FRONT 
                 (number->string (cow-dx C1))
                 SPEED-TEXT-BACK)
                FONT-SIZE 
                FONT-COLOR)))
(check-expect (background-text C2)
              (underlay/align/offset
               "left"
               "top"
               (underlay/align/offset
                "left"
                "top"
                MTS
                FONT-OFFSET-X
                (+ FONT-OFFSET-Y FONT-OFFSET-BETWEEN)
                (text
                 (string-append
                  DISTANCE-TEXT-FRONT 
                  (number->string (cow-laps C2))
                  DISTANCE-TEXT-BACK) 
                 FONT-SIZE 
                 FONT-COLOR))
               FONT-OFFSET-X
               FONT-OFFSET-Y
               (text
                (string-append
                 SPEED-TEXT-FRONT 
                 (number->string (cow-dx C2))
                 SPEED-TEXT-BACK)
                FONT-SIZE 
                FONT-COLOR)))





(define (background-text ws) 
  (underlay/align/offset
   "left"
   "top"
   (underlay/align/offset
    "left"
    "top"
    MTS
    FONT-OFFSET-X
    (+ FONT-OFFSET-Y FONT-OFFSET-BETWEEN)
    (text
     (string-append
      DISTANCE-TEXT-FRONT 
      (number->string (cow-laps ws))
      DISTANCE-TEXT-BACK) 
     FONT-SIZE 
     FONT-COLOR))
   FONT-OFFSET-X
   FONT-OFFSET-Y
   (text
    (string-append
     SPEED-TEXT-FRONT 
     (number->string (cow-dx ws))
     SPEED-TEXT-BACK)
    FONT-SIZE 
    FONT-COLOR)))

 



(check-expect (clicked C1 20 30 "button-down")
              (make-cow
               20
               30
               (cow-dx C1)
               (cow-laps C1)
               (cow-rotation C1)))
(check-expect (clicked C2 20 30 "button-up") C2)




(define (clicked ws x y me)
  (cond [(mouse=? me "button-down") 
         (make-cow
          x
          y
          (cow-dx ws)
          (cow-laps ws)
          (cow-rotation ws))]
        [else ws]))







(check-expect (key-detect C1 "s")
              (make-cow (cow-x C1)
                   (cow-y C1)
                   (+ 1 (cow-dx C1))
                   (cow-laps C1)
                   (cow-rotation C1)))
(check-expect (key-detect C3 "a")
              (make-cow (cow-x C3)
                   (cow-y C3)
                   (max 0 (- (cow-dx C3) 1))
                   (cow-laps C3)
                   (cow-rotation C3)))
(check-expect (key-detect C4 "a")
              (make-cow (cow-x C4)
                   (cow-y C4)
                   (max 0 (- (cow-dx C4) 1))
                   (cow-laps C4)
                   (cow-rotation C4)))




 

(define (key-detect ws ke)
  (cond [(key=? ke "s") 
         (make-cow (cow-x ws)
                   (cow-y ws)
                   (+ 1 (cow-dx ws))
                   (cow-laps ws)
                   (cow-rotation ws))]
        [(key=? ke "a") 
         (make-cow (cow-x ws)
                   (cow-y ws)
                   (max 0 (- (cow-dx ws) 1))
                   (cow-laps ws)
                   (cow-rotation ws))]
        [else ws]))
