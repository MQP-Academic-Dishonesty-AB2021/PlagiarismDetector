

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |cow starter FINAL Stryder & Jack|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))

[define-struct cow [x y dx rotation laps]]









          




[define START [make-cow 0 300 1 0 0]]



(define (main c)
  (big-bang c                   
    (on-tick   next-cow)        
    (to-draw   render-images)   
    (on-key    updateSpeed)     
    [on-mouse  updatePos]))     




[check-expect [next-cow [make-cow 0 300 1 0 0]] [make-cow 1 300 1 10 0]]

[check-expect [next-cow [make-cow 0 300 5 10 0]] [make-cow 5 300 5 -10 0]]

[check-expect [next-cow [make-cow 0 300 1 -10 0]] [make-cow 1 300 1 10 0]]

[check-expect [next-cow [make-cow [- WIDTH 2] 300 3 -10 0]]
              [make-cow 0 300 3 10 1]] 
                         


[define [next-cow c]
  [cond [[> [+ [cow-x c] [cow-dx c]] WIDTH]
         [make-cow 0 [cow-y c] [cow-dx c]
                   [setRotation [cow-rotation c] [cow-dx c]]
                   [+ 1 [cow-laps c]]]]
        [else
         [make-cow [+ [cow-x c] [cow-dx c]] [cow-y c] [cow-dx c]
                   [setRotation [cow-rotation c] [cow-dx c]] [cow-laps c]]]]]



[check-expect [setRotation 0 1] 10]
[check-expect [setRotation 10 1] -10]
[check-expect [setRotation -10 1] 10]
[check-expect [setRotation -10 0] 0]


(define (setRotation cowRotation cowSpeed)
  (if (= cowSpeed 0)
      
      0
      
      (cond [(= cowRotation -10) 10]
            [(= cowRotation 10) -10]
            [else 10])))
  



(define cow1 (make-cow 0 0 2 0 0))
(define cow2 (make-cow 0 0 0 0 0))

(check-expect (updateSpeed cow1 "a")(make-cow 0 0 1 0 0))
(check-expect (updateSpeed cow1 "s")(make-cow 0 0 3 0 0))
(check-expect (updateSpeed cow2 "a")(make-cow 0 0 0 0 0))
(check-expect (updateSpeed cow2 "d")cow2)


(define (updateSpeed aCow key)
  (cond [(key=? key "s") (setcowSpeed aCow (+ (cow-dx aCow) 1))]
        [(key=? key "a") (setcowSpeed aCow (- (cow-dx aCow) 1))]
        [else aCow]
        )
  )



[check-expect [setcowSpeed cow1 3] [make-cow 0 0 3 0 0]]
[check-expect [setcowSpeed cow2 5] [make-cow 0 0 5 0 0]]


(define (setcowSpeed aCow speed)
  (if (< speed 0)
      aCow
      (make-cow (cow-x aCow) (cow-y aCow) speed (cow-rotation aCow)
                (cow-laps aCow))
      )
  )




(define cow3 (make-cow 50 50 0 0 0))

(check-expect (updatePos cow3 200 300 "button-down") (make-cow 200 300 0 0 0))
(check-expect (updatePos cow3 200 300 "button-up")(make-cow 50 50 0 0 0))


(define (updatePos aCow posX posY mouseEvent)
  (cond [(mouse=? mouseEvent "button-down") (setCowPos aCow posX posY)]
        [else aCow]
        )
  )


[check-expect [setCowPos cow1 0 0] [make-cow 0 0 2 0 0]]
[check-expect [setCowPos cow1 20 30] [make-cow 20 30 2 0 0]]
[check-expect [setCowPos cow2 300 400] [make-cow 300 400 0 0 0]]


(define (setCowPos aCow posX posY)
  (make-cow posX posY (cow-dx aCow) (cow-rotation aCow) (cow-laps aCow))
  )






(define (render-images c)
  [place-images
   [list [rotate [cow-rotation c] COW]
         (text (string-append "Laps: "
                              (number->string (cow-laps c)) "\nSpeed: "
                              (number->string (cow-dx c))) 20 "black")]
   [list [make-posn [cow-x c] [cow-y c]]
         [make-posn 100 50]] MTS])