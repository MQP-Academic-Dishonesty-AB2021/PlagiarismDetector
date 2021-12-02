

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment 2 Final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define COW .)


(define COW- (rotate -2 COW))
(define COW+ (rotate  2 COW))


(define SPEED 1)
(define CTR-Y (/ HEIGHT 2))


(define MTS (empty-scene WIDTH HEIGHT))



(define C1 0)           
(define C2 (/ WIDTH 2)) 
(define C3 WIDTH)       




(define-struct cow (x y dx lap))





 
  


  

(define start (make-cow 0 200 3 0))
(define (main c)
  (big-bang c                       
            (on-tick   next-cow) 
            (to-draw   render-ims)
            (on-key    handle-key)
            (on-mouse  handle-mouse)))








(check-expect (next-cow (make-cow 20 10 3 0)) (make-cow (+ 20 3) 10 3 0)) 
(check-expect (next-cow (make-cow (- WIDTH 3) 10 3 0)) (make-cow WIDTH 10 3 0)) 
(check-expect (next-cow (make-cow (- WIDTH 2) 10 3 0)) (make-cow 0 10 3 1)) 

(define (next-cow c)
  (cond [(> (+ (cow-x c) (cow-dx c)) WIDTH) (make-cow 0 (cow-y c) (cow-dx c) (+(cow-lap c) 1))]
        [else
         (make-cow (+ (cow-x c) (cow-dx c)) (cow-y c)
                   (cow-dx c) (cow-lap c))]))









(check-expect (render-ims (make-cow 2 2 2 2))
              (place-images
           (list (choose-image (make-cow 2 2 2 2))
          (text (string-append "Distance " (number->string (cow-lap (make-cow 2 2 2 2))) " Laps") 15 "red") 
          (text (string-append "Speed " (number->string (cow-dx (make-cow 2 2 2 2))) " Pixels") 15 "red"))
           (list
            (make-posn (cow-x (make-cow 2 2 2 2)) (cow-y (make-cow 2 2 2 2)))
            (make-posn 100 20)
            (make-posn 100 40))
           MTS))
(check-expect (render-ims (make-cow 0 4 5 7))
              (place-images
           (list (choose-image (make-cow 0 4 5 7))
          (text (string-append "Distance " (number->string (cow-lap (make-cow 0 4 5 7))) " Laps") 15 "red") 
          (text (string-append "Speed " (number->string (cow-dx (make-cow 0 4 5 7))) " Pixels") 15 "red"))
           (list
            (make-posn (cow-x (make-cow 0 4 5 7)) (cow-y (make-cow 0 4 5 7)))
            (make-posn 100 20)
            (make-posn 100 40))
           MTS))




(define (render-ims c)
          (place-images
           (list (choose-image c)
          (text (string-append "Distance " (number->string (cow-lap c)) " Laps") 15 "red") 
          (text (string-append "Speed " (number->string (cow-dx c)) " Pixels") 15 "red"))
           (list
            (make-posn (cow-x c) (cow-y c))
            (make-posn 100 20)
            (make-posn 100 40))
           MTS)
  )
         









(check-expect (choose-image (make-cow 2 3 2 0)) COW+)
(check-expect (choose-image (make-cow 3 3 3 1)) COW-)
(check-expect (choose-image (make-cow 2 3 0 0)) COW+)


(define (choose-image c)
      (if (odd? (cow-x c))
          COW-
          COW+))

  







(check-expect (handle-key (make-cow (/ WIDTH 2) 10 3 0) "s") (make-cow (/ WIDTH 2) 10 4 0))
(check-expect (handle-key (make-cow (/ WIDTH 2) 15 0 1) "s") (make-cow (/ WIDTH 2) 15 1 1))
(check-expect (handle-key (make-cow (/ WIDTH 2) 10 3 0) "a") (make-cow (/ WIDTH 2) 10 2 0))
(check-expect (handle-key (make-cow (/ WIDTH 2) 10 0 0) "a") (make-cow (/ WIDTH 2) 10 0 0))



(define (handle-key c ke)
  (cond [(key=? ke "s") 
         (make-cow (cow-x c) (cow-y c) (+ (cow-dx c) 1) (cow-lap c))]
        [(key=? ke "a")
         (if (> (cow-dx c) 0)
         (make-cow (cow-x c) (cow-y c) (- (cow-dx c) 1) (cow-lap c))
         (make-cow (cow-x c) (cow-y c) (cow-dx c) (cow-lap c)))]))





(check-expect (handle-mouse (make-cow 0 0 2 0) 10 20 "button-down") (make-cow 10 20 2 0))
(check-expect (handle-mouse (make-cow 400 30 2 5) 100 20 "button-down") (make-cow 100 20 2 5))
(check-expect (handle-mouse (make-cow 400 30 2 5) 100 20 "button-up") (make-cow 400 30 2 5))





(define (handle-mouse c x-cor y-cor me)
  (cond [(mouse=? me "button-down") (make-cow x-cor y-cor (cow-dx c) (cow-lap c))]
        [else (make-cow (cow-x c) (cow-y c) (cow-dx c) (cow-lap c))]))













(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT))

(define TICKS-SECOND 28) 






(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 



(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))







(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)



(define RADIUS 40) 


(define-struct wor (tic lap autolist))


 
 

(define-struct auto (x y speed img origspeed))

(define (fn-for-auto a)
  (... (auto-x a)
       (auto-y a)
       (auto-speed a)
       (auto-img a)
       (auto-origspeed a)))



(define start (make-wor 0 1 (list (make-auto 0 250 4 1 4))))

(define (main c)
  (big-bang c                       
            (on-tick   the-timer) 
            (to-draw   render-ims)))

(check-expect (delauto (list (make-auto 1 1 1 1 1) (make-auto 1 1 1 2 1)) (make-auto 1 1 1 1 1)) (list (make-auto 1 1 1 2 1)))
  (check-expect (delauto (list (make-auto 1 2 1 1 1) (make-auto 1 1 1 1 1)) (make-auto 1 1 1 1 1)) (list (make-auto 1 2 1 1 1)))
(define (delauto Loa a)
  (cond [ (equal? a (first Loa)) (rest Loa)]
        [else
         (cons (first Loa) (delauto (rest Loa) a))]))






  

(check-expect (the-timer (make-wor 0 1 (list (make-auto 1 1 4 2 4)))) (make-wor 1 1 (list (make-auto 5 1 4 2 4))))
(check-expect (the-timer (make-wor 168 1 (list (make-auto 1 1 4 2 4)))) (make-wor 169 1 (list (make-auto 4 1 3 2 4))))
(check-expect (the-timer (make-wor 224 1 (list (make-auto 309 1 4 2 4)))) (make-wor 225 1 (list (make-auto 311 1 2 2 4))))

(check-expect (the-timer (make-wor 280 1 (list (make-auto 1 1 4 2 4) (make-auto 500 270 8 1 8)))) (make-wor 281 1 (list (make-auto 3 1 2 2 4) (make-auto 508 270 8 1 8))))

(check-expect (the-timer (make-wor 223 1 (list (make-auto 309 1 4 2 4)))) (make-wor 224 1 (list (make-auto 311 1 2 2 4))))

(check-expect (the-timer (make-wor 140 1 (list (make-auto 200 1 4 2 4)))) (make-wor 141 1 (list (make-auto 203 1 3 2 4))))

(check-expect (the-timer (make-wor 140 1 (list (make-auto 200 1 4 2 4)))) (make-wor 141 1 (list (make-auto 203 1 3 2 4))))

(check-expect (the-timer (make-wor 224 1 (list (make-auto 400 1 2 2 4)))) (make-wor 225 1 (list (make-auto 400 1 0 2 4))))

(check-expect (the-timer (make-wor 308 2 (list (make-auto 400 1 0 2 4)))) (make-wor 309 2 (list (make-auto 400 1 0 2 4))))

(check-expect (the-timer (make-wor 307 1 (list (make-auto 400 1 0 2 4)))) (make-wor 308 1 (list (make-auto 400 1 0 2 4))))

(check-expect (the-timer (make-wor 504 2 (list (make-auto 399 1 4 2 8)))) (make-wor 505 2 (list (make-auto 400 1 0 2 8))))

(check-expect (the-timer (make-wor 336 2 (list (make-auto 400 1 0 2 8)))) (make-wor 337 2 (list (make-auto 408 1 8 2 8))))

(check-expect (the-timer (make-wor 644 3 (list (make-auto 400 1 0 2 8)))) (make-wor 645 3 (list (make-auto 408 1 8 2 8))))




(define (the-timer c )

(if (= (random 40) 5)
    (if (= (ticks->seconds (wor-tic c)) (+ GREEN-LENGTH (* (- (wor-lap c) 1) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))))
        (make-wor (+ 1 (wor-tic c)) (wor-lap c) (append (Normal (YELF (wor-autolist c))) (Normal (YELF (list (make-auto 0 (+ 250 (random 550)) 1 (random 3) (+ 1 (random 10))))))))
        (if (= (ticks->seconds (wor-tic c)) (+ (+ GREEN-LENGTH YELLOW-LENGTH) (* (- (wor-lap c) 1) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))))
            (make-wor (+ 1 (wor-tic c)) (wor-lap c) (append (Normal (REDF (wor-autolist c))) (Normal (REDF (list (make-auto 0 (+ 250 (random 550)) 1 (random 3) (+ 1 (random 10))))))))
            (if (and (> (wor-tic c) 0) (= (ticks->seconds (wor-tic c)) (* (wor-lap c)  (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))))
                (make-wor (+ 1 (wor-tic c)) (wor-lap c) (append (Normal (GREENF (wor-autolist c)))
                                                                (Normal (GREENF (list (make-auto 0 (+ 250 (random 550)) 1 (random 3) (+ 1 (random 10))))))))
                (make-wor (+ 1 (wor-tic c)) (wor-lap c) (append (Normal (wor-autolist c))
                                                                (Normal (list (make-auto 0 (+ 250 (random 550)) 1 (random 3) (+ 1 (random 10))))))))))
    (if (and (>= (ticks->seconds (wor-tic c)) (+ GREEN-LENGTH (* (- (wor-lap c) 1)
                                                                 (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))))
             (< (ticks->seconds (wor-tic c)) (+ (+ GREEN-LENGTH YELLOW-LENGTH) (* (- (wor-lap c) 1) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)))))
        (make-wor (+ 1 (wor-tic c)) (wor-lap c) (Normal (YELF (wor-autolist c))))
        (if (and (>= (ticks->seconds (wor-tic c)) (+ (+ GREEN-LENGTH YELLOW-LENGTH) (* (- (wor-lap c) 1)
                                                                                       (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))))
                 (< (ticks->seconds (wor-tic c)) (+ (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH) (* (- (wor-lap c) 1) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)))))
            (make-wor (+ 1 (wor-tic c)) (wor-lap c) (Normal (REDF (wor-autolist c))))
            (if (and (> (ticks->seconds (wor-tic c)) (* (- (wor-lap c) 1) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)))
                     (< (ticks->seconds (wor-tic c)) (+ GREEN-LENGTH (* (- (wor-lap c) 1) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)))))
                (make-wor (+ 1 (wor-tic c)) (wor-lap c) (Normal (GREENF (wor-autolist c))))
                (if (and (> (wor-tic c) 0) (= (ticks->seconds (wor-tic c)) (* (wor-lap c) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))))
                (make-wor (+ 1 (wor-tic c)) (+ 1 (wor-lap c)) (Normal (GREENF (wor-autolist c))))    
                (make-wor (+ 1 (wor-tic c)) (wor-lap c) (Normal (wor-autolist c)))))))))  




(check-expect (GREENF (list (make-auto 1 1 4 2 8))) (list (make-auto 1 1 8 2 8)))




(define (GREENF Loa)
  (cond [(empty? Loa) (append Loa empty)]
        [else
         (cons (make-auto (auto-x (first Loa)) (auto-y (first Loa)) (auto-origspeed (first Loa)) (auto-img (first Loa)) (auto-origspeed (first Loa))) (GREENF (rest Loa)))]))




(check-expect (REDF (list (make-auto 1 1 10 1 10) (make-auto 405 2 8 2 8) (make-auto 400 2 8 2 8))) (list (make-auto 1 1 5 1 10) (make-auto 405 2 8 2 8) (make-auto 400 2 0 2 8)))
  (check-expect (REDF (list (make-auto 1 1 10 1 10) (make-auto 405 2 8 2 8) (make-auto 400 2 7 1 7))) (list (make-auto 1 1 5 1 10) (make-auto 405 2 8 2 8) (make-auto 400 2 0 1 7)))



(define (REDF Loa)
  (cond [(empty? Loa) (append Loa empty)]
   [ (or (= (auto-x (first Loa)) 400) (and (< (auto-x (first Loa)) 400) (> (+ (auto-x (first Loa)) (auto-speed (first Loa))) 400))) 
     (cons (make-auto 400 (auto-y (first Loa)) 0 (auto-img (first Loa)) (auto-origspeed (first Loa))) (REDF (rest Loa)))]
       [ (< (auto-x (first Loa)) 400)
     (cons (make-auto (auto-x (first Loa)) (auto-y (first Loa)) (* (auto-origspeed (first Loa)) .5) (auto-img (first Loa)) (auto-origspeed (first Loa))) (REDF (rest Loa)))]
[else
 (cons (make-auto (auto-x (first Loa)) (auto-y (first Loa)) (auto-origspeed (first Loa)) (auto-img (first Loa)) (auto-origspeed (first Loa))) (REDF (rest Loa)))]))




  (check-expect (YELF (list (make-auto 1 1 10 1 10) (make-auto 405 2 8 2 8) (make-auto 400 2 8 2 8))) (list (make-auto 1 1 7.5 1 10) (make-auto 405 2 8 2 8) (make-auto 400 2 8 2 8)))




  (define (YELF Loa)
  (cond [(empty? Loa) (append Loa empty)]
     [ (< (auto-x (first Loa)) 400)
     (cons (make-auto (auto-x (first Loa)) (auto-y (first Loa)) (* (auto-origspeed (first Loa)) .75) (auto-img (first Loa)) (auto-origspeed (first Loa))) (YELF (rest Loa)))]
[else
 (cons (make-auto (auto-x (first Loa)) (auto-y (first Loa)) (auto-origspeed (first Loa)) (auto-img (first Loa)) (auto-origspeed (first Loa))) (YELF (rest Loa)))]))



  (check-expect (Normal (list (make-auto 1 1 4 1 8))) (list (make-auto 5 1 4 1 8)))

(check-expect (Normal (list (make-auto 1 1 4 1 8) (make-auto 0 275 6 1 6) (make-auto 410 440 10 2 10))) (list (make-auto 5 1 4 1 8) (make-auto 6 275 6 1 6) (make-auto 420 440 10 2 10)))




  (define (Normal Loa)
    (cond [(empty? Loa) (append Loa empty)]
          [(> (auto-x (first Loa)) 800)
           (delauto Loa (first Loa))]
          [else
           (cons (make-auto (+ (auto-x (first Loa)) (auto-speed (first Loa))) (auto-y (first Loa)) (auto-speed (first Loa)) (auto-img (first Loa))
                            (auto-origspeed (first Loa))) (Normal (rest Loa)))]))




(check-expect (render-ims (make-wor 0 1 (list (make-auto 700 101 2 3 2))))
              (place-images (append (cons 
                  (circle RADIUS "outline"   "red")
                  (cons
                  (circle RADIUS "outline" "yellow")
                  (cons
                  (circle RADIUS "solid" "green")
                  (cons
                  (text (number->string (ticks->seconds 0)) 20 "black") empty))))
                  
                  (imgLists (list (make-auto 700 101 2 3 2))))
                  
         (append (cons
           (make-posn 400 40)
           (cons
           (make-posn 400 120)
           (cons
           (make-posn 400 200)
           (cons
           (make-posn 450 20)
           empty))))
           (xycoords (list (make-auto 700 101 2 3 2))))
          MTS))


              
(check-expect (render-ims (make-wor 168 1 (list (make-auto 313 10 10 2 10))))
              (place-images (append (cons
                  (circle RADIUS "outline"   "red")
                  (cons
                  (circle RADIUS "solid" "yellow")
                  (cons
                  (circle RADIUS "outline" "green")
                  (cons
                  (text (number->string (ticks->seconds 168)) 20 "black")
                  empty))))
                  (imgLists (list (make-auto 313 10 10 2 10)))) 
                  
          (append (cons
           (make-posn 400 40)
           (cons
           (make-posn 400 120)
           (cons
           (make-posn 400 200)
           (cons
           (make-posn 450 20)
           empty))))
           (xycoords (list (make-auto 313 10 10 2 10))))
          MTS))

(check-expect (render-ims (make-wor 224 1 (list (make-auto 400 500 2 1 2))))
              (place-images (append (cons
                  (circle RADIUS "solid"   "red")
                  (cons
                  (circle RADIUS "outline" "yellow")
                  (cons
                  (circle RADIUS "outline" "green")
                  (cons
                  (text (number->string (ticks->seconds 224)) 20 "black")
                  empty))))
                  (imgLists (list (make-auto 400 500 2 1 2))))
                  
          (append (cons
           (make-posn 400 40)
           (cons
           (make-posn 400 120)
           (cons
           (make-posn 400 200)
           (cons
           (make-posn 450 20)
           empty))))
           (xycoords (list (make-auto 400 500 2 1 2))))
          MTS))

(check-expect (render-ims (make-wor 616 3 (list (make-auto 300 300 9 2 9))))
              (place-images (append (cons
                  (circle RADIUS "outline"   "red")
                  (cons
                  (circle RADIUS "outline" "yellow")
                  (cons
                  (circle RADIUS "solid" "green")
                  (cons
                  (text (number->string (ticks->seconds 616)) 20 "black")
                  empty))))
                  (imgLists (list (make-auto 300 300 9 2 9))))
                  
          (append (cons
           (make-posn 400 40)
           (cons
           (make-posn 400 120)
           (cons
           (make-posn 400 200)
           (cons
           (make-posn 450 20)
           empty))))
           (xycoords (list (make-auto 300 300 9 2 9))))
          MTS))

(check-expect (render-ims (make-wor 309 2 (list (make-auto 550 200 8 3 8))))
              (place-images (append (cons
                  (circle RADIUS "outline"   "red")
                  (cons
                  (circle RADIUS "outline" "yellow")
                  (cons
                  (circle RADIUS "solid" "green")
                  (cons
                  (text (number->string (ticks->seconds 309)) 20 "black")
                  empty))))
                  (imgLists (list (make-auto 550 200 8 3 8))))
                  
          (append (cons
           (make-posn 400 40)
           (cons
           (make-posn 400 120)
           (cons
           (make-posn 400 200)
           (cons
           (make-posn 450 20)
           empty))))
           (xycoords (list (make-auto 550 200 8 3 8))))
          MTS))

(check-expect (render-ims (make-wor 336 2 (list (make-auto 300 100 4 2 4))))      (place-images (append (cons
                  (circle RADIUS "outline"   "red")
                  (cons
                  (circle RADIUS "outline" "yellow")
                  (cons
                  (circle RADIUS "solid" "green")
                  (cons
                  (text (number->string (ticks->seconds 336)) 20 "black")
                  empty))))
                  (imgLists (list (make-auto 300 100 4 2 4))))
                  
          (append (cons
           (make-posn 400 40)
           (cons
           (make-posn 400 120)
           (cons
           (make-posn 400 200)
           (cons
           (make-posn 450 20)
           empty))))
           (xycoords (list (make-auto 300 100 4 2 4))))
          MTS))

(check-expect (render-ims (make-wor 335 2 (list (make-auto 700 700 8 1 8))))
              (place-images (append (cons
                  (circle RADIUS "outline"   "red")
                  (cons
                  (circle RADIUS "outline" "yellow")
                  (cons
                  (circle RADIUS "solid" "green")
                  (cons
                  (text (number->string (ticks->seconds 335)) 20 "black")
                  empty))))
                  (imgLists (list (make-auto 700 700 8 1 8))))
                  
          (append (cons
           (make-posn 400 40)
           (cons
           (make-posn 400 120)
           (cons
           (make-posn 400 200)
           (cons
           (make-posn 450 20)
           empty))))
           (xycoords (list (make-auto 700 700 8 1 8))))
          MTS))

(check-expect (render-ims (make-wor 307 1 (list (make-auto 205 200 7 2 7))))
              (place-images (append (cons
                  (circle RADIUS "solid"   "red")
                  (cons
                  (circle RADIUS "outline" "yellow")
                  (cons
                  (circle RADIUS "outline" "green")
                  (cons
                  (text (number->string (ticks->seconds 307)) 20 "black")
                  empty))))
                  (imgLists (list (make-auto 205 200 7 2 7))))
                  
          (append (cons
           (make-posn 400 40)
           (cons
           (make-posn 400 120)
           (cons
           (make-posn 400 200)
           (cons
           (make-posn 450 20)
           empty))))
           (xycoords (list (make-auto 205 200 7 2 7))))
          MTS))

(check-expect (render-ims (make-wor 1473 5 (list (make-auto 10 10 3 3 3) (make-auto 100 100 2 1 2))))      

              (place-images (append (cons
                  (circle RADIUS "solid"   "red")
                  (cons
                  (circle RADIUS "outline" "yellow")
                  (cons
                  (circle RADIUS "outline" "green")
                  (cons
                  (text (number->string (ticks->seconds 1473)) 20 "black")
                  empty))))
                  (imgLists (list (make-auto 10 10 3 3 3) (make-auto 100 100 2 1 2))))
                  
          (append (cons
           (make-posn 400 40)
           (cons
           (make-posn 400 120)
           (cons
           (make-posn 400 200)
           (cons
           (make-posn 450 20)
           empty))))
           (xycoords (list (make-auto 10 10 3 3 3) (make-auto 100 100 2 1 2))))
          MTS))




(define (render-ims c)
  (cond [(and (>= (ticks->seconds (wor-tic c)) (* (- (wor-lap c) 1) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)))
                                                                (< (ticks->seconds (wor-tic c)) (+ GREEN-LENGTH (* (- (wor-lap c) 1) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)))))


  (place-images (append (cons
                  (circle RADIUS "outline"   "red")
                  (cons
                  (circle RADIUS "outline" "yellow")
                  (cons
                  (circle RADIUS "solid" "green")
                  (cons
                  (text (number->string (ticks->seconds (wor-tic c))) 20 "black")
                  empty))))
                  (imgLists (wor-autolist c)))
                  
          (append (cons
           (make-posn 400 40)
           (cons
           (make-posn 400 120)
           (cons
           (make-posn 400 200)
           (cons
           (make-posn 450 20)
           empty))))
           (xycoords (wor-autolist c)))
          MTS)

         


         ]
        [(and (>= (ticks->seconds (wor-tic c)) (+ GREEN-LENGTH (* (- (wor-lap c) 1) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))))
              (< (ticks->seconds (wor-tic c)) (+ (+ GREEN-LENGTH YELLOW-LENGTH) (* (- (wor-lap c) 1) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)))))


  (place-images (append (cons
                  (circle RADIUS "outline"   "red")
                  (cons
                  (circle RADIUS "solid" "yellow")
                  (cons
                  (circle RADIUS "outline" "green")
                  (cons
                  (text (number->string (ticks->seconds (wor-tic c))) 20 "black")
                  empty))))
                  (imgLists (wor-autolist c)))
                  
          (append (cons
           (make-posn 400 40)
           (cons
           (make-posn 400 120)
           (cons
           (make-posn 400 200)
           (cons
           (make-posn 450 20)
           empty))))
           (xycoords (wor-autolist c)))
          MTS)




         

         ]
        [(and (>= (ticks->seconds (wor-tic c)) (+ (+ GREEN-LENGTH YELLOW-LENGTH) (* (- (wor-lap c) 1) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))))
              (< (ticks->seconds (wor-tic c)) (* (wor-lap c) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))))


  (place-images (append (list
                  (circle RADIUS "solid"   "red")
                  
                  (circle RADIUS "outline" "yellow") 
                  
                  (circle RADIUS "outline" "green")
                  
                  (text (number->string (ticks->seconds (wor-tic c))) 20 "black"))
                  
                  (imgLists (wor-autolist c)))
                  
          (append (cons
           (make-posn 400 40)
           (cons
           (make-posn 400 120)
           (cons
           (make-posn 400 200)
           (cons
           (make-posn 450 20)
           empty))))
           (xycoords (wor-autolist c)))
          MTS)


         


         ]
        [else
         (render-ims (make-wor (wor-tic c) (+ (wor-lap c) 1) (wor-autolist c)))
         ]
        ))



(check-expect (xycoords (list (make-auto 1 2 3 3 3) (make-auto 5 6 7 1 7))) (list (make-posn 1 2) (make-posn 5 6)))
(check-expect (xycoords (list (make-auto 10 2 3 2 3) (make-auto 89 6 7 1 7))) (list (make-posn 10 2) (make-posn 89 6)))



(define (xycoords Loa)
  (cond [
         (empty? Loa) (append Loa empty)]
        [else
         (cons (make-posn (auto-x (first Loa)) (auto-y (first Loa))) (xycoords (rest Loa)))]))


(check-expect (imgLists (list (make-auto 1 2 3 3 3) (make-auto 5 6 7 1 7))) (list AUTO-IMAGE3 AUTO-IMAGE1))
(check-expect (imgLists (list (make-auto 10 2 3 2 3) (make-auto 89 6 7 1 7))) (list AUTO-IMAGE2 AUTO-IMAGE1))
(check-expect (imgLists (list (make-auto 0 250 4 1 4))) (list AUTO-IMAGE1))
(check-expect (imgLists empty) empty)




(define (imgLists Loa)
  (cond [(empty? Loa) (append Loa empty)]
        [(= (auto-img (first Loa)) 0)
         (cons AUTO-IMAGE1 (imgLists (rest Loa)))]
        [(= (auto-img (first Loa)) 1)
         (cons AUTO-IMAGE1 (imgLists (rest Loa)))]
         [(= (auto-img (first Loa)) 2)
         (cons AUTO-IMAGE2 (imgLists (rest Loa)))]
          [(= (auto-img (first Loa)) 3)
         (cons AUTO-IMAGE3 (imgLists (rest Loa)))]
          ))
  
























(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))

(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)





(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))
(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)


