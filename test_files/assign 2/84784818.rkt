

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname COWfinal) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/universe)
(require 2htdp/image)
(define HEIGHT 600)
(define WIDTH 800)
(define MTS (empty-scene WIDTH HEIGHT))
(define COW .) 


(define-struct cow (x y theta dx laps)) 

(check-expect(moveCow (make-cow 800 50 1 3 1)) (make-cow 0 50 1 3 2))
(check-expect(moveCow (make-cow 50 50 3 2 0)) (make-cow 52 50 3 2 0))



 




(define (moveCow cow) 
  (if (> 800 (cow-x cow))
      (make-cow (+ (cow-dx cow) (cow-x cow)) 
                (cow-y cow)
                (cow-theta cow)
                (cow-dx cow)
                (cow-laps cow))
      (reset-cow cow)))




(check-expect(reset-cow (make-cow 800 50 1 3 1)) (make-cow 0 50 1 3 2))
(check-expect(reset-cow (make-cow 800 50 1 3 0)) (make-cow 0 50 1 3 1))
(define (reset-cow cow)
  (update-cow (update-cow cow "laps" 1) "x" -800))



(define START (make-cow 800 50 3 1 0))




(define (main cow)
  (big-bang cow
    (on-tick moveCow)
    (to-draw render-all)
    (on-key handle-key)
    (on-mouse move-to-mouse)
    ))





(check-expect(update-cow (make-cow 799 50 1 3 1) "x" 1) (make-cow 800 50 1 3 1))
(check-expect(update-cow (make-cow 800 50 1 3 1) "y" 1) (make-cow 800 51 1 3 1))
(check-expect(update-cow (make-cow 800 50 1 3 1) "theta" 1) (make-cow 800 50 2 3 1))
(check-expect(update-cow (make-cow 800 50 1 3 1) "dx" 1) (make-cow 800 50 1 4 1))
(check-expect(update-cow (make-cow 800 50 1 3 1) "laps" 1) (make-cow 800 50 1 3 2))

(define (update-cow cow what new)
  (make-cow 
   (if (string=? "x" what)
       (+(cow-x cow) new)
       (cow-x cow))
   (if (string=? "y" what)
       (+(cow-y cow) new)
       (cow-y cow))
   (if (string=? "theta" what)
       (+(cow-theta cow) new)
       (cow-theta cow))
   (if (string=? "dx" what)
       (+(cow-dx cow) new)
       (cow-dx cow))
   (if (string=? "laps" what)
       (+(cow-laps cow) new)
       (cow-laps cow))))




(check-expect(move-to-mouse (make-cow 799 50 1 3 1) 10 10 "button-down") (make-cow 10 10 1 3 1))
(check-expect(move-to-mouse (make-cow 799 50 1 3 1) 10 10 "button-up") (make-cow 799 50 1 3 1))
(define (move-to-mouse cow x y m)
  (if(string=? m "button-down")
   (make-cow x y (cow-theta cow) (cow-dx cow) (cow-laps cow))
   cow))





(check-expect(handle-key (make-cow 799 50 1 3 1) "s") (make-cow 799 50 1 4 1))
(check-expect(handle-key (make-cow 799 50 1 3 1) "a") (make-cow 799 50 1 2 1))
(check-expect(handle-key (make-cow 799 50 1 3 1) "b") (make-cow 799 50 1 3 1))
(check-expect(handle-key (make-cow 799 50 1 0 1) "a") (make-cow 799 50 1 0 1))
          
(define (handle-key cow ke)
  (cond 
    [(key=? ke "s")
     (update-cow cow "dx" 1)]
    
    [(key=? ke "a")
     (if(>(cow-dx cow)0)
     (update-cow cow "dx" -1)
     (update-cow cow "dx" 0))]
    [else cow])) 





(check-expect(check-odd (make-cow 799 50 1 3 1) COW) (rotate 1 COW))
(check-expect(check-odd (make-cow 800 50 1 3 1) COW) (rotate 0 COW))
(check-expect (check-odd (make-cow 800 50 1 3 1) empty) empty)
(define (check-odd cow COW)
  (cond [(odd? (cow-x cow)) (rotate (cow-theta cow) COW)]
    [else COW]))




(check-expect (place-text "speed " 0) (text "speed 0" 24 "black"))
(check-expect (place-text "laps " 1) (text "laps 1" 24 "black"))
(define (place-text label param)
  (text (string-append label (number->string param)) 24 "black"))




(check-expect (render-all (make-cow 1 0 1 0 1))
              (place-image (check-odd (make-cow 1 0 1 0 1) COW) 1 0 
               (place-image (place-text "Speed: " 0) 400 50 
                            (place-image (place-text "Laps: " 1) 400 80 MTS))))
(check-expect (render-all (make-cow 0 0 0 0 0))
              (place-image (check-odd (make-cow 0 0 0 0 0) COW) 0 0 
               (place-image (place-text "Speed: " 0) 400 50 
                            (place-image (place-text "Laps: " 0) 400 80 MTS))))
(define (render-all cow)
  (place-image (check-odd cow COW) (cow-x cow) (cow-y cow) 
               (place-image (place-text "Speed: " (cow-dx cow)) 400 50 
                            (place-image (place-text "Laps: " (cow-laps cow)) 400 80 MTS))))
      

