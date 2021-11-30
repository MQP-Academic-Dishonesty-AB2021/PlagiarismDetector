

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Assignment2b_TrafficLight) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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

(define FONT-SIZE 24) 
(define FONT-COLOR "black") 

(define CLOCK-POS-X (+ X-POS 100)) 

(define BASE-SPEED 10) 

(define BILLBOARD-X (/ WIDTH 3)) 
(define BILLBOARD-Y (/ HEIGHT 3)) 



(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))


(define RED-ON 
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))
(define YELLOW-ON 
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))
(define GREEN-ON 
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))







(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)










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















(define-struct auto (x y speed image))


(define AUTO1 (make-auto 0 (/ HEIGHT 2) 1 AUTO-IMAGE1))
(define AUTO2 (make-auto 0 (/ HEIGHT 2) 2 AUTO-IMAGE2))
(define AUTO3 (make-auto 0 (/ HEIGHT 2) 3 AUTO-IMAGE3))


 






 






 






(define-struct ws (tick-count autos billboards))


(define START (make-ws 0 empty (list "")))




(define (main ws)
  (big-bang ws
    (on-tick update)
    (on-draw render)
    (on-key type)))













(define (update ws)
  (make-ws (+ (ws-tick-count  ws) 1)
           (add-auto (make-ws
                      (ws-tick-count ws)
                      (remove-autos (ws-autos ws))
                      (ws-billboards ws)))
           (ws-billboards ws)))






(check-expect (remove-autos empty) empty)
(check-expect (remove-autos (list AUTO1)) (list AUTO1))
(check-expect (remove-autos (list AUTO1 (make-auto (+ WIDTH 100) 0 0 AUTO-IMAGE1))) (list AUTO1))




(define (remove-autos loa)
  (cond [(empty? loa) empty]
        [(< (auto-x (first loa)) (+ WIDTH 100))
         (cons (first loa) (remove-autos (rest loa)))]
        [else
         (remove-autos (rest loa))]))







(define (add-auto ws)
  (if (<= (random 100) 5)
      (append (list(new-auto (random 3))) (update-autos ws))
      (update-autos ws)))







(define (new-auto num)                                     
  (cond [(= num 0) (make-auto  0 (- HEIGHT (random (/ HEIGHT 3)) 20) (+ BASE-SPEED (random 6)) AUTO-IMAGE1)]
        [(= num 1) (make-auto  0 (- HEIGHT (random (/ HEIGHT 3)) 20) (+ BASE-SPEED (random 6)) AUTO-IMAGE2)]
        [(= num 2) (make-auto  0 (- HEIGHT (random (/ HEIGHT 3)) 20) (+ BASE-SPEED (random 6)) AUTO-IMAGE3)]))






(check-expect (update-autos (make-ws 0 empty (list ""))) empty)
(check-expect (update-autos (make-ws 0 (list AUTO1) (list ""))) (list (make-auto 1 (/ HEIGHT 2) 1 AUTO-IMAGE1)))
(check-expect (update-autos (make-ws 0 (list AUTO1 AUTO2) (list "")))
              (list (make-auto 1 (/ HEIGHT 2) 1 AUTO-IMAGE1) (make-auto 2 (/ HEIGHT 2) 2 AUTO-IMAGE2)))




(define (update-autos ws)
  (cond [(empty? (ws-autos ws)) empty]
        [else
         (cons (make-auto (auto-inc (first (ws-autos ws)) (find-light (ws-tick-count ws)))
                          (auto-y (first (ws-autos ws)))
                          (auto-speed (first (ws-autos ws)))
                          (auto-image (first (ws-autos ws))))
               (update-autos (make-ws (ws-tick-count ws)
                                      (rest (ws-autos ws))
                                      (ws-billboards ws))))]))







(check-expect (auto-inc AUTO1 "green") (+ 0 (* 1 1))) 
(check-expect (auto-inc (make-auto 0 HEIGHT 5 AUTO-IMAGE1) "yellow") (+ 0 (* 5 3/4))) 
(check-expect (auto-inc (make-auto 0 HEIGHT 5 AUTO-IMAGE1) "red")  (+ 0 (* 5 1/2))) 
(check-expect (auto-inc (make-auto X-POS HEIGHT 5 AUTO-IMAGE1) "yellow")  (+ X-POS (* 5 3/4))) 
(check-expect (auto-inc (make-auto X-POS HEIGHT 5 AUTO-IMAGE1) "red") X-POS) 
(check-expect (auto-inc (make-auto WIDTH HEIGHT 5 AUTO-IMAGE1) "red")  (+ WIDTH (* 5 1))) 




(define (auto-inc auto color)
  (cond [(> (auto-x auto) X-POS) (+ (auto-x auto) (auto-speed auto))]
        [(string=? color "green") (+ (auto-x auto) (auto-speed auto))]
        [(string=? color "yellow") (+ (auto-x auto) (* (auto-speed auto) (/ 3 4)))]
        [(and (string=? color "red")
              (>= (+ (auto-x auto) (* (auto-speed auto) 1/2)) X-POS))
         X-POS]
        [(string=? color "red") (+ (auto-x auto) (* (auto-speed auto) (/ 1 2)))]))
  





(check-expect (find-light 0) "green")
(check-expect (find-light (* 28 GREEN-LENGTH)) "yellow") 
(check-expect (find-light (- (* 28 GREEN-LENGTH) 28)) "green") 
(check-expect (find-light (* 28 (+ YELLOW-LENGTH GREEN-LENGTH))) "red") 
(check-expect (find-light (- (* 28 (+ YELLOW-LENGTH GREEN-LENGTH)) 28)) "yellow") 
(check-expect (find-light (* 28 (+ YELLOW-LENGTH RED-LENGTH GREEN-LENGTH))) "green") 
(check-expect (find-light (- (* 28 (+ YELLOW-LENGTH RED-LENGTH GREEN-LENGTH)) 28)) "red") 




(define (find-light tick-count)
  (local [(define SECONDS (modulo (ticks->seconds tick-count) (+ GREEN-LENGTH RED-LENGTH YELLOW-LENGTH)))]
    (cond [(< SECONDS GREEN-LENGTH) "green"]
          [(< SECONDS (+ GREEN-LENGTH YELLOW-LENGTH)) "yellow"]
          [(< SECONDS (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH)) "red"])))






(check-expect (render (make-ws 0 empty (list "")))
              (place-image (text (first (list "")) FONT-SIZE FONT-COLOR)
                           BILLBOARD-X
                           BILLBOARD-Y
                           (place-image (display-clock 0)
                                        CLOCK-POS-X
                                        Y-POS
                                        (place-image
                                         GREEN-ON
                                         X-POS
                                         Y-POS
                                         (place-autos (make-ws GREEN-LENGTH
                                                               empty
                                                               (list "")))))))
(check-expect (render (make-ws (* 28 GREEN-LENGTH) empty (list "")))
              (place-image (text (first (list "")) FONT-SIZE FONT-COLOR)
                           BILLBOARD-X
                           BILLBOARD-Y
                           (place-image (display-clock (* 28 GREEN-LENGTH))
                                        CLOCK-POS-X
                                        Y-POS
                                        (place-image YELLOW-ON
                                                     X-POS
                                                     Y-POS
                                                     (place-autos (make-ws GREEN-LENGTH
                                                                           empty
                                                                           (list "")))))))




(define (render ws)
  (place-image (billboard-text (ws-billboards ws)) BILLBOARD-X BILLBOARD-Y
               (place-image (display-clock (ws-tick-count ws)) CLOCK-POS-X Y-POS
                            (place-image (choose-image (find-light (ws-tick-count ws))) X-POS Y-POS (place-autos ws)))))






(check-expect (choose-image "green") GREEN-ON)
(check-expect (choose-image "yellow") YELLOW-ON)
(check-expect (choose-image "red") RED-ON)


(define (choose-image color)
  (cond [(string=? color "green") GREEN-ON]
        [(string=? color "yellow") YELLOW-ON]
        [(string=? color "red") RED-ON]))
           




(check-expect (display-clock 0) (text (number->string (ticks->seconds 0)) FONT-SIZE FONT-COLOR))
(check-expect (display-clock 28) (text (number->string(ticks->seconds 28)) FONT-SIZE FONT-COLOR))




(define (display-clock tick-count)
  (text (number->string (ticks->seconds tick-count)) FONT-SIZE FONT-COLOR))






(check-expect (place-autos (make-ws 0 empty (list ""))) MTS)
(check-expect (place-autos (make-ws 0 (list (make-auto 0 300 2 AUTO-IMAGE1)) (list ""))) (place-image AUTO-IMAGE1 0 300 MTS))
(check-expect (place-autos (make-ws 0 (list (make-auto 0 300 2 AUTO-IMAGE1)
                                            (make-auto 30 330 3 AUTO-IMAGE2)) (list "")))
              (place-image AUTO-IMAGE1 0 300 (place-image AUTO-IMAGE2 30 330 MTS)))




(define (place-autos ws)
  (cond [(empty? (ws-autos ws)) MTS]
        [else
         (place-image
          (auto-image (first (ws-autos ws)))
          (auto-x (first (ws-autos ws)))
          (auto-y (first (ws-autos ws)))
          (place-autos (make-ws (ws-tick-count ws)
                                (rest (ws-autos ws)) (ws-billboards ws))))]))






(check-expect (type (make-ws 0 empty (list "")) "a") (make-ws 0 empty (list "a"))) 
(check-expect (type (make-ws 0 empty (list "a")) "b") (make-ws 0 empty (list "ab"))) 
(check-expect (type (make-ws 0 empty (list "a")) "shift") (make-ws 0 empty (list "a"))) 
(check-expect (type (make-ws 0 empty (list "a")) "rshift") (make-ws 0 empty (list "a"))) 
(check-expect (type (make-ws 0 empty (list "a")) "\r") (make-ws 0 empty (list "a\n"))) 
(check-expect (type (make-ws 0 empty (list "ab")) "\b") (make-ws 0 empty (list "a"))) 
(check-expect (type (make-ws 0 empty (list "")) "\b") (make-ws 0 empty (list ""))) 
(check-expect (type (make-ws 0 empty (list "hello world")) "\t") (make-ws 0 empty (list ""))) 
(check-expect (type (make-ws 0 empty (list "hello" "world")) "\t") (make-ws 0 empty (list "" "world"))) 

(check-expect (type (make-ws 0 empty (list "hello")) "down") (make-ws 0 empty (list "" "hello"))) 
(check-expect (type (make-ws 0 empty (list "hello" "world")) "up") (make-ws 0 empty (list "world"))) 



(define (type ws key)
  (make-ws (ws-tick-count ws) (ws-autos ws) (new-billboard (ws-billboards ws) key)))





(check-expect (new-billboard (list "") "a") (list (string-append "" "a"))) 
(check-expect (new-billboard (list "a") "b") (list (string-append "a" "b"))) 
(check-expect (new-billboard (list "a") "shift") (list "a")) 
(check-expect (new-billboard (list "a") "rshift") (list "a")) 
(check-expect (new-billboard (list "a") "\r") (list (string-append "a" "\n"))) 
(check-expect (new-billboard (list "ab") "\b") (list (substring "ab" 0 1))) 
(check-expect (new-billboard (list "") "\b") (list (substring "" 0))) 
(check-expect (new-billboard (list "hello world") "\t") (list "")) 

(check-expect (new-billboard (cons "hello" empty) "down") (cons "" (cons "hello" empty))) 
(check-expect (new-billboard (cons "hello" (cons "world" empty)) "up") (rest (cons "hello" (cons "world" empty)))) 



(define (new-billboard lob key)
  (cond [(string=? key "\t") (cons "" (rest lob))]
        [(string=? key "\r") (cons (string-append (first lob) "\n") (rest lob))]
        [(string=? key "\b") (if (<= (string-length (first lob)) 0)
                                 lob
                                 (cons (substring (first lob) 0 (- (string-length (first lob)) 1))(rest lob)))]
        [(string=? key "shift") lob]
        [(string=? key "rshift") lob]
        [(string=? key "down") (cons "" lob)]
        [(string=? key "up") (if(empty? (rest lob))
                                lob
                                (rest lob))]
        [else (cons (string-append (first lob) key) (rest lob))]))






(check-expect (billboard-text (list "")) (text "" FONT-SIZE FONT-COLOR))
(check-expect (billboard-text (list "hello")) (text "hello" FONT-SIZE FONT-COLOR))
(check-expect (billboard-text (list "hello" "world")) (text "hello" FONT-SIZE FONT-COLOR))





(define (billboard-text lob)
  (text (first lob) FONT-SIZE FONT-COLOR))




