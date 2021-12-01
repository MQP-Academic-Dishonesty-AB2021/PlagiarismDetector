

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname TTT-Part2_v51) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)
(require 2htdp/universe)





(define SIZE 500) 
(define SIXTH-OF-SIZE (/ SIZE 6))
(define MTS (empty-scene SIZE SIZE))
(define RECT1 (rectangle (/ SIZE 30) (* 0.9 SIZE) "solid" "orange"))
(define RECT2 (rectangle (* 0.9 SIZE) (/ SIZE 30) "solid" "orange"))


(define BACKGROUND (place-images (list RECT1 RECT1 RECT2 RECT2)
                                 (list (make-posn (/ SIZE 3) (/ SIZE 2)) 
                                       (make-posn (* 2 (/ SIZE 3)) (/ SIZE 2))
                                       (make-posn (/ SIZE 2) (/ SIZE 3))
                                       (make-posn (/ SIZE 2) (* 2 (/ SIZE 3))))
                                 MTS))
(define X-COLOR "red")
(define O-COLOR "Blue")
(define TEXTSIZE (/ SIZE 4))
(define TEXTCOLOR "green")



(define WINNING-POSITIONS  
  (list
   (list 0 1 2) 
   (list 3 4 5) 
   (list 6 7 8) 
   (list 0 3 6)
   (list 1 4 7)
   (list 2 5 8)
   (list 0 4 8) 
   (list 2 4 6)))
                                                                    





















(define-struct piece (position type))





(define-struct board (pieces user-playing? depth))









(define START (make-board empty true 0))


(define START-FULL-DEPTH (make-board empty true 8))
(define EX:XWIN (make-board (list (make-piece 0 "X")
                                  (make-piece 4 "X")
                                  (make-piece 8 "X"))
                            true 0))
(define EX:OWIN (make-board (list (make-piece 0 "O")
                                  (make-piece 1 "O")
                                  (make-piece 2 "O"))
                            true 0))
(define EX:DRAW
  (make-board (list
               (make-piece 0 "X") (make-piece 1 "X") (make-piece 2 "O")
               (make-piece 3 "O") (make-piece 4 "O") (make-piece 5 "X")
               (make-piece 6 "X") (make-piece 7 "X") (make-piece 8 "O"))
              true 0))
(define BD1 (make-board (list (make-piece 5 "O")
                              (make-piece 0 "X"))
                        true 2))
(define BD2 (make-board (list (make-piece 4 "O")
                              (make-piece 0 "X")
                              (make-piece 2 "O"))
                        false 2))
(define FULL-DEPTH-BD (make-board (list (make-piece 8 "O"))
                                  false 8))
(define NEXT-X-WIN-BD-1 (make-board (list (make-piece 0 "O")
                                          (make-piece 6 "X")
                                          (make-piece 3 "O")
                                          (make-piece 4 "X")
                                          (make-piece 8 "O")) 
                                    false 1))
(define NEXT-X-WIN-BD-2 (make-board (board-pieces NEXT-X-WIN-BD-1)
                                    false 2))
(define NEXT-O-WIN-BD (make-board (list (make-piece 0 "O")
                                        (make-piece 6 "X")
                                        (make-piece 4 "O"))
                                  false 2))
         






(define (main bd)
  (big-bang bd
    (to-draw render)     
    (stop-when is-over? render-end)      
    (on-mouse actions)      
    (on-key change-depth)))







(check-expect (render START) BACKGROUND)


(check-expect (render BD1)
              (place-image (text "X" TEXTSIZE X-COLOR)
                           SIXTH-OF-SIZE
                           SIXTH-OF-SIZE
                           (place-image (text "O" TEXTSIZE O-COLOR)
                                        (* 5 SIXTH-OF-SIZE)
                                        (* 3 SIXTH-OF-SIZE)
                                        BACKGROUND)))



(define (render bd)
  (local [(define (find-piece-x pos)     
            (* SIXTH-OF-SIZE (+ 1 (* (modulo pos 3) 2))))
         
          (define (find-piece-y pos)     
            (* SIXTH-OF-SIZE (+ 1 (* (floor (/ pos 3)) 2))))
         
          (define (render-type type)     
            (if (string=? "X" type)
                (text type TEXTSIZE X-COLOR)
                (text type TEXTSIZE O-COLOR)))
         
          (define lop (board-pieces bd))
          
          (define (place-piece piece image)     
            (place-image (render-type (piece-type piece))
                         (find-piece-x (piece-position piece))
                         (find-piece-y (piece-position piece))
                         image))]
          
    (foldr place-piece BACKGROUND lop)))
                    
     





(check-expect (render-end EX:XWIN)
              (place-image (text "X WINS" TEXTSIZE TEXTCOLOR)
                           (/ SIZE 2)
                           (/ SIZE 2)
                           (render EX:XWIN)))


(check-expect (render-end EX:OWIN)
              (place-image (text "O WINS" TEXTSIZE TEXTCOLOR)
                           (/ SIZE 2)
                           (/ SIZE 2)
                           (render EX:OWIN)))


(check-expect (render-end EX:DRAW)
              (place-image (text "DRAW" TEXTSIZE TEXTCOLOR)
                           (/ SIZE 2)
                           (/ SIZE 2)
                           (render EX:DRAW)))



(define (render-end bd)
  (local [(define (render-board winner)
            (if (string=? winner "draw")
                (place-image (text "DRAW" TEXTSIZE TEXTCOLOR)
                             (/ SIZE 2)
                             (/ SIZE 2)
                             (render bd))
                (place-image (text
                              (string-append winner " WINS") TEXTSIZE TEXTCOLOR)
                             (/ SIZE 2)
                             (/ SIZE 2)
                             (render bd))))]
    
    (cond [(is-won-by? "X" (board-pieces bd))
           (render-board "X")]
          [(is-won-by? "O" (board-pieces bd))
           (render-board "O")]
          [else (render-board "draw")])))















(check-expect (actions START-FULL-DEPTH (* SIXTH-OF-SIZE 5)(* SIXTH-OF-SIZE 5)
                       "button-down")
              (make-board (list (make-piece 4 "X")
                                (make-piece 8 "O"))
                          true 8))


(check-expect (actions (make-board (rest (board-pieces EX:DRAW))
                                   true
                                   0) 0 0 "button-down")
              (make-board
               (cons (make-piece 0 "O") (rest (board-pieces EX:DRAW)))
               true
               0))



(define (actions bd x y event)
  (local [(define (coords->position x y)        
            (+ (* 3 (floor (/ (* 3 y) SIZE))) 
               (floor (/ (* 3 x) SIZE))))

          
          (define (position-not-taken? pos lop) 
            (andmap (λ (piece) (not (= pos (piece-position piece)))) lop))
   
          (define (player-move bd)                
            (make-board (cons (make-piece (coords->position x y) "O")
                              (board-pieces bd))
                        false (board-depth bd)))

          (define (choose-random-pos lo-pos)   
            (local [(define (choose-pos lo-pos index current-index) 
                      (if (= current-index index)
                          (first lo-pos)
                          (choose-pos (rest lo-pos)
                                      index
                                      (add1 current-index))))]
              (choose-pos lo-pos (random (length lo-pos)) 0)))

          (define (computer-move bd)  
            (cond 
              [(is-over? bd) (board-pieces bd)] 
              [(= 0 (board-depth bd))
               (cons (make-piece (choose-random-pos
                                  (find-empty-pos bd)) "X")
                     (board-pieces bd))]
              [else (minimax bd)]))]
    
    (if (and (string=? event "button-down")
             (position-not-taken? (coords->position x y) (board-pieces bd)))
        (make-board (computer-move (player-move bd)) true (board-depth bd))
        bd)))
    







(check-expect (full-board? (board-pieces START)) false)


(check-expect (full-board? (board-pieces EX:XWIN)) false)


(check-expect (full-board? (board-pieces EX:OWIN)) false)


(check-expect (full-board? (board-pieces EX:DRAW)) true)


 
(define (full-board? lop)
  (= (length lop) 9))








(check-expect (is-won-by? "X" (board-pieces START)) false)


(check-expect (is-won-by? "O" (board-pieces EX:OWIN)) true)


(check-expect (is-won-by? "X" (board-pieces EX:XWIN)) true)


(check-expect (is-won-by? "O" (board-pieces EX:XWIN)) false)


(check-expect (is-won-by? "O" (board-pieces EX:DRAW)) false)


(check-expect (is-won-by? "X" (board-pieces EX:DRAW)) false)



(define (is-won-by? type lop)
  (local [(define (type-at pos lop)   
            (cond [(empty? lop) ""]
                  [(= (piece-position (first lop)) pos)
                   (piece-type (first lop))]
                  [else
                   (type-at pos (rest lop))]))]
    (ormap (λ (n) (apply string=? (cons type (map (λ (p) (type-at p lop)) n))))
           WINNING-POSITIONS)))







(check-expect (is-over? START) false)


(check-expect (is-over? EX:XWIN) true)


(check-expect (is-over? EX:OWIN) true)


(check-expect (is-over? EX:DRAW) true)


(check-expect (is-over? EX:DRAW) true)



(define (is-over? bd)
  (local [(define lop (board-pieces bd))]
    (or (full-board? lop) (is-won-by? "X" lop) (is-won-by? "O" lop))))








(check-expect (find-empty-pos START) (list 0 1 2 3 4 5 6 7 8))


(check-expect (find-empty-pos EX:DRAW) empty)


(check-expect (find-empty-pos EX:XWIN)
              (list 1 2 3 5 6 7))



(define (find-empty-pos bd)
  (filter (λ (pos)
            (not (ormap (λ (filled-pos)
                          (= pos filled-pos)) (map piece-position
                                                   (board-pieces bd)))))
          (build-list 9 (λ (n) n))))







(check-expect (change-depth BD1 "7")
              (make-board (board-pieces BD1)
                          (board-user-playing? BD1) 7))


(check-expect (change-depth BD1 "j")
              BD1)



(define (change-depth bd ke)
  (local [(define depth (string->number ke))]
    (if (number? depth)
        (make-board (board-pieces bd) (board-user-playing? bd) depth)
        bd)))




















(check-expect (minimax BD2)
              (list (make-piece 6 "X")
                    (make-piece 4 "O")
                    (make-piece 0 "X")
                    (make-piece 2 "O")
                    ))


(check-expect (minimax NEXT-X-WIN-BD-1)
              (list (make-piece 2 "X")
                    (make-piece 0 "O")
                    (make-piece 6 "X")
                    (make-piece 3 "O")
                    (make-piece 4 "X")
                    (make-piece 8 "O")))


(check-expect (minimax NEXT-X-WIN-BD-2)
              (list (make-piece 2 "X")
                    (make-piece 0 "O")
                    (make-piece 6 "X")
                    (make-piece 3 "O")
                    (make-piece 4 "X")
                    (make-piece 8 "O")))


(check-expect (minimax NEXT-O-WIN-BD)
              (list (make-piece 8 "X") 
                    (make-piece 0 "O")
                    (make-piece 6 "X")
                    (make-piece 4 "O")))


(check-expect (minimax FULL-DEPTH-BD)
              (list (make-piece 4 "X")
                    (make-piece 8 "O")))



(define (minimax bd)
  (local [(define (find-player-type bd)   
            (if (board-user-playing? bd)
                "O"
                "X"))
          
          (define (evaluate bd)    
            (cond [(is-won-by? "O" (board-pieces bd)) 1]
                  [(is-won-by? "X" (board-pieces bd)) -1]
                  [else 0]))
          
          (define (next-boards bd) 
            (map (λ (empty-pos)
                   (make-board (cons (make-piece empty-pos
                                                 (find-player-type bd))
                                     (board-pieces bd))
                               (not (board-user-playing? bd))
                               (sub1 (board-depth bd))))
                 (find-empty-pos bd)))

          
          (define (select-player-fn bd)
            (if (board-user-playing? bd) 
                max
                min))
 
          (define (minimax--bd bd)    
            (cond
              [(is-over? bd)(evaluate bd)]
              [(= (board-depth bd) 0) 0]
              [else
               (apply (select-player-fn bd)
                      (map minimax--bd (next-boards bd)))]))

          (define (rand-order list)    
            (sort list (λ (n p) (= 1 (random 2)))))]
    
    (board-pieces (argmin minimax--bd (rand-order (next-boards bd))))))