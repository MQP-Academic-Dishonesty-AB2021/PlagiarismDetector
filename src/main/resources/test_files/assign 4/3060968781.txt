

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname VenatArjunVoletiVivek_Assignment3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)


(define SIZE 300)
(define MTS (empty-scene SIZE SIZE))



(define BOARD-SKELETON .)
(define BOARD-SCALE-FACTOR (/ SIZE (image-width BOARD-SKELETON)))

(define X-COLOR "red")
(define O-COLOR "blue")
(define RESULT-COLOR "green")
(define FONT-SIZE (/ SIZE 4))






(define UNIT-INDICES-ROWS (list (list 0 1 2)
                                (list 3 4 5)
                                (list 6 7 8)))

(define UNIT-INDICES-COLS (list (list 0 3 6)
                                (list 1 4 7)
                                (list 2 5 8)))

(define UNIT-INDICES-DIAGS (list (list 0 4 8)
                                 (list 2 4 6)))


(define UNIT-INDICES (append UNIT-INDICES-ROWS
                             UNIT-INDICES-COLS
                             UNIT-INDICES-DIAGS))


(define UNIT-LENGTH 3)


(define-struct player (board engine-depth player-turn?))

































(define empty-board (list "" "" ""
                          "" "" ""
                          "" "" ""))

(define midgame-board1 (list "X" "" "X"
                             "" "O" ""
                             "O" "" "O"))

(define midgame-board2 (list "O" "" "X"
                             "" "X" "O"
                             "O" "" "X"))

(define draw-board1 (list "X" "X" "O"
                          "O" "X" "X"
                          "X" "O" "O"))

(define draw-board2 (list "O" "X" "X"
                          "X" "O" "O"
                          "O" "X" "X"))

(define X-win-board (list "X" "X" "X"
                          ""  "O" "O"
                          "O" ""  "X"))

(define O-win-board (list ""  ""  "O"
                          ""  "O" "X"
                          "O" "X" "X"))



(define row1-win (list "X" "X" "X"
                       ""  "O" "O"
                       "O" ""  "X"))

(define row2-win (list ""  "O" "O"
                       "X" "X" "X"
                       "O" ""  "X"))

(define row3-win (list "X" ""  ""
                       ""  "X" ""
                       "O" "O" "O"))


(define column1-win (list "X"  "" ""
                          "X" "O" "O"
                          "X"  "" ""))

(define column2-win (list "X" "O" "X"
                          ""  "O" "X"
                          ""  "O" ""))

(define column3-win (list  ""  ""  "O"
                           "X" "X" "O"
                           ""  "X" "O"))


(define diag1-win (list "X" "O" "O"
                        "X" "X" ""
                        "O" ""  "X"))

(define diag2-win (list "X" "" "O"
                        "X" "O" ""
                        "O" "" "X"))



(define START (make-player empty-board 0 true))

(define P1 (make-player midgame-board1 0 true))
(define P2 (make-player row1-win 2 false))
(define P3 (make-player column3-win 0 true))
(define P4 (make-player diag2-win 0 false))
(define P5 (make-player empty-board 9 true))
(define P6 (make-player midgame-board2 7 true))
(define P7 (make-player draw-board1 1 false))


(define (main player)
  (big-bang player
    (on-tick tick)
    (to-draw render)
    (on-key keypress)
    (on-mouse click)))















(check-expect (tick P1) P1)
(check-expect (tick P3) P3)
(check-expect (make-player midgame-board1 2 "true")
              (make-player midgame-board1 2 "true"))


(check-expect (tick (make-player midgame-board1 1 false)) 
              (make-player (list "X" "" "X"
                                 "" "O" ""
                                 "O" "O" "O")
                           1
                           true))

(check-expect (tick (make-player (list "X" "O" "" 
                                       "X" "O" ""
                                       ""  "" "X")
                                 1
                                 false))
              (make-player (list "X" "O" ""
                                 "X" "O" ""
                                 ""  "O" "X")
                           1
                           true))

(check-expect (tick (make-player (list "X" "" ""  
                                       "X" "O" ""
                                       "O" "" "X")
                                 1
                                 false))
              (make-player (list "X" "" "O"
                                 "X" "O" ""
                                 "O" "" "X")
                           1
                           true))



(check-expect (tick (make-player (list "" "" ""
                                       "" "X" ""
                                       "X" "" "O")
                                 2
                                 false))
              (make-player (list "" "" "O" 
                                 "" "X" ""
                                 "X" "" "O")
                           2
                           true))

(check-expect (tick (make-player (list "" "" "" 
                                       "X" "" "X"
                                       "" "O" "")
                                 2
                                 false))
              (make-player (list "" "" "" 
                                 "X" "O" "X"
                                 "" "O" "")
                           2
                           true))





(check-expect (tick (make-player (list "X" "" "O" 
                                       "" "O" ""
                                       "X" "" "X")
                                 2
                                 false))
              (make-player (list "X" "" "O"
                                 "O" "O" ""
                                 "X" "" "X")
                           2
                           true))

(check-expect (tick (make-player (list "X" "" "" 
                                       "" "X" "O"
                                       "X" "O" "")
                                 2
                                 false))
              (make-player (list "X" "" "O"
                                 "" "X" "O"
                                 "X" "O" "")
                           2
                           true))


(check-expect (tick (make-player midgame-board1 2 false)) 
              (make-player (list "X" "" "X"
                                 "" "O" ""
                                 "O" "O" "O")
                           2
                           true))

(check-expect (tick (make-player (list "X" "" ""  
                                       "X" "O" ""
                                       "O" "" "X")
                                 2
                                 false))
              (make-player (list "X" "" "O"
                                 "X" "O" ""
                                 "O" "" "X")
                           2
                           true))



(check-expect (tick (make-player (list "" "" ""
                                       "" "X" ""
                                       "X" "" "O")
                                 3
                                 false))
              (make-player (list "" "" "O" 
                                 "" "X" ""
                                 "X" "" "O")
                           3
                           true))

(check-expect (tick (make-player (list "" "" "" 
                                       "X" "" "X"
                                       "" "O" "")
                                 6
                                 false))
              (make-player (list "" "" "" 
                                 "X" "O" "X"
                                 "" "O" "")
                           6
                           true))


(check-expect (tick (make-player (list "X" "" "O" 
                                       "" "O" ""
                                       "X" "" "X")
                                 7
                                 false))
              (make-player (list "X" "" "O"
                                 "O" "O" ""
                                 "X" "" "X")
                           7
                           true))

(check-expect (tick (make-player (list "X" "" "" 
                                       "" "X" "O"
                                       "X" "O" "")
                                 3
                                 false))
              (make-player (list "X" "" "O"
                                 "" "X" "O"
                                 "X" "O" "")
                           3
                           true))


(check-expect (tick (make-player midgame-board1 3 false)) 
              (make-player (list "X" "" "X"
                                 "" "O" ""
                                 "O" "O" "O")
                           3
                           true))

(check-expect (tick (make-player (list "X" "" ""  
                                       "X" "O" ""
                                       "O" "" "X")
                                 5
                                 false))
              (make-player (list "X" "" "O"
                                 "X" "O" ""
                                 "O" "" "X")
                           5
                           true))


(check-expect (tick (make-player X-win-board 1 false)) 
              (make-player X-win-board 1 false)) 
(check-expect (tick (make-player O-win-board 2 false)) 
              (make-player O-win-board 2 false))
(check-expect (tick P7) P7) 


(define (tick player)
  (if (not (player-player-turn? player))
      (local
        [(define (random-cpu-move player)
           (if (string=? (game-state (player-board player)) "")
               
               (local [(define num-blanks (length (filter (λ (cell) (string=? cell ""))
                                                          (player-board player))))

                       
                       (define random-blank (random num-blanks))

                       
                       
                       
              
                       (define (fill-random-blank current-index number-blank)
                         (if (and (string=? (list-ref (player-board player) current-index) "")
                                  (= random-blank number-blank))
                             
                             (place-char (player-board player) "O" current-index)

                             
                             
                             (fill-random-blank (add1 current-index)
                                                (if (string=? (list-ref (player-board player)
                                                                        current-index)
                                                              "")
                                                    (add1 number-blank)
                                                    number-blank))))]

                 
                 
                 (make-player (fill-random-blank 0 0)
                              (player-engine-depth player)
                              (not (player-player-turn? player))))
        
               
               player))

         
         
         
         (define (next-boards board char)
           (local [(define (next-boards-inner current-index)
                     (cond [(= current-index (length board)) empty]
                           [else (if (string=? (list-ref board current-index) "")
                                     (cons (place-char board char current-index)
                                           (next-boards-inner (add1 current-index)))
                                     (next-boards-inner (add1 current-index)))]))]
             (next-boards-inner 0))) 

         
         (define (all-potential-winning-boards board char)
           (filter (λ (potential-board)
                     (string=? (game-state potential-board)
                               (string-append char " wins")))
                   (next-boards board char)))

         
         (define (grab-win player char)
           (if (empty? (all-potential-winning-boards (player-board player) char))
               
               (random-cpu-move player)

               
               (make-player (first (all-potential-winning-boards (player-board player) char))
                            (player-engine-depth player)
                            (not (player-player-turn? player)))))

         
         
         (define (block player)
           
           (if (empty? (all-potential-winning-boards (player-board player) "O"))
               
               (if (empty? (all-potential-winning-boards (player-board player) "X"))
                   
                   (random-cpu-move player)

                   
                   (local [(define (determine-win-index current-index)
                             
                             
                             
                             
                             
                             (if (not (string=? (list-ref (player-board player) current-index)
                                                (list-ref (first (all-potential-winning-boards
                                                                  (player-board player)
                                                                  "X"))
                                                          current-index)))
                                  
                                 
                                 current-index

                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 (determine-win-index (add1 current-index))))] 
                      
                     
                     (make-player (place-char (player-board player) "O" (determine-win-index 0)) 
                                  (player-engine-depth player)
                                  (not (player-player-turn? player)))))
                
               
               (grab-win player "O")))]

        (if (string=? (game-state (player-board player)) "")
            (cond [(= (player-engine-depth player) 0) (random-cpu-move player)]
                  [(= (player-engine-depth player) 1) (grab-win player "O")]
                  [(= (player-engine-depth player) 2) (block player)]
                  [else (block player)])
            player))
      player))

























































































(check-expect (game-state X-win-board) "X wins")
(check-expect (game-state diag1-win) "X wins")


(check-expect (game-state O-win-board) "O wins")
(check-expect (game-state diag2-win) "O wins")


(check-expect (game-state draw-board1) "Draw")
(check-expect (game-state draw-board2) "Draw")


(check-expect (game-state midgame-board1) "")
(check-expect (game-state midgame-board2) "")
(check-expect (game-state empty-board) "")


(define (game-state board)
  
  
  (local [(define (make-board-unit-from-index-unit index-unit)
            (map (λ (index) (list-ref board index)) index-unit))

          
          (define all-board-units
            (map make-board-unit-from-index-unit UNIT-INDICES))

          
          
          (define (three-of-kind-in-unit unit)
            (if (and (string=? (first unit) (second unit) (third unit))
                     (not (string=? (first unit) "")))
                (first unit) 
                "")) 

          
          (define winning-board-units (filter (λ (win-check-result)
                                                (not (string=? win-check-result "")))
                                              (map three-of-kind-in-unit all-board-units)))

          
          (define (determine-win-state board)
            (if (not (empty? winning-board-units))
                (first winning-board-units)
                ""))]

    
    (if (not (string=? (determine-win-state board) ""))
        
        (string-append (determine-win-state board) " wins")

        
        (local [(define (board-filled? board)
                  (andmap (λ (cell) (not (string=? cell ""))) board))]
          (if (board-filled? board)
              "Draw"
              "")))))
  











(check-expect (render START)
              (place-image (scale BOARD-SCALE-FACTOR BOARD-SKELETON)
                           (/ SIZE 2)
                           (/ SIZE 2)
                           MTS))


(check-expect (render (make-player (list "" "" "X"
                                         "" "" ""
                                         "" "" "")
                                   4
                                   true))
              (place-image (text "X" (round FONT-SIZE) X-COLOR)
                           (* 5 (/ SIZE 6))
                           (/ SIZE 6)
                           (render START)))


(check-expect (render (make-player (list "" "" ""
                                         "" "" ""
                                         "" "O" "")
                                   0
                                   false))
              (place-image (text "O" (round FONT-SIZE) O-COLOR)
                           (/ SIZE 2)
                           (* 5 (/ SIZE 6))
                           (render START)))


(check-expect (render P1)
              (place-images (list (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR))
                            (list (make-posn (/ SIZE 6) (/ SIZE 6))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 6))
                                  (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (/ SIZE 6) (* 5 (/ SIZE 6)))
                                  (make-posn (* 5 (/ SIZE 6)) (* 5 (/ SIZE 6))))
                            (render START)))


(check-expect (render P6)
              (place-images (list (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR))
                            (list (make-posn (/ SIZE 6) (/ SIZE 6))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 6))
                                  (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 2))
                                  (make-posn (/ SIZE 6) (* 5 (/ SIZE 6)))
                                  (make-posn (* 5 (/ SIZE 6)) (* 5 (/ SIZE 6))))
                            (render START)))
 

(check-expect (render (make-player X-win-board 8 false))
              (place-images (list (text "X wins" (round FONT-SIZE) RESULT-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR))
                            (list (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (/ SIZE 6) (/ SIZE 6))
                                  (make-posn (/ SIZE 2) (/ SIZE 6))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 6))
                                  (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 2))
                                  (make-posn (/ SIZE 6) (* 5 (/ SIZE 6)))
                                  (make-posn (* 5 (/ SIZE 6)) (* 5 (/ SIZE 6))))
                            (render START)))


(check-expect (render (make-player row2-win 3 true))
              (place-images (list (text "X wins" (round FONT-SIZE) RESULT-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR))
                            (list (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (/ SIZE 2) (/ SIZE 6))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 6))
                                  (make-posn (/ SIZE 6) (/ SIZE 2))
                                  (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 2))
                                  (make-posn (/ SIZE 6) (* 5 (/ SIZE 6)))
                                  (make-posn (* 5 (/ SIZE 6)) (* 5 (/ SIZE 6))))
                            (render START)))


(check-expect (render (make-player O-win-board 7 true))
              (place-images (list (text "O wins" (round FONT-SIZE) RESULT-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR))
                            (list (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 6))
                                  (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 2))
                                  (make-posn (/ SIZE 6) (* 5 (/ SIZE 6)))
                                  (make-posn (/ SIZE 2) (* 5 (/ SIZE 6)))
                                  (make-posn (* 5 (/ SIZE 6)) (* 5 (/ SIZE 6))))
                            (render START)))


(check-expect (render (make-player column3-win 0 false))
              (place-images (list (text "O wins" (round FONT-SIZE) RESULT-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR))
                            (list (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 6))
                                  (make-posn (/ SIZE 6) (/ SIZE 2))
                                  (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 2))
                                  (make-posn (/ SIZE 2) (* 5 (/ SIZE 6)))
                                  (make-posn (* 5 (/ SIZE 6)) (* 5 (/ SIZE 6))))
                            (render START)))


(check-expect (render (make-player draw-board1 1 false))
              (place-images (list (text "Draw" (round FONT-SIZE) RESULT-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR))
                            (list (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (/ SIZE 6) (/ SIZE 6))
                                  (make-posn (/ SIZE 2) (/ SIZE 6))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 6))
                                  (make-posn (/ SIZE 6) (/ SIZE 2))
                                  (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 2))
                                  (make-posn (/ SIZE 6) (* 5 (/ SIZE 6)))
                                  (make-posn (/ SIZE 2) (* 5 (/ SIZE 6)))
                                  (make-posn (* 5 (/ SIZE 6)) (* 5 (/ SIZE 6))))
                            (render START)))


(check-expect (render (make-player draw-board2 9 true))
              (place-images (list (text "Draw" (round FONT-SIZE) RESULT-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "O" (round FONT-SIZE) O-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR)
                                  (text "X" (round FONT-SIZE) X-COLOR))
                            (list (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (/ SIZE 6) (/ SIZE 6))
                                  (make-posn (/ SIZE 2) (/ SIZE 6))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 6))
                                  (make-posn (/ SIZE 6) (/ SIZE 2))
                                  (make-posn (/ SIZE 2) (/ SIZE 2))
                                  (make-posn (* 5 (/ SIZE 6)) (/ SIZE 2))
                                  (make-posn (/ SIZE 6) (* 5 (/ SIZE 6)))
                                  (make-posn (/ SIZE 2) (* 5 (/ SIZE 6)))
                                  (make-posn (* 5 (/ SIZE 6)) (* 5 (/ SIZE 6))))
                            (render START)))


(define (render player)
  
  
  
  (local [(define list-of-cell-images
            (map (λ (char)
                   (text char
                         (round FONT-SIZE)
                         (if (string=? char "X")
                             X-COLOR
                             O-COLOR)))
                 (player-board player)))

          
          
          
          
          
          (define list-of-cell-posns
            (build-list
             9
             (λ (index) (make-posn
                         
                         
                         (* (/ SIZE (* UNIT-LENGTH 2)) (+ (* 2 (modulo index UNIT-LENGTH)) 1))

                         
                         
                         (* (/ SIZE (* UNIT-LENGTH 2))
                            (+ 1 (* 2 (/ (- index (modulo index UNIT-LENGTH)) UNIT-LENGTH))))))))]
    
    
    
    (place-images (append (list (text (game-state (player-board player))
                                      (round FONT-SIZE)
                                      RESULT-COLOR))
                          list-of-cell-images
                          (list (scale BOARD-SCALE-FACTOR BOARD-SKELETON)))
                  
                  (append (list (make-posn (/ SIZE 2) (/ SIZE 2)))
                          list-of-cell-posns
                          (list (make-posn (/ SIZE 2) (/ SIZE 2))))
                  MTS)))









(check-expect (keypress P1 "0") (make-player (player-board P1) 0 (player-player-turn? P1)))
(check-expect (keypress P2 "4") (make-player (player-board P2) 4 (player-player-turn? P2)))
(check-expect (keypress P3 "8") (make-player (player-board P3) 8 (player-player-turn? P3)))
(check-expect (keypress P4 "9") (make-player (player-board P4) 9 (player-player-turn? P4)))


(check-expect (keypress START "h") START)
(check-expect (keypress P1 "p") P1)
(check-expect (keypress P3 "shift") P3)
(check-expect (keypress P5 "left") P5)


(define (keypress player key)
  (local [(define (contains? s los)
            (ormap (λ (element) (string=? element s)) los))]
    
    (if (contains? key (build-list 10 number->string))
        (make-player (player-board player) (string->number key) (player-player-turn? player))
        player)))










(check-expect (click P5 SIZE SIZE "button-down")
              (make-player (place-char (player-board P5)
                                       "X"
                                       (get-box SIZE SIZE))
                           (player-engine-depth P5)
                           (not (player-player-turn? P5))))


(check-expect (click P1 (/ SIZE 2) (/ SIZE 6) "button-down") 
              (make-player (place-char (player-board P1)
                                       "X"
                                       (get-box (/ SIZE 2) (/ SIZE 6)))
                           (player-engine-depth P1)
                           (not (player-player-turn? P1))))


(check-expect (click P6 (/ SIZE 6) (/ SIZE 2) "button-down") 
              (make-player (place-char (player-board P6)
                                       "X"
                                       (get-box (/ SIZE 6) (/ SIZE 2)))
                           (player-engine-depth P6)
                           (not (player-player-turn? P6))))





(check-expect (click P2 SIZE SIZE "button-down") P2)
(check-expect (click P4 0 0 "button-down") P4)
(check-expect (click P7 (/ SIZE 2) SIZE "button-down") P7)

(check-expect (click (make-player midgame-board1 4 false) SIZE (/ SIZE 2) "button-down")
              (make-player midgame-board1 4 false)) 

(check-expect (click (make-player midgame-board2 5 false) (/ SIZE 6) SIZE "button-down")
              (make-player midgame-board2 5 false)) 


(check-expect (click P1 SIZE SIZE "button-down") P1)


(check-expect (click P7 (/ SIZE 2) (/ SIZE 2) "button-down") P7)


(check-expect (click (make-player X-win-board 0 true)
                     0
                     (* 5 (/ SIZE 6))
                     "button-down")
              (make-player row1-win 0 true))


(check-expect (click (make-player X-win-board 4 false)
                     SIZE
                     (/ SIZE 2)
                     "button-down")
              (make-player row1-win 4 false))


(check-expect (click (make-player row2-win 3 false) 0 0 "button-down")
              (make-player row2-win 3 false))

(check-expect (click (make-player column1-win 1 false) (/ SIZE 2) SIZE "button-down")
              (make-player column1-win 1 false))

(check-expect (click (make-player diag1-win 7 true) SIZE (/ SIZE 2) "button-down")
              (make-player diag1-win 7 true))


(check-expect (click P1 (/ SIZE 2) (/ SIZE 2) "button-up") P1)


(define (click player x y me)
  (cond [(and (mouse=? me "button-down") 
              (player-player-turn? player) 
              (string=? (list-ref (player-board player) (get-box x y)) "") 
              (string=? (game-state (player-board player)) "")) 
              
         
         (make-player (place-char (player-board player)
                                  "X"
                                  (get-box x y))
                      (player-engine-depth player)
                      (not (player-player-turn? player)))]
        [else player]))












(check-expect (get-box 0 0) 0) 
(check-expect (get-box 0 SIZE) 6) 
(check-expect (get-box SIZE 0) 2) 
(check-expect (get-box SIZE SIZE) 8) 


(check-expect (get-box 0 (/ SIZE 2)) 3) 
(check-expect (get-box (/ SIZE 3) (/ SIZE 6)) 1) 
(check-expect (get-box (* 2 (/ SIZE 3)) (/ SIZE 2)) 5) 
(check-expect (get-box SIZE (* 5 (/ SIZE 6))) 8) 

(check-expect (get-box (/ SIZE 2) 0) 1) 
(check-expect (get-box (/ SIZE 6) (/ SIZE 3)) 3) 
(check-expect (get-box (/ SIZE 2) (* 2 (/ SIZE 3))) 7) 
(check-expect (get-box (* 5 (/ SIZE 6)) SIZE) 8) 


(check-expect (get-box (/ SIZE 6) (/ SIZE 6)) 0) 
(check-expect (get-box (/ SIZE 2) (/ SIZE 6)) 1) 
(check-expect (get-box (* 5 (/ SIZE 6)) (/ SIZE 6)) 2) 
(check-expect (get-box (/ SIZE 6) (/ SIZE 2)) 3) 
(check-expect (get-box (/ SIZE 2) (/ SIZE 2)) 4) 
(check-expect (get-box (* 5 (/ SIZE 6)) (/ SIZE 2)) 5) 
(check-expect (get-box (/ SIZE 6) (* 5 (/ SIZE 6))) 6) 
(check-expect (get-box (/ SIZE 2) (* 5 (/ SIZE 6))) 7) 
(check-expect (get-box (* 5 (/ SIZE 6)) (* 5 (/ SIZE 6))) 8) 


(define (get-box x y)
  (local
    
    [(define row (cond [(< y (/ SIZE 3)) 0] 
                       [(< y (* 2 (/ SIZE 3))) 1] 
                       [(<= y SIZE) 2])) 
     
     
     (define col (cond [(< x (/ SIZE 3)) 0] 
                       [(< x (* 2 (/ SIZE 3))) 1] 
                       [(<= x SIZE) 2]))] 

    
    (+ (* row 3) col)))















(check-expect (place-char empty "X" 2) empty) 

(check-expect (place-char empty-board "O" 3) 
              (list ""  "" "" 
                    "O" "" ""
                    ""  "" "")) 

(check-expect (place-char midgame-board1 "O" 1) 
              (list "X" "O" "X"
                    ""  "O" ""
                    "O" ""  "O"))







(define (place-char los char pos)
  (local [(define (place-char-inner los current-index)
            (cond [(empty? los) empty]
                  [else (append (if (= current-index pos)
                                    (list char)
                                    (list (first los)))
                                (place-char-inner (rest los)
                                                  (add1 current-index)))]))]
    (place-char-inner los 0))) 