

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tic tac toe starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 872) 
(define FONT-SIZE (round (/ SIZE 10)))
(define OFFSET (round (/ SIZE 10))) 
(define BOARD-SIZE (round (- SIZE (* 2 OFFSET))))
(define SQUARE-SIZE (round (/ BOARD-SIZE 3)))
(define MTS (empty-scene SIZE SIZE))
(define X-COLOR "red")
(define O-COLOR "blue")
(define BOARD-COLOR "black") 
(define WIN-COLOR "purple") 

(define BOX-COORDS 
  
  
  (list OFFSET (+ OFFSET SQUARE-SIZE) (+ OFFSET (* 2 SQUARE-SIZE))))

(define END (- SIZE OFFSET))

(define BACKGROUND (add-line
                    
                    (add-line
                     (add-line
                      (add-line
                       MTS
                       (list-ref BOX-COORDS 0) (list-ref BOX-COORDS 2)
                       END (list-ref BOX-COORDS 2) BOARD-COLOR)
                      (list-ref BOX-COORDS 0) (list-ref BOX-COORDS 1)
                      END (list-ref BOX-COORDS 1) BOARD-COLOR)
                     (list-ref BOX-COORDS 2) (list-ref BOX-COORDS 0)
                     (list-ref BOX-COORDS 2) END BOARD-COLOR)
                    (list-ref BOX-COORDS 1) (list-ref BOX-COORDS 0)
                    (list-ref BOX-COORDS 1) END BOARD-COLOR))                             


(define-struct pos (x y))




(define POS1 (make-pos 0 0))
(define POS2 (make-pos 100 100))
(define POS3 (make-pos OFFSET OFFSET))







(define-struct ws (board turn ai))





(define START (make-ws (list "" "" ""
                             "" "" ""
                             "" "" "") "X" 0)) 

(define WS2 (make-ws (list "X" "X" "X"
                           "O" "O" ""
                           "" "" "") "_X" 1)) 

(define WS3 (make-ws (list "O" "X" "O"
                           "O" "X" "X"
                           "X" "O" "X") "_D" 2)) 

(define WS4 (make-ws (list "O" "X" "X"
                           "X" "O" ""
                           "" "" "O") "_O" 1)) 

(define WS5 (make-ws (list "X" "O" ""
                           "X" "" "O"
                           "X" "" "") "_X" 0)) 

(define WS6 (make-ws (list "" "X" "O"
                           "" "X" ""
                           "O" "" "") "X" 2)) 
(define WS7 (make-ws (list "X" "O" "X"
                           "" "" ""
                           "" "" "") "O" 2)) 
(define WS8 (make-ws (list "O" "X" ""
                           "O" "X" "X"
                           "" "" "") "O" 1)) 
(define WS9 (make-ws (list "O" "X" "X"
                           "X" "O" ""
                           "" "" "") "O" 1)) 
(define WS1 (make-ws (list "O" "X" ""
                           "" "X" ""
                           "" "" "") "O" 2)) 





(check-expect (grid-pos->pos 0) (make-pos (list-ref BOX-COORDS 0)
                                          (list-ref BOX-COORDS 0)))
(check-expect (grid-pos->pos 4) (make-pos (list-ref BOX-COORDS 1)
                                          (list-ref BOX-COORDS 1)))
(check-expect (grid-pos->pos 8) (make-pos (list-ref BOX-COORDS 2)
                                          (list-ref BOX-COORDS 2)))

(define (grid-pos->pos grid-pos) (make-pos (list-ref BOX-COORDS
                                                     (modulo grid-pos 3))
                                           (list-ref BOX-COORDS
                                                     (quotient grid-pos 3))))






(check-expect (render START) BACKGROUND) 
(check-expect
 (render WS2)
 (overlay (text "X Wins" FONT-SIZE WIN-COLOR)
          (render-move
           WS2 0
           (render-move
            WS2 1
            (render-move
             WS2 2
             (render-move
              WS2 3
              (render-move
               WS2 4
               (render-move
                WS2 5
                (render-move
                 WS2 6
                 (render-move
                  WS2 7
                  (render-move
                   WS2 8
                   BACKGROUND))))))))))) 
(check-expect
 (render WS3)
 (overlay (text "Draw" FONT-SIZE WIN-COLOR)
          (render-move
           WS3 0
           (render-move
            WS3 1
            (render-move
             WS3 2
             (render-move
              WS3 3
              (render-move
               WS3 4
               (render-move
                WS3 5
                (render-move
                 WS3 6
                 (render-move
                  WS3 7
                  (render-move
                   WS3 8
                   BACKGROUND))))))))))) 
(check-expect
 (render WS4)
 (overlay (text "O Wins" FONT-SIZE WIN-COLOR)
          (render-move
           WS4 0
           (render-move
            WS4 1
            (render-move
             WS4 2
             (render-move
              WS4 3
              (render-move
               WS4 4
               (render-move
                WS4 5
                (render-move
                 WS4 6
                 (render-move
                  WS4 7
                  (render-move
                   WS4 8
                   BACKGROUND))))))))))) 
(check-expect
 (render WS7)
 (render-move
  WS7 0
  (render-move
   WS7 1
   (render-move
    WS7 2
    (render-move
     WS7 3
     (render-move
      WS7 4
      (render-move
       WS7 5
       (render-move
        WS7 6
        (render-move
         WS7 7
         (render-move
          WS7 8
          BACKGROUND)))))))))) 
(define (render ws)
  
  
  
  (local [(define (draw index isf)
            (cond [(> index 8) isf]
                  [else
                   (draw (add1 index) (render-move ws index isf))]))]
    (overlay (cond
               [(string=? (ws-turn ws) "_D")
                (text "Draw" FONT-SIZE WIN-COLOR)]
               [(string=? (ws-turn ws) "_O")
                (text "O Wins" FONT-SIZE WIN-COLOR)]
               [(string=? (ws-turn ws) "_X")
                (text "X Wins" FONT-SIZE WIN-COLOR)]
               [else empty-image])
             (draw 0 BACKGROUND))))                






(check-expect (render-move START 0 BACKGROUND) BACKGROUND) 
(check-expect (render-move WS4 3 MTS) 
              (place-image (text "X" FONT-SIZE X-COLOR)
                           (+ (pos-x (grid-pos->pos 3))
                              (/ SQUARE-SIZE 2))
                           (+ (pos-y (grid-pos->pos 3))
                              (/ SQUARE-SIZE 2))
                           MTS))
(check-expect (render-move WS6 2 BACKGROUND) 
              (place-image (text "O" FONT-SIZE O-COLOR)
                           (+ (pos-x (grid-pos->pos 2))
                              (/ SQUARE-SIZE 2))
                           (+ (pos-y (grid-pos->pos 2))
                              (/ SQUARE-SIZE 2))
                           BACKGROUND))

(define (render-move ws board-index image)
  (local 
    [(define (x-or-o-color str) (if (string=? "O" str) O-COLOR X-COLOR))
     (define move (list-ref (ws-board ws) board-index))
     (define this-pos (grid-pos->pos board-index))]
    (place-image (text move FONT-SIZE (x-or-o-color move))
                 (+ (pos-x this-pos) (/ SQUARE-SIZE 2))
                 (+ (pos-y this-pos) (/ SQUARE-SIZE 2)) image)))









(check-expect (handle-mouse START 0 0 "drag") START)

(check-expect (handle-mouse START 1 1 "button-down") START)

(check-expect (handle-mouse WS3 (+ OFFSET 1) (+ OFFSET 1) "button-down") WS3)

(check-expect
 (handle-mouse WS4 
               (+ (pos-x (grid-pos->pos 6)) 1)
               (+ (pos-y (grid-pos->pos 6)) 1)
               "button-down") WS4)
(check-expect
 (handle-mouse WS5 
               (+ (pos-x (grid-pos->pos 8)) 1)
               (+ (pos-y (grid-pos->pos 8)) 1)
               "button-down") WS5)
(check-expect
 (handle-mouse WS6 
               (+ (pos-x (grid-pos->pos 1)) 1)
               (+ (pos-y (grid-pos->pos 1)) 1)
               "button-down") WS6)
(check-random
 (handle-mouse WS6  
               (+ (pos-x (grid-pos->pos 7)) 1)
               (+ (pos-y (grid-pos->pos 7)) 1)
               "button-down")
 (make-ws (list-set (ws-board WS6) 7 "X") "_X" (ws-ai WS6)))
(check-random
 (handle-mouse START 
               (+ (pos-x (grid-pos->pos 2)) 10)
               (+ (pos-y (grid-pos->pos 2)) 10)
               "button-down")
 (ai-move (make-ws (list-set (ws-board START) 2 "X") "O" (ws-ai START))))
(check-random
 (handle-mouse WS6 
               (+ (pos-x (grid-pos->pos 5)) 8)
               (+ (pos-y (grid-pos->pos 5)) 8)
               "button-down")
 (ai-move (make-ws (list-set (ws-board WS6) 5 "X") "O" (ws-ai WS6))))

(define (handle-mouse ws x y mouse)
  (cond [(not (mouse=? mouse "button-down")) ws]
        [(not (string=? (ws-turn ws) "X")) ws]
        [(or (<= x OFFSET)
             (>= x END)
             (<= y OFFSET)
             (>= y END)) ws]
        [else (if
               (string=? "" (list-ref (ws-board ws)
                                      (pos->grid-pos (make-pos x y))))
               (ai-move (player-move ws (pos->grid-pos (make-pos x y)) "X")) 
               ws)]))







(define (player-move ws index player)
  (local [(define new-board (list-set (ws-board ws) index player))]
    (make-ws new-board (next-player new-board player index) (ws-ai ws))))

(check-expect (player-move START 0 "X")                 
              (make-ws (list-set (ws-board START) 0 "X")
                       "O"
                       (ws-ai START)))
(check-expect (player-move WS6 7 "X")                   
              (make-ws (list-set (ws-board WS6) 7 "X")
                       "_X"
                       (ws-ai WS6)))
(check-expect (player-move WS7 4 "O")
              (make-ws (list-set (ws-board WS7) 4 "O")
                       "X"
                       (ws-ai WS7)))






(define (next-player board player index) 
  (local [(define (compare i)
            (string=? (list-ref board i) player))
          (define (check-vertical col)
            (andmap compare
                    (build-list 3 (λ (n) (+ col (* 3 n))))))
          (define (check-horizontal row)
            (andmap compare
                    (build-list 3 (λ (n) (+ (* row 3) n)))))
          (define check-left-diag
            (andmap compare
                    (build-list 3 (λ (n) (* 4 n)))))
          (define check-right-diag
            (andmap compare
                    (build-list 3 (λ (n) (* 2 (+ 1 n))))))
          (define check-full
            (not (ormap (λ (s) (string=? s "")) board)))
          (define (next-player current)
            (cond [(string=? current "X") "O"]
                  [(string=? current "O") "X"]))]
    (cond [(or (check-vertical (modulo index 3))
               (check-horizontal (quotient index 3))
               check-left-diag
               check-right-diag)
           (string-append "_" player)]
          [check-full "_D"]
          [else (next-player player)])))
         
                  
(check-expect (next-player (ws-board WS2) "X" 2) "_X")  
(check-expect (next-player (ws-board WS3) "O" 0) "_D")  
(check-expect (next-player (ws-board WS4) "O" 8) "_O")  
(check-expect (next-player (ws-board WS5) "X" 6) "_X")  
(check-expect (next-player (ws-board WS6) "X" 4) "O") 
(check-expect (next-player (ws-board WS7) "O" 1) "X") 







(check-expect (pos->grid-pos (make-pos (+ 1 OFFSET) (+ 1 OFFSET))) 0)

(check-expect (pos->grid-pos (make-pos (+ 2 OFFSET) (+ 1 OFFSET)))
              
              (+ (quotient (- (+ 2 OFFSET) OFFSET) SQUARE-SIZE)
                 (* 3 (quotient (- (+ 1 OFFSET) OFFSET) SQUARE-SIZE))))
(check-expect (pos->grid-pos (make-pos (+ 1 (pos-x (grid-pos->pos 2)))
                                       (+ 2 (pos-y (grid-pos->pos 2))))) 2)

(check-expect (pos->grid-pos (make-pos (+ 2 (pos-x (grid-pos->pos 8)))
                                       (+ 3 (pos-y (grid-pos->pos 8))))) 8)


(define (pos->grid-pos pos)
  (+ (quotient (- (pos-x pos) OFFSET) SQUARE-SIZE)
     (* 3 (quotient (- (pos-y pos) OFFSET) SQUARE-SIZE))))





(check-expect (open-spaces (ws-board START)) (build-list 9 identity))

(check-expect (open-spaces (ws-board WS2)) (list 5 6 7 8))

(check-expect (open-spaces (ws-board WS3)) empty)

(check-expect (open-spaces (ws-board WS6)) (list 0 3 5 7 8))


(define (open-spaces board)
  (filter (λ (n) (string=? (list-ref board n) "")) (build-list 9 identity)))





(define (ai-move ws)
  (local [
          (define open-spaces-local
            (open-spaces (ws-board ws)))
          (define (next-moves player)
            (next-boards (ws-board ws) player))
          (define (random-move board)
            (player-move ws (list-ref open-spaces-local
                                      (random (length open-spaces-local))) "O"))
          (define (winning-move? index player)
            (string=? (next-player
                       (list-ref (next-moves player) index) player
                       (list-ref open-spaces-local index))
                      (string-append "_" player)))
          (define (winning-list player)
            (map (λ (i) (list-ref open-spaces-local i))
                 (filter (λ (i) (winning-move? i player))
                         (build-list (length open-spaces-local) identity))))
          (define wins-list (winning-list "O"))
          (define x-wins (winning-list "X"))]
    (if (string=? (substring (ws-turn ws) 0 1) "_")
        ws
        (cond [(= (ws-ai ws) 0) (random-move (ws-board ws))]
              [(= (ws-ai ws) 1)
               (if (empty? wins-list)
                   (random-move (ws-board ws))
                   (player-move ws (first wins-list) "O"))]
              [(= (ws-ai ws) 2)
               (cond [(not (empty? wins-list))
                      (player-move ws (first wins-list) "O")]
                     [(not (empty? x-wins)) (player-move ws (first x-wins) "O")]
                     [else (random-move (ws-board ws))])]))))

(check-random
 (ai-move START)
 (player-move START
              
              (list-ref (open-spaces (ws-board START))
                        (random (length
                                 (open-spaces
                                  (ws-board START))))) "O"))
(check-random
 (ai-move
  (make-ws (ws-board START) (ws-turn START) 2)) 
 (player-move
  (make-ws (ws-board START) (ws-turn START) 2)
  (list-ref (open-spaces (ws-board START))
            (random (length
                     (open-spaces
                      (ws-board
                       START)))))
  "O"))
(check-random
 (ai-move (make-ws (ws-board START) (ws-turn START) 1))
 
 (player-move
  (make-ws (ws-board START)
           (ws-turn START)
           1) (list-ref (open-spaces
                         (ws-board START))
                        (random (length
                                 (open-spaces
                                  (ws-board
                                   START)))))
              "O"))
(check-expect (ai-move WS8) (player-move WS8 6 "O")) 
(check-expect (ai-move WS9) (player-move WS9 8 "O")) 
(check-expect (ai-move (make-ws (ws-board WS9) (ws-turn WS9) 2))
              (player-move (make-ws (ws-board WS9) (ws-turn WS9) 2) 8 "O"))
(check-expect (ai-move (make-ws (ws-board WS8) (ws-turn WS8) 2))
              (player-move (make-ws (ws-board WS8) (ws-turn WS8) 2) 6 "O"))

(check-expect (ai-move WS1) (player-move WS1 7 "O")) 







(check-expect (next-boards (ws-board START) "X")
              (list (list-set (ws-board START) 0 "X") 
                    (list-set (ws-board START) 1 "X")
                    (list-set (ws-board START) 2 "X")
                    (list-set (ws-board START) 3 "X")
                    (list-set (ws-board START) 4 "X")
                    (list-set (ws-board START) 5 "X")
                    (list-set (ws-board START) 6 "X")
                    (list-set (ws-board START) 7 "X")
                    (list-set (ws-board START) 8 "X")))

(check-expect (next-boards (ws-board WS6) "X")
              (list (list-set (ws-board WS6) 0 "X") 
                    (list-set (ws-board WS6) 3 "X")
                    (list-set (ws-board WS6) 5 "X")
                    (list-set (ws-board WS6) 7 "X")
                    (list-set (ws-board WS6) 8 "X")))
              
(check-expect (next-boards (ws-board WS7) "O")
              (list (list-set (ws-board WS7) 3 "O") 
                    (list-set (ws-board WS7) 4 "O")
                    (list-set (ws-board WS7) 5 "O")
                    (list-set (ws-board WS7) 6 "O")
                    (list-set (ws-board WS7) 7 "O")
                    (list-set (ws-board WS7) 8 "O")))

(define (next-boards board player)
  (local [(define open (open-spaces board))]
    (build-list (length open)
                (λ (i) (list-set board (list-ref open i) player)))))







(check-expect (handle-key START "a") START) 
(check-expect (handle-key WS2 "0") (make-ws (ws-board WS2) (ws-turn WS2) 0))

(check-expect (handle-key WS3 "1") (make-ws (ws-board WS3) (ws-turn WS3) 1))

(check-expect (handle-key WS4 "2") (make-ws (ws-board WS4) (ws-turn WS4) 2))


(define (handle-key ws key)
  (local [(define number (string->number key))]
    (if (number? number)
        (make-ws (ws-board ws) (ws-turn ws) (min number 2))
        ws)))



(define (main ws)
  (big-bang ws
    (to-draw render)
    (on-mouse handle-mouse)
    (on-key handle-key)))