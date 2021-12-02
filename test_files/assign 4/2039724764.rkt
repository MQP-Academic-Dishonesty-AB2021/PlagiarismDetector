

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment4_final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))








(define MIN-SIZE 300) 


(define LINE-COLOR "goldenrod") 
(define LINE-THICKNESS (/ SIZE 30)) 
(define LINE-INC (/ SIZE 3)) 
(define PEN (make-pen LINE-COLOR LINE-THICKNESS 
                      "solid"
                      "round"
                      "round")) 


(define PADDING (/ SIZE 20)) 
(define CENTER (/ SIZE 2)) 


(define TEXT-SCALE (/ SIZE MIN-SIZE)) 
(define TEXT-SIZE  (/ MIN-SIZE 3)) 
(define PLACEMENT (/ (- SIZE (* 2 PADDING)) 6)) 

(define POS1 (+ PADDING PLACEMENT)) 
(define POS3 (- SIZE PADDING PLACEMENT)) 


(define BOARD-LINES (add-line (add-line (add-line (add-line MTS
                                                            PADDING LINE-INC
                                                            (- SIZE PADDING) LINE-INC
                                                            PEN) 
                                                  
                                                  PADDING (- SIZE LINE-INC)
                                                  (- SIZE PADDING) (- SIZE LINE-INC)
                                                  PEN) 
                                        
                                        LINE-INC PADDING
                                        LINE-INC (- SIZE PADDING)
                                        PEN) 
                              
                              (- SIZE  LINE-INC) PADDING 
                              (- SIZE  LINE-INC) (- SIZE PADDING)
                              PEN)) 






(define X-IMG (scale TEXT-SCALE (text "X" TEXT-SIZE "red")))

(define O-IMG (scale TEXT-SCALE (text "O" TEXT-SIZE "blue")))






(define DRAW (scale TEXT-SCALE (text "It's a Draw" (/ TEXT-SIZE 2.5) "green")))

(define X-WINS (scale TEXT-SCALE (text "X Player Wins!" (/ TEXT-SIZE 2.5) "green")))

(define O-WINS (scale TEXT-SCALE (text "O Player Wins!" (/ TEXT-SIZE 2.5) "green"))) 





(define POSITION (list 0 1 2 
                       3 4 5
                       6 7 8))


(define ROWS (list (list 0 1 2) 
                   (list 3 4 5)
                   (list 6 7 8)))


(define COLUMNS (list (list 0 3 6) 
                      (list 1 4 7)
                      (list 2 5 8)))


(define DIAGONALS (list (list 0 4 8) 
                        (list 2 4 6)))


(define SOLUTIONS (append ROWS 
                          COLUMNS
                          DIAGONALS))


(define X 0)
(define O 1)


(define MINVAL -10000) 
(define MAXVAL 10000) 










(define EMPTY (build-list 9 (λ (n) false))) 

(define BOARD1 (list X #f #f 
                     #f #f #f
                     #f #f #f)) 

(define BOARD2 (list #f #f O 
                     #f X #f
                     #f #f #f))

(define BOARD3 (list #f #f X 
                     #f X #f 
                     X O O))

(define BOARD4 (list O O O 
                     X #f X 
                     #f X #f))

(define BOARD5 (list O  O  X 
                     #f #f X
                     #f #f X))

(define BOARD6 (list X  X  X 
                     #f #f #f
                     #f #f #f))

(define BOARD7 (list O  O  O 
                     #f #f #f
                     #f #f #f))

(define BOARD8 (list X  #f #f 
                     #f X  #f
                     #f #f X))

(define BOARD9 (list O  #f #f 
                     #f O  #f
                     #f #f O))

(define BOARD10 (list #f #f #f 
                      X  X  X
                      #f #f #f))
(define BOARD11 (list #f #f #f 
                      O  O  O
                      #f #f #f))

(define BOARD12 (list X  #f #f 
                      #f #f #f
                      X  #f X))

(define BOARD13 (list X #f  X 
                      #f #f #f
                      #f #f #f))

(define DRAW1 (list O O X 
                    X X O
                    O X X))

(define DRAW2 (list O  X  O  
                    O  O  X 
                    X  O  X))
(define DRAW3 (list X  O  X 
                    X  X  O 
                    O  X  O))











(define-struct ws (board player difficulty game-over?))







 



















(define START (make-ws EMPTY X 0 false))









(define (main ws)
  (big-bang ws
    (to-draw render)
    (on-key handle-key)
    (on-mouse add-move)))








(check-expect (render START) BOARD-LINES)          

(check-expect (render (make-ws DRAW3 X 0 true))    
              (place-image DRAW                 
                           CENTER
                           CENTER
                           (place-moves DRAW3)))

(check-expect (render (make-ws BOARD3 X 0 true))   
              (place-image X-WINS            
                           CENTER
                           CENTER
                           (place-moves BOARD3)))

(check-expect (render (make-ws BOARD11 X 0 true))       
              (place-image O-WINS                
                           CENTER
                           CENTER
                           (place-moves BOARD11)))

(check-expect (render (make-ws BOARD1 O 0 false)) 
              (place-moves BOARD1))         


(define (render ws)
  (if (ws-game-over? ws)
      (local [(define result (eval (ws-board ws)))
              (define winner (cond [(= result 0) DRAW]
                                   [(> result 0) X-WINS]
                                   [(< result 0) O-WINS]))]
        (place-image winner
                     CENTER
                     CENTER
                     (place-moves (ws-board ws))))
      (place-moves (ws-board ws))))











(check-expect (place-moves EMPTY) BOARD-LINES) 
(check-expect (place-moves BOARD1) (place-image X-IMG
                                                POS1
                                                POS1
                                                BOARD-LINES))

(check-expect (place-moves BOARD2) (place-image O-IMG 
                                                POS3
                                                POS1
                                                (place-image X-IMG
                                                             CENTER
                                                             CENTER
                                                             BOARD-LINES)))

(check-expect (place-moves BOARD9) 
              (place-image O-IMG
                           POS1
                           POS1
                           (place-image O-IMG
                                        CENTER
                                        CENTER
                                        (place-image O-IMG
                                                     POS3
                                                     POS3
                                                     BOARD-LINES))))

(define (place-moves board)
  (local [(define (place-moves-in board pos-counter)
            (local [(define (get-pos pos CorR)
                      (cond [(member? pos (first CorR))
                             POS1]
                            [(member? pos (list-ref CorR 1))
                             CENTER]
                            [(member? pos (list-ref CorR 2))
                             POS3]))]
                   
              (cond [(empty? board) BOARD-LINES]
                    [else
                     (if (not (false? (first board)))
                         (local [(define img 
                                   (if (= X (first board))
                                       X-IMG
                                       O-IMG))]
                           (place-image img
                                        (get-pos pos-counter COLUMNS)
                                        (get-pos pos-counter ROWS)
                                        (place-moves-in (rest board) (add1 pos-counter))))
                         (place-moves-in (rest board) (add1 pos-counter)))])))]
    (place-moves-in board 0)))










(check-expect (handle-key START "0") (make-ws EMPTY
                                              X
                                              (string->number "0")
                                              false))
(check-expect (handle-key START "1") (make-ws EMPTY
                                              X
                                              (string->number "1")
                                              false))
(check-expect (handle-key START "2") (make-ws EMPTY
                                              X
                                              (string->number "2")
                                              false))
(check-expect (handle-key START "9") (make-ws EMPTY
                                              X
                                              (string->number "9")
                                              false))
(check-expect (handle-key START "a") START)

(define (handle-key ws key)
  (local [(define num (string->number key))]
    (if (number? num)                         
        (make-ws (ws-board ws)
                 (ws-player ws)
                 num
                 (ws-game-over? ws))
        ws)))













(check-expect (add-move (make-ws (cons #f (rest BOARD13)) X 9 false)
                        POS1
                        POS1
                        "button-down")
              (make-ws (ai-move BOARD13 O 9)
                       X
                       9
                       false))


(check-expect (add-move (make-ws (list X O #f
                                       #f #f #f
                                       #f #f #f) X 0 false)
                        (- LINE-INC 10)
                        ( - LINE-INC 10)
                        "button-down")
              (make-ws (list X O #f 
                             #f #f #f
                             #f #f #f)
                       X
                       0
                       false))


(check-expect (add-move (make-ws BOARD1 O 0 false) LINE-INC LINE-INC "button-down")
              (make-ws BOARD1 O 0 false))


(check-expect (add-move (make-ws BOARD3 O 4 true) LINE-INC LINE-INC "button-down")
              (make-ws BOARD3 O 4 true))


(define (add-move ws x y click)
  (if (and (string=? click "button-down")
           (not (ws-game-over? ws))
           (= (ws-player ws) X))
      (local [(define pos (find-position x y))]
        (if (not-filled? (ws-board ws) pos)
            (local [(define new-ws (game-state ws (set-val (ws-board ws)
                                                           (ws-player ws)
                                                           pos)))]
              (if (and (= (ws-player new-ws) O)
                       (not (ws-game-over? new-ws)))
                  (game-state new-ws
                              (ai-move (ws-board new-ws)
                                       (ws-player new-ws)
                                       (ws-difficulty new-ws)))
                  new-ws))
            ws))
      ws))











(check-expect (game-state START BOARD1) 
              (make-ws BOARD1 O 0 false))

(check-expect (game-state (make-ws BOARD13 
                                   X
                                   9
                                   false)
                          BOARD6)
              (make-ws BOARD6 O 9 true))

(check-expect (game-state (make-ws BOARD2 X 2 false) BOARD13) 
              (make-ws BOARD13 O 2 false))

(define (game-state ws new-board)
  (make-ws new-board
           (next-player (ws-player ws))
           (ws-difficulty ws)
           (not (false? (eval new-board)))))











(check-expect (not-filled? EMPTY 5) true)
(check-expect (not-filled? BOARD1 0) false)
(check-expect (not-filled? BOARD2 6) true)
(check-expect (not-filled? BOARD2 4) false)
(check-expect (not-filled? DRAW1 2) false)

(define (not-filled? board pos)
  (false? (list-ref board pos)))












(check-expect (find-position (- LINE-INC 10)        (- LINE-INC 10)) 0)
(check-expect (find-position LINE-INC               (- LINE-INC 10)) 1)
(check-expect (find-position (- SIZE LINE-INC -10)  (- LINE-INC 10)) 2)
(check-expect (find-position (- LINE-INC 10)        LINE-INC)        3)
(check-expect (find-position LINE-INC               LINE-INC)        4)
(check-expect (find-position (- SIZE LINE-INC -10)  LINE-INC)        5)
(check-expect (find-position (- LINE-INC 10)        (- SIZE LINE-INC -10)) 6)
(check-expect (find-position LINE-INC               (- SIZE LINE-INC -10)) 7)
(check-expect (find-position (- SIZE LINE-INC -10)  (- SIZE LINE-INC -10)) 8)

(define (find-position x y)
  (local [(define (find-c-r val CorR)
            (cond [(< val LINE-INC)           0]    
                  
                  [(< val (- SIZE                   
                             LINE-INC
                             LINE-THICKNESS)) 1]    
                  
                  [else                       2]))  

          (define COLUMN (find-c-r x COLUMNS))
          (define ROW (find-c-r y ROWS))]
    
    (+ (* ROW 3) COLUMN)))











(check-expect (next-player X) O)
(check-expect (next-player O) X)

(define (next-player player)
  (if (= player X)
      O
      X ))











(define (eval board)
  (local [
          (define (match? indices key)
            (andmap (λ (x) (eq? key (list-ref board x))) indices))
          (define (anymatch? key)
            (ormap (λ (x) (match? x key)) SOLUTIONS))]
    
    (cond [(anymatch? X) 1]
          [(anymatch? O) -1]
          [((λ (n) (andmap number? n)) board) 0]
          [else #f])))


(check-expect (eval EMPTY) #f)
(check-expect (eval BOARD6) 1)
(check-expect (eval BOARD7) -1)
(check-expect (eval DRAW2) 0)
(check-expect (eval DRAW3) 0)
(check-expect (eval BOARD8) 1)
(check-expect (eval BOARD9) -1)
(check-expect (eval BOARD10) 1)
(check-expect (eval BOARD11) -1)
(check-expect (eval BOARD12) #f)
(check-expect (eval BOARD13) #f)







(define (player-fn player)
  (if (= player X) max min))









(define (minimax board player depth)
  (local [
          (define (minimax--board b player depth)
            (local [(define s (eval b))]
              (cond [(number? s) s]
                    [(<= depth 0) 0]
                    [else (minimax--lob (next-boards b player)
                                        player
                                        depth)])))
          (define (minimax--lob lob player depth)
            (apply (player-fn player)
                   (map (λ (x) (minimax--board x (next-player player) (- depth 1)))
                        lob)))]
    (minimax--board board player depth)))

(check-expect (minimax EMPTY  X 1)  0)
(check-expect (minimax BOARD6  X 0)  1)
(check-expect (minimax BOARD6  X 1)  1)
(check-expect (minimax BOARD7  X 0) -1)
(check-expect (minimax BOARD12  O 1)  0)
(check-expect (minimax BOARD12  O 2)  1)
(check-expect (minimax BOARD12  X 1)  1)
(check-expect (minimax BOARD13 O 1)  0)
(check-expect (minimax BOARD13 O 2)  0)
(check-expect (minimax BOARD13 X 1)  1)














(define (minimax-ab board player depth)
  (local [
          (define (minimax--board b player depth alpha beta)
            (local [(define s (eval b))]
              (cond [(number? s) s]
                    [(<= depth 0) 0]
                    [else (minimax--lob (next-boards b player)
                                        player
                                        depth
                                        alpha
                                        beta)])))
          (define (minimax--lob lob player depth alpha beta)
            (cond [(empty? lob)
                   
                   ((player-fn (next-player player)) MINVAL MAXVAL)] 
                  [else
                   (local [ 
                           (define s (minimax--board (first lob)
                                                     (next-player player)
                                                     (- depth 1)
                                                     alpha
                                                     beta))
                           
                           (define new-alpha (if (= 0 player) 
                                                 (max alpha s)
                                                 alpha))
                           
                           (define new-beta (if (= 1 player) 
                                                (min beta s)
                                                beta))]
                     
                     (if (<= new-beta new-alpha)
                         
                         s
                         
                         ((player-fn player)
                          s
                          (minimax--lob (rest lob)
                                        player
                                        depth
                                        new-alpha
                                        new-beta))))]))]
    (minimax--board board player depth MINVAL MAXVAL)))

(check-expect (minimax-ab EMPTY 0 1) 0)
(check-expect (minimax-ab BOARD6 0 0) 1)
(check-expect (minimax-ab BOARD6 0 1) 1)
(check-expect (minimax-ab BOARD7 0 0) -1)
(check-expect (minimax-ab BOARD12 1 1) 0)
(check-expect (minimax-ab BOARD12 1 2) 1)
(check-expect (minimax-ab BOARD12 0 1) 1)
(check-expect (minimax-ab BOARD13 1 1) 0)
(check-expect (minimax-ab BOARD13 1 2) 0)
(check-expect (minimax-ab BOARD13 0 1) 1)
















(define (set-val ls val index)
  (cond [(= 0 index) (cons val (rest ls))]
        [else (cons (first ls)
                    (set-val (rest ls) val (- index 1)))]))





(check-expect (set-val (list 0 1 2) 5 0)
              (list 5 1 2))
(check-expect (set-val (list 0 1 2) 5 1)
              (list 0 5 2))
(check-expect (set-val EMPTY X 0) (cons X (rest EMPTY)))
(check-expect (set-val EMPTY X 1) (cons (first EMPTY) (cons X (rest (rest EMPTY))))) 
(check-expect (set-val EMPTY O 8) (list #f #f #f
                                        #f #f #f
                                        #f #f O)) 
(check-expect (set-val EMPTY X 4) (list #f #f #f
                                        #f X  #f
                                        #f #f #f))





(check-expect (next-boards BOARD12 O) 
              (list
               (list X O #f
                     #f #f #f
                     X  #f X)
               (list X  #f O
                     #f #f #f
                     X  #f X)
               (list X  #f #f
                     O #f #f
                     X  #f X)
               (list X  #f #f
                     #f O #f
                     X  #f X)
               (list X  #f #f
                     #f #f O
                     X  #f X)
               (list X  #f #f
                     #f #f #f
                     X  O X)))

(check-expect (next-boards BOARD12 O) 
              (list
               (list X O #f
                     #f #f #f
                     X  #f X)
               (list X  #f O
                     #f #f #f
                     X  #f X)
               (list X  #f #f
                     O #f #f
                     X  #f X)
               (list X  #f #f
                     #f O #f
                     X  #f X)
               (list X  #f #f
                     #f #f O
                     X  #f X)
               (list X  #f #f
                     #f #f #f
                     X  O X)))


(check-expect (next-boards (list O X  O
                                 X #f X 
                                 X X  O) O)

              (list (list O X O
                          X O X 
                          X X O)))


(check-expect (next-boards DRAW1 O) empty)

(define (next-boards board player)
  (local
    [(define (next-boards-i board player index)
       (cond [(> index 8) empty]
             [(not-filled? board index)
              (cons (set-val board player index)
                    (next-boards-i board player (+ 1 index)))]
             [else (next-boards-i board player (+ 1 index))]))]
    (next-boards-i board player 0)))







(define (ai-move board player depth)
  (local [(define options (next-boards board player))]
    (cond [(= depth 0) (list-ref options (random (length options)))]
          [else ((if (= player X) argmax argmin)
                 (λ (x) (minimax-ab x (- 1 player) (- depth 1)))
                 options)])))

(check-expect (ai-move BOARD13 0 3)
              (list 0 0  0
                    #f #f #f
                    #f #f #f))
(check-expect (ai-move BOARD13 1 3)
              (list 0 1  0
                    #f #f #f
                    #f #f #f))