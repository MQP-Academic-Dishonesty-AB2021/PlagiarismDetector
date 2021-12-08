

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Kai Nakamura and Parker Frizzle Assignment 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))







(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))


(define CENTER (/ SIZE 2))
(define SQUARE-SIZE (/ SIZE 3))

(define GRID-PEN (make-pen "red"
                           (round (/ SQUARE-SIZE 10))
                           "solid"
                           "round"
                           "bevel"))

(define SYMBOL-SIZE (round (/ SQUARE-SIZE 2)))
(define SYMBOL-COLOR "black")

(define WIN-TEXT-SIZE (round (* SQUARE-SIZE 0.4)))
(define WIN-TEXT-COLOR "green")

(define DIFFICULTY-TEXT-SIZE (round (* SQUARE-SIZE 0.1)))
(define DIFFICULTY-TEXT-COLOR "black")









(define B " ")
(define X "X")
(define O "O")

(define B-IMAGE (empty-scene 0 0))
(define X-IMAGE (text X SYMBOL-SIZE SYMBOL-COLOR))
(define O-IMAGE (text O SYMBOL-SIZE SYMBOL-COLOR))

 










(define EMPTY-SQUARES (list B B B
                            B B B
                            B B B))
 





(define-struct player (name symbol))

(define HUMAN (make-player "Human" X))
(define COMPUTER (make-player "Computer" O))

 










(define DRAW "Draw")

 







(define-struct board (squares player difficulty win-state))

(define START (make-board EMPTY-SQUARES HUMAN 0 false))

 











(define ROWS
  (list (list 0 1 2)
        (list 3 4 5)
        (list 6 7 8)))

(define COLS
  (list (list 0 3 6)
        (list 1 4 7)
        (list 2 5 8)))

(define DIAGONALS
  (list (list 0 4 8)
        (list 2 4 6)))








(define (main board)
  (big-bang board           
    (on-tick update)        
    (to-draw render)        
    (on-mouse handle-mouse) 
    (on-key handle-key)))   





(define (update board)
  (local [
          
          
          
          (define (take-computer-turn board)
            (cond [(= 0 (board-difficulty board))
                   (take-random-computer-turn board)]
                  [else
                   (take-smart-computer-turn board (board-difficulty board))]))]
    (cond [(string=? (player-name COMPUTER)
                     (player-name (board-player board)))
           (take-computer-turn board)]
          [else
           board])))





(check-expect (image-width (render START)) SIZE)
(check-expect (image-height (render START)) SIZE)

(define (render board)
  (local
    [
     
     
     
     (define (render-win-state win-state scene)
       (local [(define (get-text win-state)
                 (cond [(false? win-state) ""]
                       [(and (string? win-state)
                             (string=? DRAW win-state)) "Draw!"]
                       [else
                        (string-append (player-name win-state) " Wins!")]))]
         (place-image (text (get-text win-state)
                            WIN-TEXT-SIZE
                            WIN-TEXT-COLOR)
                      CENTER CENTER scene)))

     
     
     
     (define (render-difficulty difficulty scene)
       (place-image/align (text (string-append "Difficulty: "
                                               (number->string difficulty))
                                DIFFICULTY-TEXT-SIZE
                                DIFFICULTY-TEXT-COLOR)
                          0 0 "left" "top" scene))

     
     
     
     (define (render-grid scene)
       (local
         [(define (add-vertical-line x scene)
            (add-line scene x 0 x SIZE GRID-PEN))
          (define (add-horizontal-line y scene)
            (add-line scene 0 y SIZE y GRID-PEN))]
         (add-horizontal-line
          SQUARE-SIZE
          (add-horizontal-line
           (* SQUARE-SIZE 2)
           (add-vertical-line
            SQUARE-SIZE
            (add-vertical-line
             (* SQUARE-SIZE 2)
             scene))))))]
    (render-win-state (board-win-state board)
                      (render-difficulty (board-difficulty board)
                                         (render-squares (board-squares board)
                                                         (render-grid MTS))))))





(check-expect (handle-mouse START 0 0 "button-up") START)
(check-expect (handle-mouse START 0 0 "button-down")
              (make-board (list X B B
                                B B B
                                B B B)
                          COMPUTER
                          (board-difficulty START)
                          false))
(check-expect (handle-mouse START SQUARE-SIZE 0 "button-down")
              (make-board (list B X B
                                B B B
                                B B B)
                          COMPUTER
                          (board-difficulty START)
                          false))
(check-expect (handle-mouse START SIZE 0 "button-down")
              (make-board (list B B X
                                B B B
                                B B B)
                          COMPUTER
                          (board-difficulty START)
                          false))
(check-expect (handle-mouse (make-board (list X B B
                                              B B B
                                              B B B)
                                        HUMAN
                                        (board-difficulty START)
                                        false) 0 0 "button-down")
              (make-board (list X B B
                                B B B
                                B B B)
                          HUMAN
                          (board-difficulty START)
                          false))

(define (handle-mouse board x y mouse-event)
  (cond [(mouse=? mouse-event "button-down")
         (local [
                 
                 
                 
                 
                 
                 
                 
                 (define (get-index-at-row-and-col row col)
                   (+ (* 3 col) row))
                 
                 
                 
                 
                 (define (pixels->grid pixels)
                   (cond [(< pixels SQUARE-SIZE) 0]
                         [(< pixels (* 2 SQUARE-SIZE)) 1]
                         [else 2]))
                 
                 
                 
                 (define (get-index-at-position x y)
                  
                   (get-index-at-row-and-col (pixels->grid x)
                                             (pixels->grid y)))
                 
                 
                 
                 (define (square-blank? squares index)
                   (string=? (list-ref squares index) B))
                 (define index-at-position (get-index-at-position x y))]
           (if (square-blank? (board-squares board) index-at-position)
               (take-turn board index-at-position)
               board))]
        [else board]))





(check-expect (handle-key START " ") START)
(check-expect (handle-key START "0") START)
(check-expect (handle-key START "9")
              (make-board EMPTY-SQUARES
                          (board-player START)
                          9
                          false))

(define (handle-key board key-event)
  (local [(define key-event-number (string->number key-event))]
    (cond [(false? key-event-number) board]
          [else
           (make-board (board-squares board)
                       (board-player board)
                       key-event-number
                       (board-win-state board))])))





(check-expect (take-turn START 0)
              (make-board (set-square (board-squares START)
                                      0
                                      (player-symbol (board-player START)))
                          (get-next-player (board-player START))
                          (board-difficulty START)
                          (board-win-state START)))
(check-expect (take-turn START 1)
              (make-board (set-square (board-squares START)
                                      1
                                      (player-symbol (board-player START)))
                          (get-next-player (board-player START))
                          (board-difficulty START)
                          (board-win-state START)))
(check-expect (take-turn (make-board (list X O O
                                           O X X
                                           X O O)
                                     HUMAN
                                     0
                                     false)
                         1)
              (make-board (list X O O
                                O X X
                                X O O)
                          HUMAN
                          0
                          DRAW))
          
(define (take-turn board index)
  (local [(define win-state (get-win-state board))]
    (if (false? win-state) 
        (local [(define next-board 
                  (make-board (set-square (board-squares board)
                                          index
                                          (player-symbol (board-player board)))
                              (get-next-player (board-player board))
                              (board-difficulty board)
                              false))]
          (make-board (board-squares next-board) 
                      (board-player next-board)
                      (board-difficulty board)
                      (get-win-state next-board)))
        (make-board (board-squares board)
                    (board-player board)
                    (board-difficulty board)
                    win-state)))) 





(check-expect (length
               (get-empty-squares
                (board-squares (take-random-computer-turn START)))) 8)
(check-expect (board-player
               (take-random-computer-turn
                (make-board EMPTY-SQUARES COMPUTER 0 false))) HUMAN)
(check-expect (take-random-computer-turn (make-board (list X X X
                                                           X X X
                                                           X X X)
                                                     COMPUTER
                                                     0
                                                     HUMAN))
              (make-board (list X X X
                                X X X
                                X X X)
                          COMPUTER
                          0
                          HUMAN))

(define (take-random-computer-turn board)
  (local [(define empty-squares (get-empty-squares (board-squares board)))
          (define computer-move
            (cond [(empty? empty-squares) 0]
                  [else
                   (list-ref empty-squares
                             (random (length empty-squares)))]))]
    (take-turn board computer-move)))





(check-expect (take-smart-computer-turn (make-board (list O X O
                                                          X O X
                                                          X B B)
                                                    COMPUTER
                                                    0
                                                    false)
                                        1)
              (make-board (list O X O
                                X O X
                                X B O)
                          HUMAN
                          0
                          COMPUTER))
(check-expect (take-smart-computer-turn (make-board (list O B X
                                                          X X B
                                                          O B B)
                                                    COMPUTER
                                                    0
                                                    false)
                                        2)
              (make-board (list O B X
                                X X O
                                O B B)
                          HUMAN
                          0
                          false))
(check-expect (take-smart-computer-turn (make-board (list X X O
                                                          O O X
                                                          X X B)
                                                    COMPUTER
                                                    0
                                                    false)
                                        1)
              (make-board (list X X O
                                O O X
                                X X O)
                          HUMAN
                          0
                          DRAW))

(define (take-smart-computer-turn board depth0)
  (local [
          (define-struct board-value-pair (board value))

          
          
          
          (define (evaluate-position win-state)
            (cond [(or (false? win-state)
                       (and (string? win-state)
                            (string=? DRAW win-state)))
                   0]
                  [(player? win-state)
                   (if (string=? (player-name HUMAN)
                                 (player-name win-state))
                       1
                       -1)]))
          
          
          (define (take-smart-computer-turn board depth)
            (cond [(or (= 0 depth) 
                       (not (false? (board-win-state board)))) 
                   (make-board-value-pair board
                                          (evaluate-position
                                           (board-win-state board)))]
                  [else
                   ((if (string=? (player-name HUMAN)
                                  (player-name (board-player board)))
                        argmax 
                        argmin) 
                    board-value-pair-value
                    
                    (map
                     
                     (lambda (next-board)
                       (make-board-value-pair next-board
                                              (board-value-pair-value
                                               (take-smart-computer-turn
                                                next-board
                                                (sub1 depth)))))
                     
                     (get-possible-moves board)))]))]
    (board-value-pair-board (take-smart-computer-turn board depth0))))





(check-expect (get-possible-moves (make-board (list O O X
                                                    X X O
                                                    O O X)
                                              COMPUTER
                                              0
                                              false))
              empty)
(check-expect (get-possible-moves (make-board (list X X X
                                                    X X X
                                                    X X X)
                                              COMPUTER
                                              0
                                              HUMAN))
              empty)
(check-expect (get-possible-moves (make-board (list O O X
                                                    X X O
                                                    O O B)
                                              HUMAN
                                              0
                                              false))
              (list (make-board (list O O X
                                      X X O
                                      O O X)
                                COMPUTER
                                0
                                DRAW)))
(check-expect (get-possible-moves (make-board (list X O X
                                                    O X O
                                                    O O B)
                                              HUMAN
                                              0
                                              false))
              (list (make-board (list X O X
                                      O X O
                                      O O X)
                                COMPUTER
                                0
                                HUMAN)))
(check-expect (get-possible-moves (make-board (list O O X
                                                    X X O
                                                    O B B)
                                              COMPUTER
                                              0
                                              false))
              (list (make-board (list O O X
                                      X X O
                                      O O B)
                                HUMAN
                                0
                                false)
                    (make-board (list O O X
                                      X X O
                                      O B O)
                                HUMAN
                                0
                                false)))

(define (get-possible-moves board)
  (local [(define (get-possible-moves squares)
            (cond [(empty? squares) empty]
                  [else
                   (cons (take-turn board (first squares))
                         (get-possible-moves (rest squares)))]))]
    (if (false? (board-win-state board)) 
        (get-possible-moves (get-empty-squares (board-squares board))) 
        empty))) 





(check-expect (get-empty-squares EMPTY-SQUARES) (list 0 1 2 3 4 5 6 7 8))
(check-expect (get-empty-squares (list X X X
                                       X X X
                                       X X X)) empty)
(check-expect (get-empty-squares (list X B B
                                       B B B
                                       B B B)) (list 1 2 3 4 5 6 7 8))

(define (get-empty-squares squares)
  (indexes-where squares (lambda (square)
                           (string=? square B))))





(check-expect (set-square EMPTY-SQUARES 0 B) EMPTY-SQUARES)
(check-expect (set-square EMPTY-SQUARES 0 X) (list X B B
                                                   B B B
                                                   B B B))
(check-expect (set-square EMPTY-SQUARES 4 O) (list B B B
                                                   B O B
                                                   B B B))
(check-expect (set-square (list X X X
                                X X X
                                X X X)
                          4 O)
              (list X X X
                    X X X
                    X X X))

(define (set-square squares index square)
  (local [(define (set-square squares index square i)
            (cond [(empty? squares) empty]
                  [(= i index)
                   (cons square
                         (set-square (rest squares) index square (add1 i)))]
                  [else
                   (cons
                    (first squares)
                    (set-square (rest squares) index square (add1 i)))]))]
    (if (string=? B (list-ref squares index))
        (set-square squares index square 0) 
        squares))) 





(check-expect (render-squares EMPTY-SQUARES MTS) MTS)
(check-expect (render-squares (list X O X
                                    O X O
                                    X O X) MTS)
              (render-squares-at X (list 0 2 4 6 8)
                                 (render-squares-at O (list 1 3 5 7) MTS)))

(define (render-squares squares scene)
  (render-squares-at X (indexes-of squares X)
                     (render-squares-at O (indexes-of squares O) scene)))





(check-expect (render-squares-at B empty MTS) MTS)
(check-expect (render-squares-at B (list 0 1 2) MTS) MTS)
(check-expect (render-squares-at X (list 0 1 2) MTS)
              (render-square X 0 (render-square X 1 (render-square X 2 MTS))))
(check-expect (render-squares-at O (list 0 1 2) MTS)
              (render-square O 0 (render-square O 1 (render-square O 2 MTS))))

(define (render-squares-at square indicies scene)
  (cond [(empty? indicies) scene]
        [else
         (render-square square (first indicies)
                        (render-squares-at square
                                           (rest indicies)
                                           scene))]))





(check-expect (render-square B 0 MTS) MTS)
(check-expect (render-square X 4 MTS)
              (place-image (get-square-image X)
                           (index->x 4)
                           (index->y 4)
                           MTS))
(check-expect (render-square O 4 MTS)
              (place-image (get-square-image O)
                           (index->x 4)
                           (index->y 4)
                           MTS))

(define (render-square square index scene)
  (place-image (get-square-image square)
               (index->x index)
               (index->y index)
               scene))





(check-expect (get-square-image B) B-IMAGE)
(check-expect (get-square-image X) X-IMAGE)
(check-expect (get-square-image O) O-IMAGE)

(define (get-square-image square)
  (cond [(string=? B square) B-IMAGE]
        [(string=? X square) X-IMAGE]
        [(string=? O square) O-IMAGE]))





(check-expect (index->x 0) (grid->pixels 0))
(check-expect (index->x 1) (grid->pixels 1))
(check-expect (index->x 2) (grid->pixels 2))

(define (index->x index)
  (local [
          
          
          
          (define (index->col index)
            (modulo index 3))]
    (grid->pixels (index->col index))))





(check-expect (index->y 0) (grid->pixels 0))
(check-expect (index->y 3) (grid->pixels 1))
(check-expect (index->y 6) (grid->pixels 2))

(define (index->y index)
  (local [
          
          
          
          (define (index->row index)
            (floor (/ index 3)))]
    (grid->pixels (index->row index))))





(check-expect (grid->pixels 0) (/ SQUARE-SIZE 2))
(check-expect (grid->pixels 1) (+ (* 1 SQUARE-SIZE) (/ SQUARE-SIZE 2)))
(check-expect (grid->pixels 2) (+ (* 2 SQUARE-SIZE) (/ SQUARE-SIZE 2)))

(define (grid->pixels grid)
  (+ (* grid SQUARE-SIZE)
     (/ SQUARE-SIZE 2)))





(check-expect (get-next-player HUMAN) COMPUTER)
(check-expect (get-next-player COMPUTER) HUMAN)
(check-error (get-next-player (make-player "" B))
             "Could not get next player of unknown player")

(define (get-next-player player)
  (cond [(string=? (player-name HUMAN) (player-name player)) COMPUTER]
        [(string=? (player-name COMPUTER) (player-name player)) HUMAN]
        [else (error "Could not get next player of unknown player")]))







(check-expect (get-win-state START) false)

(check-expect (get-win-state (make-board (list X O X
                                               O X O
                                               B B X)
                                         HUMAN
                                         0
                                         false)) HUMAN)

(check-expect (get-win-state (make-board (list O O O
                                               X O X
                                               B B X)
                                         HUMAN
                                         0
                                         false)) COMPUTER)

(check-expect (get-win-state (make-board (list X O O
                                               O X X
                                               X O O)
                                         HUMAN
                                         0
                                         false)) DRAW)

(define (get-win-state board)
  (cond [(player-won? board HUMAN) HUMAN]
        [(player-won? board COMPUTER) COMPUTER]
        [(empty? (get-empty-squares (board-squares board))) DRAW]
        [else false]))







(check-expect (player-won? START HUMAN) false)

(check-expect (player-won? (make-board (list X X X
                                             B B B
                                             B B B)
                                       HUMAN
                                       0
                                       false)
                           HUMAN) true)

(check-expect (player-won? (make-board (list O B B
                                             O B B
                                             O B B)
                                       COMPUTER
                                       0
                                       false)
                           COMPUTER) true)

(check-expect (player-won? (make-board (list X B B
                                             B X B
                                             B B X)
                                       HUMAN
                                       0
                                       false)
                           HUMAN) true)

(define (player-won? board player)
  (local [
          
          
          
          (define (player-filled-any-lines? lines)
            (lambda (squares player)
              (local [(define (line-filled? line)
                        (andmap (lambda (index)
                                  (string=?
                                   (list-ref squares index)
                                   (player-symbol player))) line))]
                (ormap line-filled? lines))))]
    (or ((player-filled-any-lines? ROWS) (board-squares board) player)
        ((player-filled-any-lines? COLS) (board-squares board) player)
        ((player-filled-any-lines? DIAGONALS) (board-squares board) player))))