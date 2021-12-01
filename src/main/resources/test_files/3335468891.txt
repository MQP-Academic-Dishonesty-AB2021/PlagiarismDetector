

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |CS1102 Assignment 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 







(define SIZE 300) 
(define BG-COLOR "gainsboro")


(define X-IMAGE .)
(define O-IMAGE .)

(define MTS (empty-scene SIZE SIZE BG-COLOR))


(define LINE-THICC (/ SIZE 50))
(define SIZE-HALF (/ SIZE 2))

(define PADDING (* LINE-THICC 6))
(define TIC-TAC-OFFSET (* (- SIZE PADDING) 1/3))
(define LINE-OFFSET (* (- SIZE PADDING) 1/6))


(define SCALING-X (/ (- SIZE PADDING) 3 (image-height X-IMAGE)))
(define SCALING-O (/ (- SIZE PADDING) 3 (image-height O-IMAGE)))
(define X (scale SCALING-X X-IMAGE))
(define O (scale SCALING-O O-IMAGE))
(define E empty-image)


(define BOARD-LINE-PEN (make-pen "chocolate" (floor LINE-THICC) "solid" "round" "round"))
(define LINE-HORIZONTAL (line (- SIZE (* PADDING 1)) 0 BOARD-LINE-PEN)) 
(define LINE-VERTICAL (line 0 (- SIZE (* PADDING 1)) BOARD-LINE-PEN))

(define LINES (list LINE-HORIZONTAL LINE-HORIZONTAL
                    LINE-VERTICAL LINE-VERTICAL))
(define LINES-POS (list (make-posn SIZE-HALF (- SIZE-HALF LINE-OFFSET)) (make-posn SIZE-HALF (+ SIZE-HALF LINE-OFFSET))
                        (make-posn (- SIZE-HALF LINE-OFFSET) SIZE-HALF) (make-posn (+ SIZE-HALF LINE-OFFSET) SIZE-HALF)))

(define TIC-TAC-POS (list (make-posn (- SIZE-HALF TIC-TAC-OFFSET) (- SIZE-HALF TIC-TAC-OFFSET))
                          (make-posn SIZE-HALF (- SIZE-HALF TIC-TAC-OFFSET))
                          (make-posn (+ SIZE-HALF TIC-TAC-OFFSET) (- SIZE-HALF TIC-TAC-OFFSET))
                          
                          (make-posn (- SIZE-HALF TIC-TAC-OFFSET) SIZE-HALF)
                          (make-posn SIZE-HALF SIZE-HALF)
                          (make-posn (+ SIZE-HALF TIC-TAC-OFFSET) SIZE-HALF)
                          
                          (make-posn (- SIZE-HALF TIC-TAC-OFFSET) (+ SIZE-HALF TIC-TAC-OFFSET))
                          (make-posn SIZE-HALF (+ SIZE-HALF TIC-TAC-OFFSET))
                          (make-posn (+ SIZE-HALF TIC-TAC-OFFSET) (+ SIZE-HALF TIC-TAC-OFFSET))))

(define EMPTY-BOARD (place-images LINES LINES-POS MTS)) 




(define-struct board (player-turn game-won difficulty values))







 


(define values-empty (list 0 0 0
                           0 0 0
                           0 0 0))

(define values-player-won (list  1 2 1
                                 2 1 0
                                 1 0 2))

(define values-bot-won (list 1 0 2
                             1 2 0
                             2 0 1))

(define values-middle-game (list 0 2 2
                                 0 1 0
                                 1 0 0))

(define values-draw (list 2 1 1
                          1 1 2
                          2 2 1))


(define START (make-board true false 0 values-empty))
(define B-player (make-board true true 0 values-player-won))
(define B-bot (make-board false true 0 values-bot-won))
(define B-game (make-board true false 2 values-middle-game))
(define B-draw (make-board false false 0 values-draw))







(define (main bd)
  (big-bang bd
    (on-tick play)
    (to-draw render)
    (on-mouse handle-mouse)
    (on-key handle-key)))







(check-expect (play B-game) B-game) 
(check-expect (play START) START) 
(check-expect (play B-bot) B-bot) 
(check-expect (play B-draw) B-draw) 

(define (play board)
  (local [(define next-board (check-board board))]
    (cond [(board-game-won next-board) next-board]       
          [(board-player-turn next-board) next-board]    
          [(empty? (indexes-of (board-values next-board) 0)) next-board] 
          [else (bot-turn next-board)])))                   





(check-expect (check-board B-bot) (make-board false true 0 values-bot-won)) 
(check-expect (check-board B-player) (make-board true true 0 values-player-won)) 
(check-expect (check-board B-game) B-game) 

(define (check-board board)
  (local[(define (check-moves val number) 
           (or  (= (first val) (second val) (third val) number)
                (= (fourth val) (fifth val) (sixth val) number)
                (= (seventh val) (eighth val) (ninth val) number) 

                (= (first val) (fourth val) (seventh val) number)
                (= (second val) (fifth val) (eighth val) number)
                (= (third val) (sixth val) (ninth val) number) 
                
                (= (first val) (fifth val) (ninth val) number) 
                (= (third val) (fifth val) (seventh val) number)
                ))]
    
    (cond [(check-moves (board-values board) 1)
           (make-board true true (board-difficulty board) (board-values board))] 
          [(check-moves (board-values board) 2)
           (make-board false true (board-difficulty board) (board-values board))] 
          [else board])))
        




(define (render bd)
  (local[
         (define TEXT (text (cond [(and (board-game-won bd) (board-player-turn bd)) "X wins"]        
                                  [(and (board-game-won bd) (not (board-player-turn bd))) "O wins"]  
                                  [(empty? (indexes-of (board-values bd) 0)) "DRAW"]               
                                  [else ""])
                            (round (/ SIZE 5)) "green"))]
    (overlay TEXT (render-values (board-values bd))))) 





(check-expect (render-values values-empty) (place-images (list E E E
                                                               E E E
                                                               E E E)
                                                         TIC-TAC-POS EMPTY-BOARD))

(define (render-values values)
  (place-images (map (lambda (n) (cond [(= n 0) E]
                                       [(= n 1) X]
                                       [(= n 2) O])) values)
                TIC-TAC-POS EMPTY-BOARD))






(check-expect (handle-mouse B-game (* SIZE 1/6) (* SIZE 1/6) "button-down")
              (make-board #false #false 2 (list 1 2 2 0 1 0 1 0 0)))

(check-expect (handle-mouse START (* SIZE 5/6) (* SIZE 1/6) "button-down")
              (make-board #false #false 0 (list 0 0 1 0 0 0 0 0 0)))

(check-expect (handle-mouse B-draw (* SIZE 3/6) (* SIZE 3/6) "button-down") B-draw) 
              

(define (handle-mouse board mouse-x mouse-y m-event)
  (local [
          (define (find-third pos)       
            (cond [(<= pos (- SIZE-HALF LINE-OFFSET)) 0]
                  [(<= pos (+ SIZE-HALF LINE-OFFSET)) 1]
                  [(<= pos SIZE) 2]
                  [else pos]))
          
          (define box                    
            (+ (find-third mouse-x) (* (find-third mouse-y) 3)))            

          (define (player-turn board)    
            (make-board false false (board-difficulty board)
                        (insert-play (board-player-turn board) (board-values board) box)))

          (define (valid-move board)     
            (and (= (list-ref (board-values board) box) 0)
                 (not (board-game-won board))))]
   
    (cond [(and (mouse=? m-event "button-down") (valid-move board))
           (player-turn board)]
          [else board])))





(check-expect (handle-key B-game "0") (make-board true false 0 values-middle-game))
(check-expect (handle-key B-game "5") (make-board true false 5 values-middle-game))
(check-expect (handle-key B-game "9") (make-board true false 9 values-middle-game))
(check-expect (handle-key B-game "a") (make-board true false 2 values-middle-game))



(define (handle-key board key)
  (local [(define difficulty (string->number key))] 
    (if (equal? difficulty false) board
    
        (make-board (board-player-turn board)
                    (board-game-won board)
                    difficulty
                    (board-values board)))))







(check-expect (bot-turn (make-board #false #false 1 values-middle-game)) (make-board #true #false 1 (list 2 2 2 0 1 0 1 0 0)))

(check-expect (bot-turn B-game)      
              (make-board #true #false 2 (list 1 2 2 0 1 0 1 0 0)))

(define (bot-turn board)
  (cond [(= (board-difficulty board) 0)
         (make-board true false (board-difficulty board)
                     (insert-play (board-player-turn board) (board-values board) (random-open-space (board-values board))))]
        [(= (board-difficulty board) 1)
         (make-board true false (board-difficulty board)
                     (insert-play (board-player-turn board) (board-values board) (one-d board)))]
        [(= (board-difficulty board) 2)
         (make-board true false (board-difficulty board)
                     (insert-play (board-player-turn board) (board-values board) (two-d board)))]
        [else
         (make-board true false (board-difficulty board)
                     (insert-play (board-player-turn board) (board-values board) (random-open-space  (board-values board))))]
        ))





(check-expect 
 (insert-play true values-middle-game 3)
 (list 0 2 2
       1 1 0
       1 0 0)
 )

(check-expect 
 (insert-play true values-middle-game 4)
 (list 0 2 2
       0 1 0
       1 0 0)
 )
                                                              
(check-expect 
 (insert-play true values-middle-game 1)
 (list 0 2 2
       0 1 0
       1 0 0)
 )


(define (insert-play player-turn values box)
  (if (= (list-ref values box) 0)
      (append 
       (take values box) 
       (list (if player-turn 1 2))
       (take-right values (- 8 box)))
      values))





(define (random-open-space current-board)
  (list-ref (indexes-of current-board 0) (random (- (length (indexes-of current-board 0)) 1))))





(define B (make-board false false 1 (list 1 0 0
                                          2 2 0
                                          1 0 0)))

(define B2 (make-board false false 1 (list 0 2 1
                                           0 0 0
                                           1 2 0)))

(check-expect (one-d B) 5) 
(check-expect (one-d B2) 4) 

(define (one-d board)
  (local[
         (define-struct outcomes (index board)) 

         
         
         (define (generate-next-layer board) 
           (map (lambda (n) (make-outcomes n (make-board
                                              (not (board-player-turn board))
                                              (board-game-won board) (board-difficulty board)
                                              (insert-play (board-player-turn board) (board-values board) n))))   
                (indexes-where (board-values board) zero?)))      

         
         
         (define (checkit board)   
           (map (lambda (out) (make-outcomes (outcomes-index out) (check-board (outcomes-board out))))
                (generate-next-layer board)))

         
         
         
         
         (define (make-move board) 
           (local [(define list-of-won-boards (filter
                                               (lambda (n) (equal? (board-game-won (outcomes-board  n)) true))
                                               (checkit board)))] 
             (if (empty? list-of-won-boards)                      
                 (random-open-space (board-values board))
                 (outcomes-index (first list-of-won-boards)))))   
           
         ]
    (make-move board)))







(define B-almost (make-board true false 1 (list 0 0 0
                                                0 1 2
                                                2 0 1)))

(define B-almost2 (make-board true false 1 (list 2 0 0
                                                 0 1 2
                                                 2 0 1)))

(check-expect (two-d B-almost) 0) 
(check-expect (two-d B-almost2) 3) 


(define (two-d board)
  (local[
         (define-struct outcomes (index board)) 

         
         
         (define (generate-next-layer board) 
           (map (lambda (n) (make-outcomes n (make-board (not (board-player-turn board)) (board-game-won board) (board-difficulty board)
                                                         (insert-play (board-player-turn board) (board-values board) n))))   
                (indexes-where (board-values board) zero?)))      

         
         
         (define (checkit board)   
           (map (lambda (out) (make-outcomes (outcomes-index out) (check-board (outcomes-board out))))
                (generate-next-layer board)))

         
         
         
         
         (define (make-move board) 
           (local [(define list-of-won-boards (filter
                                               (lambda (n) (equal? (board-game-won (outcomes-board  n)) true))
                                               (checkit board)))] 
             (if (empty? list-of-won-boards)                      
                 -1                                               
                 (outcomes-index (first list-of-won-boards)))))   

         
         
         (define (flip-it board)
           (make-board (not (board-player-turn board)) (board-game-won board) (board-difficulty board) (board-values board)))
         ]
    
    (if (= (make-move board) -1)                     
        (if (= (make-move (flip-it board)) -1)       
            (random-open-space (board-values board)) 
            (make-move (flip-it board)))             
        (make-move board))))                         