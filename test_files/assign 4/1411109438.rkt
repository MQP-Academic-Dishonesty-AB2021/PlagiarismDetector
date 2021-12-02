

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab4Part2F2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 








(define SIZE 420) 
(define TEXT_SCALAR 0.8) 
(define XCOLOR "red")
(define OCOLOR "blue")


(define SIZE_1/2 (floor (/ SIZE 2)))
(define SIZE_1/3 (floor (/ SIZE 3)))
(define SIZE_2/3 (floor (* SIZE_1/3 2))) 
(define TEXT_SIZE (floor (* SIZE_1/3 TEXT_SCALAR)))


(define LINE_COLOUR "black")
(define LINE_THICKNESS (floor (/ SIZE 50)))
(define LINE_PEN (make-pen LINE_COLOUR LINE_THICKNESS "solid" "butt" "bevel"))


(define MTS (empty-scene SIZE SIZE)) 


(define BOARD_SCENE (scene+line
                     (scene+line
                      (scene+line
                       (scene+line MTS SIZE_1/3 0 SIZE_1/3 SIZE LINE_PEN)
                       SIZE_2/3 0 SIZE_2/3 SIZE LINE_PEN)
                      0 SIZE_1/3 SIZE SIZE_1/3 LINE_PEN)
                     0 SIZE_2/3 SIZE SIZE_2/3 LINE_PEN))


(define ROWS (list (list 0 1 2)
                   (list 3 4 5)
                   (list 6 7 8)))

(define COLS (list (list 0 3 6)
                   (list 1 4 7)
                   (list 2 5 8)))

(define DIAGS (list (list 0 4 8)
                    (list 2 4 6)))

(define UNITS (append ROWS COLS DIAGS))



(define EMPTY-BOARD (list " "  " "  " "
                          " "  " "  " "
                          " "  " "  " "))

(define OWIN-T1 (list "O" "X" "O"
                      "O" " " "X"
                      "O" "X" "X"))

(define XWIN-T1 (list "X" "O" "X"
                      " " "X" "O"
                      "O" "O" "X"))

(define GENERIC-T1 (list "O" "X" " "
                         " " " " " "
                         "O" "X" "X"))

(define O1MvWIN-T2 (list "O" "X" " "
                         " " " " " "
                         "O" "X" "X"))

(define X1MvWIN-T1 (list " " " " " "
                         " " " " "X"
                         "O" " " "X"))

(define TIE-T1 (list "O" "X" "X"
                     "X" " " "O"
                     "O" "O" "X"))

(define TIE-T2 (list "O" "X" "X"
                     "X" "X" "O"
                     "O" "O" "X"))



















(define-struct game (board difficulty))

(define START (make-game EMPTY-BOARD 0))


(define (main game)
  (big-bang game
    (to-draw render)     
    (on-mouse game-move) 
    (on-key key-logic))) 










(define (check-board-winner board)
  (local [
          
          (define (check-unit-winner unit)
            (cond [(andmap (λ (n) (string=? n "X")) unit) "X"]
                  [(andmap (λ (n) (string=? n "O")) unit) "O"]
                  [else " "]))

          
          
          
          
          (define (check-units-win units)
            (local [
                    
                    
                    (define (no-possible-moves? board)
                      (= (length (filter (λ (n) (string=? " " n)) board)) 0))

                    
                    
                    
                    (define (get-unit board unit)
                      (cond [(empty? unit) empty]
                            [else
                             (append (list (list-ref board (first unit)))
                                     (get-unit board (rest unit)))]))]

              
              (cond [(empty? units)
                     (if (no-possible-moves? board)
                         "N"
                         " ")]
                    [else
                     (local [(define unitwin
                               (check-unit-winner (get-unit board (first units))))]
                       (cond [(string=? unitwin "X") "X"]
                             [(string=? unitwin "O") "O"]
                             [else (check-units-win (rest units))]))])))]

    
    (check-units-win UNITS)))

(check-expect (check-board-winner TIE-T1) " ")
(check-expect (check-board-winner TIE-T2) "N")
(check-expect (check-board-winner OWIN-T1) "O")
(check-expect (check-board-winner XWIN-T1) "X")





(define (render game)
  (local [(define (draw-board board pos)
            (cond [(empty? board) BOARD_SCENE]
                  [else (place-image (letter->image (first board))
                                     (pos->x pos)
                                     (pos->y pos)
                                     (draw-board (rest board) (+ pos 1)))]))
          (define (pos->x num)
            (pos->v (λ (n) (remainder n 3)) num))
          (define (pos->y num)
            (pos->v (λ (n) (floor (/ n 3))) num))
          (define (pos->v fn num)
            (+ (* (fn num) SIZE_1/3) (/ SIZE 6)))
          (define (letter->image letter)
            (text letter
                  TEXT_SIZE
                  (if (string=? letter "X")
                      XCOLOR
                      OCOLOR)))
          (define (win-draw board) 
            (local [(define win (check-board-winner board))]
              (cond [(string=? win " ")
                     (draw-board board 0)]
                    [else
                     (place-image (win-image win)
                                  SIZE_1/2
                                  SIZE_1/2
                                  (draw-board board 0))])))
          (define (win-image letter) 
            (text (string-append (if (string=? letter "N")
                                     "Nobody"
                                     letter)
                                 " wins!"
                                 "\n"
                                 "   Hit R to restart"
                                 "\n")
                  (floor (/ TEXT_SIZE 4))
                  "green"))]
    (win-draw (game-board game))))



(define RENDER_TIE_T2 (render (make-game TIE-T2 0))) 

(check-expect (render START)
              BOARD_SCENE) 

(check-expect (= (image-width RENDER_TIE_T2) 
                 (image-height RENDER_TIE_T2)
                 SIZE) true) 







(define (game-move game mx my me)
  (local [
          
          
          (define (valid-move? board pos)
            (string=? (list-ref board pos) " "))

          
          
          
          
          (define (place-square bd pos letter)
            (if (> pos -1)
                (append (take bd pos)
                        (list letter)
                        (drop bd (+ pos 1)))
                bd))

          
          
          
          
          (define (possible-moves-LOPos board)
            (local [(define (match-LOPos lol pos)
                      (cond [(empty? lol) empty]
                            [else
                             (if (string=? (first lol) " ")
                                 (cons pos
                                       (match-LOPos (rest lol) (+ pos 1)))
                                 (match-LOPos (rest lol) (+ pos 1)))]))]
              (match-LOPos board 0)))

          
          
          
          
          
          (define (place-random-valid-pos board letter)
            (local [(define (pick-random-valid-pos board)
                      (local [(define valid-moves (possible-moves-LOPos board))]
                        (if (> (length valid-moves) 0)
                            (list-ref valid-moves (random (length valid-moves))) -1)))]
              (if (string=? (check-board-winner board) " ")
                  (place-square board
                                (pick-random-valid-pos board)
                                letter)
                  board)))

          
          
          
          (define (click-pos-on-board x y)
            (local [(define bx (floor (/ x (/ SIZE 3))))
                    (define by (floor (/ y (/ SIZE 3))))]
              (+ bx (* by 3))))

          
          
          
          
          (define (smart-move game)
            (local [
                    
                    (define (choose-win-pos? board letter)           
                      (string=? (check-board-winner board) letter))

                    
                    
                    
                    (define (check-win-subs board subs letter)
                      (local [
                              
                              
                              (define (flip letter)
                                (cond [(string=? letter "X") "O"]
                                      [(string=? letter "O") "X"]
                                      [else error])) 

                              
                              
                              
                              (define win-move?
                                (choose-win-pos?
                                 (place-square board (first subs) letter)
                                 letter))]

                        
                        (cond [win-move? (place-square 
                                          board 
                                          (first subs) 
                                          "O")] 

                              [(or (empty? subs) (empty? (rest subs))) 
                               (if (string=? letter "X")     
                                   (place-random-valid-pos board "O")    
                                   (if (= (game-difficulty game) 2)      
                                       (check-win-subs
                                        board                        
                                        (possible-moves-LOPos board)
                                        (flip letter))
                                       (place-random-valid-pos board "O")))] 
                              
                              [else (check-win-subs board (rest subs) letter)])))] 

              
              (check-win-subs (game-board game)
                              (possible-moves-LOPos (game-board game))
                              "O")))]

    
    (cond [(and (mouse=? me "button-down")
                (string=? (check-board-winner (game-board game)) " "))
           (local [(define work-pos (remainder (click-pos-on-board mx my) 9))]
             
             
             
             (if (valid-move? (game-board game) work-pos)
                 (local [(define pmove (place-square (game-board game)
                                                     work-pos
                                                     "X"))]
                   (cond [(string=? (check-board-winner pmove) " ") 
                          (make-game (if (not (= (game-difficulty game) 0)) 
                                         (smart-move (make-game pmove
                                                                (game-difficulty game))) 
                                         (place-random-valid-pos pmove "O")) 
                                     (game-difficulty game))]
                         [else
                          (make-game pmove (game-difficulty game))])) 
                 game))]     
          [else game]))) 


(check-expect
 (game-move START 0 0 "button-up") START) 
(check-satisfied
 (game-move START 0 0 "button-down")  
 (lambda(x) 
   (and (member? "O" (game-board x)) (member? "X" (game-board x)))))
(check-expect 
 (game-move (make-game TIE-T2 2) 0 0 "button-down")
 (make-game TIE-T2 2))
(check-expect 
 (game-move (make-game XWIN-T1 1) SIZE_1/2 SIZE_1/2 "button-down")
 (make-game XWIN-T1 1))
(check-expect 
 (game-move (make-game OWIN-T1 0) SIZE_1/2 SIZE_1/2 "button-down")
 (make-game OWIN-T1 0))










(define (key-logic game key)
  (cond [(key=? key "r")
         (make-game EMPTY-BOARD (game-difficulty game))]
        [(key=? key "0") 
         (make-game (game-board game) 0)]
        [(key=? key "1") 
         (make-game (game-board game) 1)] 
        [(key=? key "2") 
         (make-game (game-board game) 2)]
        [else game])) 


(check-expect (key-logic START "x") START) 
(check-expect (key-logic (make-game TIE-T1 2) "r")
              (make-game EMPTY-BOARD 2)) 
(check-expect (key-logic (make-game GENERIC-T1 2) "0")
              (make-game GENERIC-T1 0)) 
(check-expect (key-logic (make-game GENERIC-T1 2) "1")
              (make-game GENERIC-T1 1)) 
(check-expect (key-logic START "2")
              (make-game EMPTY-BOARD 2)) 
