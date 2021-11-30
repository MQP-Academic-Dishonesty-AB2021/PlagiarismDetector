

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 487) 
(define MTS (empty-scene SIZE SIZE))

(define X-COLOR "red")
(define O-COLOR "blue")
(define LINE-COLOR "orange")
(define TEXT-COLOR "green")

(define TEXT-SIZE (floor (/ SIZE 4)))
(define PEN-SIZE (floor (/ SIZE 30)))
(define X "x")
(define O "o")
(define BASE-DIFFICULTY 0)
(define PLAYER-CHAR X)

(define LINE-PEN (make-pen LINE-COLOR PEN-SIZE "solid" "round" "round"))

(define PADDING (/ SIZE 10))


(define BOARD-SIZE (- SIZE (* 2 PADDING)))

(define BOX-SIZE (/ BOARD-SIZE 3))



(define _ false)
(define EMPTY-BOARD (list _ _ _
                          _ _ _
                          _ _ _))



(define-struct bd-box (start-x start-y end-x end-y))





(check-expect (make-bounding-box 1 1) (make-bd-box PADDING
                                                   PADDING
                                                   (+ PADDING BOX-SIZE)
                                                   (+ PADDING BOX-SIZE)))
(check-expect (make-bounding-box 1 3) (make-bd-box PADDING
                                                   (+ PADDING (* (sub1 3) BOX-SIZE))
                                                   (+ PADDING BOX-SIZE)
                                                   (+ PADDING (* 3 BOX-SIZE))))
(check-expect (make-bounding-box 3 1) (make-bd-box (+ PADDING (* (sub1 3) BOX-SIZE))
                                                   PADDING
                                                   (+ PADDING (* 3 BOX-SIZE))
                                                   (+ PADDING BOX-SIZE)))
(check-expect (make-bounding-box 3 3) (make-bd-box (+ PADDING (* (sub1 3) BOX-SIZE))
                                                   (+ PADDING (* (sub1 3) BOX-SIZE))
                                                   (+ PADDING (* 3 BOX-SIZE))
                                                   (+ PADDING (* 3 BOX-SIZE))))

(define (make-bounding-box x y)
  (make-bd-box (+ PADDING (* BOX-SIZE (sub1 x)))
               (+ PADDING (* BOX-SIZE (sub1 y)))
               (+ PADDING (* BOX-SIZE x))
               (+ PADDING (* BOX-SIZE y))))




(check-expect (x-y->idx 1 1) 0)
(check-expect (x-y->idx 2 1) 1)
(check-expect (x-y->idx 3 1) 2)
(check-expect (x-y->idx 1 2) 3)
(check-expect (x-y->idx 2 2) 4)
(check-expect (x-y->idx 3 2) 5)
(check-expect (x-y->idx 1 3) 6)
(check-expect (x-y->idx 2 3) 7)
(check-expect (x-y->idx 3 3) 8)

(define (x-y->idx x y)
  (+ (sub1 x) (* 3 (sub1 y))))





(check-expect
 (elem-at (list X O X
                _ O _
                _ _ _)
          1 1)
 X)
(check-expect
 (elem-at (list X O X
                _ O _
                _ _ _)
          2 3)
 _)
(check-expect
 (elem-at (list X O X
                _ O _
                _ _ _)
          2 2)
 O)
(check-expect
 (elem-at (list X O X
                _ O _
                _ _ _)
          3 2)
 _)

(define (elem-at lox x y)
  (list-ref lox (x-y->idx x y)))



(check-expect (set-elem (list X O X
                              _ O _
                              _ _ _)
                        2 3
                        X)
              (list X O X
                    _ O _
                    _ X _))
(check-expect (set-elem (list X O X
                              _ O _
                              _ _ _)
                        2 2
                        _)
              (list X O X
                    _ _ _
                    _ _ _))
(check-expect (set-elem (list X O X
                              _ O _
                              _ _ _)
                        2 1
                        _)
              (list X _ X
                    _ O _
                    _ _ _))

(define (set-elem lox x y val)
  (list-set lox (x-y->idx x y) val))



(define BOUNDS
  (map
   (lambda (lon) (make-bounding-box (first lon) (first (rest lon))))
   (list (list 1 1) (list 2 1) (list 3 1)
         (list 1 2) (list 2 2) (list 3 2)
         (list 1 3) (list 2 3) (list 3 3))))












(define-struct player (difficulty board endstate turn?))

(define START (make-player BASE-DIFFICULTY EMPTY-BOARD false true))

(define (main WS)
  (big-bang WS
    (to-draw draw-board)
    (on-mouse mouse-input)
    (on-tick do-ai)
    (on-key handle-input)))




(check-expect (handle-input START "0")
              (set-difficulty START 0))
(check-expect (handle-input START "1")
              (set-difficulty START 1))
(check-expect (handle-input START "2")
              (set-difficulty START 2))
(check-expect (handle-input START "3")
              (set-difficulty START 3))
(check-expect (handle-input START "4")
              (set-difficulty START 4))
(check-expect (handle-input START "5")
              (set-difficulty START 5))
(check-expect (handle-input START "6")
              (set-difficulty START 6))
(check-expect (handle-input START "7")
              (set-difficulty START 7))
(check-expect (handle-input START "8")
              (set-difficulty START 8))
(check-expect (handle-input START "9")
              (set-difficulty START 9))

(define (handle-input player key)
  (cond [(key=? key "0") (set-difficulty player 0)]
        [(key=? key "1") (set-difficulty player 1)]
        [(key=? key "2") (set-difficulty player 2)]
        [(key=? key "3") (set-difficulty player 3)]
        [(key=? key "4") (set-difficulty player 4)]
        [(key=? key "5") (set-difficulty player 5)]
        [(key=? key "6") (set-difficulty player 6)]
        [(key=? key "7") (set-difficulty player 7)]
        [(key=? key "8") (set-difficulty player 8)]
        [(key=? key "9") (set-difficulty player 9)]
        [else player]))



(check-expect (set-difficulty START 0)
              (make-player 0
                           (player-board START)
                           (player-endstate START)
                           (player-turn? START)))
(check-expect (set-difficulty START 8)
              (make-player 8
                           (player-board START)
                           (player-endstate START)
                           (player-turn? START)))

(define (set-difficulty player diff)
  (make-player diff
               (player-board player)
               (player-endstate player)
               (player-turn? player)))



(check-expect (avg -8 8) 0)
(check-expect (avg 9 11) 10)
(check-expect (avg 18 2) 10)
(check-expect (avg 1 2) 1.5)

(define (avg rhs lhs) (/ (+ rhs lhs) 2))




(define EXAMPLE-BOARD
  (list X O _
        _ X _
        O _ _))


(check-expect (mouse-input START (/ SIZE 2) (/ SIZE 2) "move") START)

(check-expect (mouse-input
               (make-player BASE-DIFFICULTY EMPTY-BOARD "draw" true)
               (/ SIZE 2)
               (/ SIZE 2)
               "button-down")
              (make-player BASE-DIFFICULTY EMPTY-BOARD "draw" true))

(check-expect (mouse-input START 0 0 "button-down") START)

(check-expect (mouse-input START
                           (middle-bb "x" (x-y->idx 1 1))
                           (middle-bb "y" (x-y->idx 1 1))
                           "button-down")
              (make-player BASE-DIFFICULTY
                           (set-elem (player-board START) 1 1 PLAYER-CHAR)
                           (player-endstate START)
                           (not (player-turn? START))))

(check-expect (mouse-input (make-player BASE-DIFFICULTY EXAMPLE-BOARD false true)
                           (middle-bb "x" (x-y->idx 3 3))
                           (middle-bb "y" (x-y->idx 3 3))
                           "button-down")
              (make-player BASE-DIFFICULTY (set-elem EXAMPLE-BOARD 3 3 PLAYER-CHAR) false false))

(check-expect (mouse-input (make-player BASE-DIFFICULTY EXAMPLE-BOARD false false)
                           (middle-bb "x" (x-y->idx 3 3))
                           (middle-bb "y" (x-y->idx 3 3))
                           "button-down")
              (make-player BASE-DIFFICULTY EXAMPLE-BOARD false false))

(check-expect (mouse-input (make-player BASE-DIFFICULTY EXAMPLE-BOARD false true)
                           (middle-bb "x" (x-y->idx 1 1))
                           (middle-bb "y" (x-y->idx 1 1))
                           "button-down")
              (make-player BASE-DIFFICULTY EXAMPLE-BOARD false true))

(check-expect (mouse-input (make-player BASE-DIFFICULTY EXAMPLE-BOARD false true)
                           (middle-bb "x" (x-y->idx 1 3))
                           (middle-bb "y" (x-y->idx 1 3))
                           "button-down")
              (make-player BASE-DIFFICULTY EXAMPLE-BOARD false true))


(define (mouse-input player x y mouse-event)
  (cond [(not (string=? mouse-event "button-down")) player]
        [(not (false? (player-endstate player))) player]
        [(not (player-turn? player)) player]
        [(cond [(> x (+ PADDING (* 3 BOX-SIZE))) true]
               [(< x PADDING) true]
               [(> y (+ PADDING (* 3 BOX-SIZE))) true]
               [(< y PADDING) true]
               [else false])
         player]
        [else
         (local [(define box-idx (get-index-by-bb x y))]
           (if (false? (list-ref (player-board player) box-idx)) 
               (make-player (player-difficulty player)
                            (list-set (player-board player) box-idx PLAYER-CHAR)
                            (player-endstate player)
                            (not (player-turn? player)))
               player))]))




(check-expect (get-index-by-bb
               (middle-bb "x" (x-y->idx 3 3))
               (middle-bb "y" (x-y->idx 3 3)))
              8)
(check-expect (get-index-by-bb
               (middle-bb "x" (x-y->idx 1 1))
               (middle-bb "y" (x-y->idx 1 1)))
              0)
(check-expect (get-index-by-bb
               (middle-bb "x" (x-y->idx 2 2))
               (middle-bb "y" (x-y->idx 2 2)))
              4)

(define (get-index-by-bb x y)
  (local [(define (get-index lobb idx)
            (cond [(empty? lobb) (error "Invalid x and y for get-index-by-bb. HELP!")]
                  [(in-bounds? x y (first lobb)) idx]
                  [else (get-index (rest lobb) (add1 idx))]))]
    (get-index BOUNDS 0)))




(check-expect (in-bounds? 0 0 (elem-at BOUNDS 1 1)) false)

(check-expect (in-bounds?
               (middle-bb "x" (x-y->idx 1 1))
               (middle-bb "y" (x-y->idx 1 1))
               (elem-at BOUNDS 1 1)) true)

(check-expect (in-bounds?
               (middle-bb "x" (x-y->idx 1 1))
               (middle-bb "y" (x-y->idx 1 1))
               (elem-at BOUNDS 2 2)) false)

(check-expect (in-bounds?
               (middle-bb "x" (x-y->idx 2 2))
               (middle-bb "y" (x-y->idx 2 2))
               (elem-at BOUNDS 2 2)) true)

(define (in-bounds? x y box)
  (and (and (>= x (bd-box-start-x box)) (<= x (bd-box-end-x box)))
       (and (>= y (bd-box-start-y box)) (<= y (bd-box-end-y box)))))






(define (draw-lines MTS)
  (add-line
   (add-line
    (add-line
     (add-line
      MTS
      (bd-box-end-x (elem-at BOUNDS 1 1))
      (bd-box-start-y (elem-at BOUNDS 1 1))
      (bd-box-end-x (elem-at BOUNDS 1 3))
      (bd-box-end-y (elem-at BOUNDS 1 3))
      LINE-PEN)
     (bd-box-start-x (elem-at BOUNDS 3 1))
     (bd-box-start-y (elem-at BOUNDS 3 1))
     (bd-box-start-x (elem-at BOUNDS 3 3))
     (bd-box-end-y (elem-at BOUNDS 3 3))
     LINE-PEN)
    (bd-box-start-x (elem-at BOUNDS 1 1))
    (bd-box-end-y (elem-at BOUNDS 1 1))
    (bd-box-end-x (elem-at BOUNDS 3 1))
    (bd-box-end-y (elem-at BOUNDS 3 1))
    LINE-PEN)
   (bd-box-start-x (elem-at BOUNDS 1 3))
   (bd-box-start-y (elem-at BOUNDS 1 3))
   (bd-box-end-x (elem-at BOUNDS 3 3))
   (bd-box-start-y (elem-at BOUNDS 3 3))
   LINE-PEN))




(define (draw-board player)
  (draw-if-win (player-endstate player)
               (overlay-board-contents (player-board player) (draw-lines MTS))))




(check-expect (draw-if-win false MTS) MTS)
(check-expect (draw-if-win O MTS)
              (place-image (text "O wins!" TEXT-SIZE TEXT-COLOR) (/ SIZE 2) (/ SIZE 2) MTS))
(check-expect (draw-if-win X MTS)
              (place-image (text "X wins!" TEXT-SIZE TEXT-COLOR) (/ SIZE 2) (/ SIZE 2) MTS))
(check-expect (draw-if-win "draw" MTS)
              (place-image (text "A draw!" TEXT-SIZE TEXT-COLOR) (/ SIZE 2) (/ SIZE 2) MTS))

(define (draw-if-win state mts)
  (cond [(false? state) mts]
        [else (place-image
               (text (cond [(string=? state X) "X wins!"]
                           [(string=? state O) "O wins!"]
                           [(string=? state "draw") "A draw!"])
                     TEXT-SIZE
                     TEXT-COLOR)
               (/ SIZE 2) (/ SIZE 2) mts)]))    





(check-expect (overlay-board-contents EMPTY-BOARD (draw-lines MTS)) (draw-lines MTS))
(check-expect (overlay-board-contents (list X _ _ _ _ _ _ _ _) (draw-lines MTS))
              (place-image
               (text X TEXT-SIZE X-COLOR)                          
               (middle-bb "x" (x-y->idx 1 1))
               (middle-bb "y" (x-y->idx 1 1))
               (draw-lines MTS)))

(check-expect (overlay-board-contents (list X _ _ _ O _ _ _ X) (draw-lines MTS))
              (place-image
               (text X TEXT-SIZE X-COLOR)
               (middle-bb "x" (x-y->idx 1 1))
               (middle-bb "y" (x-y->idx 1 1))
               (place-image
                (text O TEXT-SIZE O-COLOR)
                (middle-bb "x" (x-y->idx 2 2))
                (middle-bb "y" (x-y->idx 2 2))
                (place-image
                 (text X TEXT-SIZE X-COLOR)
                 (middle-bb "x" (x-y->idx 3 3))
                 (middle-bb "y" (x-y->idx 3 3))
                 (draw-lines MTS)))))

(define (overlay-board-contents board mts)
  (local [(define (overlay-contents board mts idx)
            (cond [(empty? board) mts]
                  [(false? (first board)) (overlay-contents (rest board) mts (add1 idx))]
                  [else
                   (overlay-contents
                    (rest board)
                    (place-image 
                     (text (first board) TEXT-SIZE (color-for (first board)))
                     (middle-bb "x" idx)
                     (middle-bb "y" idx) mts)
                    (add1 idx))]))]
    (overlay-contents board mts 0)))



(check-expect (color-for X) X-COLOR)
(check-expect (color-for O) O-COLOR)

(define (color-for sym)
  (cond [(string=? sym X) X-COLOR]
        [(string=? sym O) O-COLOR]))





(check-expect (middle-bb "x" 0)
              (avg (bd-box-start-x (first BOUNDS))
                   (bd-box-end-x (first BOUNDS))))
(check-expect (middle-bb "y" 5)
              (avg (bd-box-start-y (list-ref BOUNDS 5))
                   (bd-box-end-y (list-ref BOUNDS 5))))

(define (middle-bb mode idx)
  (cond [(string=? mode "x")
         (avg (bd-box-start-x (list-ref BOUNDS idx))
              (bd-box-end-x (list-ref BOUNDS idx)))]
        [(string=? mode "y")
         (avg (bd-box-start-y (list-ref BOUNDS idx))
              (bd-box-end-y (list-ref BOUNDS idx)))]))










(define (do-ai player)
  (ai-turn (handle-game-over player)))





 





(check-expect (handle-game-over START) START)

(check-expect (handle-game-over (make-player BASE-DIFFICULTY EXAMPLE-BOARD false false))
              (make-player BASE-DIFFICULTY EXAMPLE-BOARD false false))

(check-expect (handle-game-over
               (make-player BASE-DIFFICULTY
                            (list _ _ X
                                  _ X _
                                  X O O)
                            false
                            false))
              (make-player BASE-DIFFICULTY
                           (list _ _ X
                                 _ X _
                                 X O O)
                           X
                           false))

(check-expect (handle-game-over
               (make-player BASE-DIFFICULTY
                            (list _ X X
                                  _ X _
                                  O O O)
                            false
                            true))
              (make-player BASE-DIFFICULTY
                           (list _ X X
                                 _ X _
                                 O O O)
                           O
                           true))

(check-expect (handle-game-over
               (make-player BASE-DIFFICULTY
                            (list O X X
                                  X X O
                                  O O X)
                            false
                            true))
              (make-player BASE-DIFFICULTY
                           (list O X X
                                 X X O
                                 O O X)
                           "draw"
                           true))


(define (handle-game-over player)
  (cond [(winning-board?
          (indexes-of (player-board player) O))
         (make-player (player-difficulty player) (player-board player) O (player-turn? player))]
        [(winning-board?
          (indexes-of (player-board player) X))
         (make-player (player-difficulty player) (player-board player) X (player-turn? player))]
        [(not (member false (player-board player)))
         (make-player (player-difficulty player)
                      (player-board player)
                      "draw"
                      (player-turn? player))]
        [else player]))



(check-expect (winning-board? (indexes-of EMPTY-BOARD O)) false)
(check-expect (winning-board? (indexes-of EMPTY-BOARD X)) false)
(check-expect (winning-board? (indexes-of EXAMPLE-BOARD O)) false)
(check-expect (winning-board? (indexes-of EXAMPLE-BOARD X)) false)
(check-expect (winning-board? 
               (indexes-of (list O X X
                                 X X O
                                 O O X) X))
              false)
(check-expect (winning-board? 
               (indexes-of (list O X X
                                 X X O
                                 O O X) O))
              false)
(check-expect (winning-board? 
               (indexes-of (list _ X X
                                 _ X _
                                 O O O) X))
              false)
(check-expect (winning-board? 
               (indexes-of (list _ X X
                                 _ X _
                                 O O O) O))
              true)
(check-expect (winning-board?
               (indexes-of (list _ _ X
                                 _ X _
                                 X O O) X))
              true)
(check-expect (winning-board?
               (indexes-of (list _ _ X
                                 _ X _
                                 X O O) O))
              false)
(check-expect (winning-board?
               (indexes-of (list X _ X
                                 X O _
                                 X O O) X))
              true)
(check-expect (winning-board?
               (indexes-of (list X _ X
                                 X O _
                                 X O O) O))
              false)

(define (winning-board? board)
  (local [(define WINSTATES
            (list
             (list 0 1 2)
             (list 3 4 5)
             (list 6 7 8)
             (list 0 3 6)
             (list 1 4 7)
             (list 2 5 8)
             (list 0 4 8)
             (list 2 4 6)))
          (define (matches-state todo)
            (cond [(empty? todo) false]
                  [(and (member (list-ref (first todo) 0) board)
                        (member (list-ref (first todo) 1) board)
                        (member (list-ref (first todo) 2) board))
                   true]
                  [else (matches-state (rest todo))]))]
    (matches-state WINSTATES)))





(check-expect (ai-turn START) START)

(check-expect (ai-turn (make-player BASE-DIFFICULTY EXAMPLE-BOARD false true))
              (make-player BASE-DIFFICULTY EXAMPLE-BOARD false true))

(check-expect (ai-turn (make-player BASE-DIFFICULTY EXAMPLE-BOARD O true))
              (make-player BASE-DIFFICULTY EXAMPLE-BOARD O true))

(check-expect (ai-turn (make-player BASE-DIFFICULTY EXAMPLE-BOARD X true))
              (make-player BASE-DIFFICULTY EXAMPLE-BOARD X true))

(check-expect (ai-turn (make-player BASE-DIFFICULTY EXAMPLE-BOARD "draw" true))
              (make-player BASE-DIFFICULTY EXAMPLE-BOARD "draw" true))


(check-expect (ai-turn (make-player BASE-DIFFICULTY EXAMPLE-BOARD O false))
              (make-player BASE-DIFFICULTY EXAMPLE-BOARD O false))

(check-expect (ai-turn (make-player BASE-DIFFICULTY EXAMPLE-BOARD X false))
              (make-player BASE-DIFFICULTY EXAMPLE-BOARD X false))

(check-expect (ai-turn (make-player BASE-DIFFICULTY EXAMPLE-BOARD "draw" false))
              (make-player BASE-DIFFICULTY EXAMPLE-BOARD "draw" false))

(check-expect (length
               (indexes-of
                (player-board
                 (ai-turn
                  (make-player BASE-DIFFICULTY EXAMPLE-BOARD false false))) O))
              (add1
               (length
                (indexes-of
                 (player-board
                  (make-player BASE-DIFFICULTY EXAMPLE-BOARD false false)) O))))


(check-expect (ai-turn (make-player 1 (list X X O
                                            _ O _
                                            _ _ X)
                                    false
                                    false))
              (make-player 1 (list X X O
                                   _ O _
                                   O _ X)
                           false
                           true))


 

(define (ai-turn player)
  (if (or (player-turn? player) (not (false? (player-endstate player)))) 
      player
      (make-player (player-difficulty player)
                   (list-set (player-board player)
                             (get-best-next-idx
                              (player-board player)
                              (indexes-of (player-board player) false)
                              O
                              (player-difficulty player))
                             O)
                   (player-endstate player)
                   (not (player-turn? player)))))





(check-expect (get-best-next-idx (list X X O
                                       X O X
                                       _ _ _)
                                 (list 6 7 8)
                                 O 1)
              6)

(check-expect (get-best-next-idx (list _ O _
                                       X _ X
                                       O X _)
                                 (list 0 2 4 8)
                                 O 2)
              4)

(define (get-best-next-idx board valids char depth)
  (local [(define board-vals
            (map
             (lambda (idx) (mm-board (list-set board idx O) (invert-ch O) O depth))
             valids))
          (define max-val (argmax identity board-vals))]
    (list-ref valids
              (first (shuffle
                      (indexes-where board-vals
                                     (lambda (val) (= max-val val))))))))



(define (invert-ch ch) 
  (cond [(string=? ch X) O]
        [(string=? ch O) X]))






(check-expect (board-eval EMPTY-BOARD O) 0)
(check-expect (board-eval EMPTY-BOARD X) 0)
(check-expect (board-eval (list X X X
                                O _ O
                                _ _ _) O) -1)
(check-expect (board-eval (list X X X
                                O _ O
                                _ _ _) X) 1)

(check-expect (board-eval (list X X _
                                O O O
                                _ X _) O) 1)
(check-expect (board-eval (list X X _
                                O O O
                                _ X _) X) -1)
(check-expect (board-eval (list X O X
                                O O X
                                X X O) O) 0)
(check-expect (board-eval (list X O X
                                O O X
                                X X O) X) 0)

(define (board-eval board ch)
  (cond [(winning-board? (indexes-of board ch)) 1]
        [(winning-board? (indexes-of board (invert-ch ch))) -1]
        [else 0]))

(define BOARD-X-WIN-0
  (list X X X
        _ O O
        _ _ O))
(define BOARD-O-WIN-0
  (list O X X
        _ O X
        _ _ O))
(define BOARD-X-WIN-1
  (list X _ X
        _ O O
        _ _ O))
(define BOARD-O-WIN-1
  (list O X _
        _ O X
        X _ _))
(define BOARD-EXP-1
  (list _ X _
        _ O X
        _ _ _))
(define BOARD-EXP-2
  (list _ X O
        _ X _
        _ O _))
(define BOARD-FULL
  (list O X O
        X X O
        X O X))









(check-expect (mm-board BOARD-X-WIN-0 O O 0) 0)
(check-expect (mm-board BOARD-O-WIN-0 X X 0) 0)

(check-expect (mm-board BOARD-X-WIN-0 O X 0) 0)
(check-expect (mm-board BOARD-O-WIN-0 X O 0) 0)


(check-expect (mm-board BOARD-X-WIN-0 O O 1) -1)
(check-expect (mm-board BOARD-O-WIN-0 X X 1) -1)

(check-expect (mm-board BOARD-X-WIN-0 O X 1) 1)
(check-expect (mm-board BOARD-O-WIN-0 X O 1) 1)

(check-expect (mm-board BOARD-X-WIN-1 O O 1) 0)
(check-expect (mm-board BOARD-O-WIN-1 X X 1) 0)

(check-expect (mm-board BOARD-X-WIN-1 O X 1) 0)
(check-expect (mm-board BOARD-O-WIN-1 X O 1) 0)

(check-expect (mm-board BOARD-EXP-1 O O 1) 0)
(check-expect (mm-board BOARD-EXP-1 X X 1) 0)
(check-expect (mm-board BOARD-EXP-1 O X 1) 0)
(check-expect (mm-board BOARD-EXP-1 X O 1) 0)

(check-expect (mm-board BOARD-FULL O O 1) 0)
(check-expect (mm-board BOARD-FULL X X 1) 0)
(check-expect (mm-board BOARD-FULL O X 1) 0)
(check-expect (mm-board BOARD-FULL X O 1) 0)


(check-expect (mm-board BOARD-X-WIN-1 X O 2) -1)
(check-expect (mm-board BOARD-O-WIN-1 O X 2) -1)

(check-expect (mm-board BOARD-X-WIN-1 X X 2) 1)
(check-expect (mm-board BOARD-O-WIN-1 O O 2) 1)

(define (mm-board board player for-who depth)
  (local [(define (mm-lob board player for-who depth)
            (map (lambda (board)
                   (mm-board board (invert-ch player) for-who depth))
                 (map (lambda (idx) (list-set board idx player)) (indexes-of board false))))]
    (cond [(= depth 0) 0]
          [(not (member false board)) 0]
          [(= 1 (board-eval board for-who)) 1]
          [(= -1 (board-eval board for-who)) -1]
          [(string=? player for-who) (argmax identity (mm-lob board player for-who (sub1 depth)))]
          [(not (string=? player for-who))
           (argmin identity (mm-lob board player for-who (sub1 depth)))])))

