

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tic tac toe starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 600) 
(define MTS (empty-scene SIZE SIZE))


(define XO-SIZE (round (/ SIZE 9)))
(define X-COLOR "red")
(define O-COLOR "blue")
(define XY-OFFSET (round (/ SIZE 7)))

(define HUMAN-PLAYER 'human)
(define COMPUTER-PLAYER 'computer)
(define (is-human-move? m) (eq? m HUMAN-PLAYER))
(define (is-computer-move? m) (eq? m COMPUTER-PLAYER))




(define EMPTY-BOARD (make-list 9 #f))










(define-struct world-state (board last-player ai-depth))

(define (fn-for-world-state ws)
  (... (world-state-board ws)
       (world-state-last-player ws)
       (world-state-ai-depth ws)))







(check-expect (same-player-in-spaces? null) #f) 
(check-expect (same-player-in-spaces? (make-list 3 HUMAN-PLAYER)) #t)
(check-expect (same-player-in-spaces? (list HUMAN-PLAYER COMPUTER-PLAYER)) #f)

(check-expect (same-player-in-spaces? (make-list 3 #f)) #f)
(define (same-player-in-spaces? spaces)
  (if (empty? spaces) #f
      (local [(define check-space (first spaces))
              (define (same-player-in-spaces? vals rsf)
                (cond
                  [(empty? vals) rsf]
                  [else (same-player-in-spaces? (rest vals)
                                                (and rsf
                                                     (not (false? (first vals)))
                                                     (eq? (first vals)
                                                          check-space)))]))]
        (same-player-in-spaces? spaces #t))))

(define FB-ROW0-DOMINATED (list COMPUTER-PLAYER COMPUTER-PLAYER COMPUTER-PLAYER
                                #f HUMAN-PLAYER HUMAN-PLAYER
                                HUMAN-PLAYER #f COMPUTER-PLAYER))
(define FB-ROW1-DOMINATED (list HUMAN-PLAYER COMPUTER-PLAYER HUMAN-PLAYER
                                COMPUTER-PLAYER COMPUTER-PLAYER COMPUTER-PLAYER
                                HUMAN-PLAYER HUMAN-PLAYER COMPUTER-PLAYER))
(define FB-ROW2-DOMINATED (list HUMAN-PLAYER COMPUTER-PLAYER HUMAN-PLAYER
                                COMPUTER-PLAYER HUMAN-PLAYER HUMAN-PLAYER
                                COMPUTER-PLAYER COMPUTER-PLAYER COMPUTER-PLAYER))
(define FB-NO-ROW-DOMINATED (list HUMAN-PLAYER HUMAN-PLAYER COMPUTER-PLAYER
                                  COMPUTER-PLAYER HUMAN-PLAYER HUMAN-PLAYER
                                  COMPUTER-PLAYER HUMAN-PLAYER COMPUTER-PLAYER))
(define PB-NO-ROW-DOMINATED (list #f HUMAN-PLAYER COMPUTER-PLAYER
                                  COMPUTER-PLAYER HUMAN-PLAYER #f
                                  COMPUTER-PLAYER #f COMPUTER-PLAYER))






(check-expect (is-one-row-dominated? FB-ROW0-DOMINATED) #t)
(check-expect (is-one-row-dominated? FB-ROW1-DOMINATED) #t)
(check-expect (is-one-row-dominated? FB-ROW2-DOMINATED) #t)
(check-expect (is-one-row-dominated? FB-NO-ROW-DOMINATED)  #f)
(check-expect (is-one-row-dominated? PB-NO-ROW-DOMINATED) #f)
(check-expect (is-one-row-dominated? EMPTY-BOARD) #f)
(define (is-one-row-dominated? board)
  (ormap
   (lambda (x)
     (same-player-in-spaces? (list (list-ref board x)
                                   (list-ref board (add1 x))
                                   (list-ref board (+ x 2)))))
   (list 0 3 6)))

(define FB-COL0-DOMINATED (list COMPUTER-PLAYER HUMAN-PLAYER HUMAN-PLAYER
                                COMPUTER-PLAYER COMPUTER-PLAYER HUMAN-PLAYER
                                COMPUTER-PLAYER HUMAN-PLAYER COMPUTER-PLAYER))
(define FB-COL1-DOMINATED (list COMPUTER-PLAYER COMPUTER-PLAYER HUMAN-PLAYER
                                COMPUTER-PLAYER COMPUTER-PLAYER HUMAN-PLAYER
                                HUMAN-PLAYER COMPUTER-PLAYER COMPUTER-PLAYER))
(define FB-COL2-DOMINATED (list HUMAN-PLAYER HUMAN-PLAYER COMPUTER-PLAYER
                                COMPUTER-PLAYER HUMAN-PLAYER HUMAN-PLAYER
                                COMPUTER-PLAYER HUMAN-PLAYER COMPUTER-PLAYER))
(define FB-NO-COL-DOMINATED (list COMPUTER-PLAYER HUMAN-PLAYER COMPUTER-PLAYER
                                  HUMAN-PLAYER COMPUTER-PLAYER HUMAN-PLAYER
                                  COMPUTER-PLAYER HUMAN-PLAYER COMPUTER-PLAYER))
(define PB-NO-COL-DOMINATED (list #f HUMAN-PLAYER COMPUTER-PLAYER
                                  #f COMPUTER-PLAYER #f
                                  #f #f COMPUTER-PLAYER))






(check-expect (is-one-col-dominated? FB-COL0-DOMINATED) #t)
(check-expect (is-one-col-dominated? FB-COL1-DOMINATED) #t)
(check-expect (is-one-col-dominated? FB-COL2-DOMINATED) #t)
(check-expect (is-one-col-dominated? FB-NO-COL-DOMINATED) #f)
(check-expect (is-one-col-dominated? PB-NO-COL-DOMINATED) #f)
(define (is-one-col-dominated? board)
  (ormap
   (lambda (x)
     (same-player-in-spaces? (list (list-ref board x)
                                   (list-ref board (+ x 3))
                                   (list-ref board (+ x 6)))))
   (list 0 1 2)))

(define FB-DIAG-DOMINATED (list COMPUTER-PLAYER HUMAN-PLAYER HUMAN-PLAYER
                                HUMAN-PLAYER COMPUTER-PLAYER HUMAN-PLAYER
                                HUMAN-PLAYER HUMAN-PLAYER COMPUTER-PLAYER))
(define FB-ANTIDIAG-DOMINATED (list HUMAN-PLAYER HUMAN-PLAYER COMPUTER-PLAYER
                                    HUMAN-PLAYER COMPUTER-PLAYER HUMAN-PLAYER
                                    COMPUTER-PLAYER HUMAN-PLAYER HUMAN-PLAYER))
(define FB-BOTH-DIAGS-DOMINATED (list COMPUTER-PLAYER HUMAN-PLAYER COMPUTER-PLAYER
                                      HUMAN-PLAYER COMPUTER-PLAYER HUMAN-PLAYER
                                      COMPUTER-PLAYER HUMAN-PLAYER COMPUTER-PLAYER))
(define FB-NO-DIAG-DOMINATED (list HUMAN-PLAYER HUMAN-PLAYER HUMAN-PLAYER
                                   HUMAN-PLAYER COMPUTER-PLAYER HUMAN-PLAYER
                                   COMPUTER-PLAYER HUMAN-PLAYER HUMAN-PLAYER))
(define PB-NO-DIAG-DOMINATED (list HUMAN-PLAYER HUMAN-PLAYER HUMAN-PLAYER
                                   #f #f HUMAN-PLAYER
                                   COMPUTER-PLAYER HUMAN-PLAYER #f))






(check-expect (is-one-diagonal-dominated? FB-DIAG-DOMINATED) #t)
(check-expect (is-one-diagonal-dominated? FB-ANTIDIAG-DOMINATED) #t)
(check-expect (is-one-diagonal-dominated? FB-BOTH-DIAGS-DOMINATED) #t)
(check-expect (is-one-diagonal-dominated? FB-NO-DIAG-DOMINATED) #f)
(check-expect (is-one-diagonal-dominated? PB-NO-DIAG-DOMINATED) #f)
(define (is-one-diagonal-dominated? board)
  (or
   (same-player-in-spaces? (list (list-ref board 0)
                                 (list-ref board 4)
                                 (list-ref board 8)))
   (same-player-in-spaces? (list (list-ref board 2)
                                 (list-ref board 4)
                                 (list-ref board 6)))))





(check-expect (is-board-finished? EMPTY-BOARD) #f)
(check-expect (is-board-finished? PB-NO-DIAG-DOMINATED) #f)
(check-expect (is-board-finished? FB-DIAG-DOMINATED) #t)
(define (is-board-finished? board)
  (andmap (compose not false?) board))





(check-expect (is-board-won? EMPTY-BOARD) #f)
(check-expect (is-board-won? (list HUMAN-PLAYER COMPUTER-PLAYER #f
                                   #f HUMAN-PLAYER #f
                                   HUMAN-PLAYER HUMAN-PLAYER #f))
              #f)

(check-expect (is-board-won? FB-ROW0-DOMINATED) #t)
(check-expect (is-board-won? FB-ROW1-DOMINATED) #t)
(check-expect (is-board-won? FB-ROW2-DOMINATED) #t)

(check-expect (is-board-won? FB-COL0-DOMINATED) #t)
(check-expect (is-board-won? FB-COL1-DOMINATED) #t)
(check-expect (is-board-won? FB-COL2-DOMINATED) #t)

(check-expect (is-board-won? FB-DIAG-DOMINATED) #t)
(check-expect (is-board-won? FB-ANTIDIAG-DOMINATED) #t)

(define (is-board-won? board)
  (or (is-one-row-dominated? board)
      (is-one-col-dominated? board)
      (is-one-diagonal-dominated? board)))










(check-expect (findf even? (list 1 2 3 4)) 2)
(define (findf proc loi)
  (cond
    [(empty? loi) #f]
    [(proc (first loi)) (first loi)]
    [else (findf proc (rest loi))]))




(define-struct board-pos (col row))

(define (fn-for-board-pos bpos)
  (... (board-pos-col bpos)
       (board-pos-row bpos)))





(check-expect (posn->board-pos (board-pos->posn (make-board-pos 0 0)))
              (make-board-pos 0 0))
(check-expect (posn->board-pos (board-pos->posn (make-board-pos 0 1)))
              (make-board-pos 0 1))
(check-expect (posn->board-pos (board-pos->posn (make-board-pos 0 2)))
              (make-board-pos 0 2))
(check-expect (posn->board-pos (board-pos->posn (make-board-pos 1 0)))
              (make-board-pos 1 0))
(check-expect (posn->board-pos (board-pos->posn (make-board-pos 1 1)))
              (make-board-pos 1 1))
(check-expect (posn->board-pos (board-pos->posn (make-board-pos 1 2)))
              (make-board-pos 1 2))
(check-expect (posn->board-pos (board-pos->posn (make-board-pos 2 0)))
              (make-board-pos 2 0))
(check-expect (posn->board-pos (board-pos->posn (make-board-pos 2 1)))
              (make-board-pos 2 1))
(check-expect (posn->board-pos (board-pos->posn (make-board-pos 2 2)))
              (make-board-pos 2 2))
(define (posn->board-pos pos)
  (local [(define (pos-in-bounds? idx)
            (local
              [(define COL (remainder idx 3))
               (define ROW (quotient idx 3))
               (define XSTART (board-pos-part->coord COL))
               (define YSTART (board-pos-part->coord ROW))
               (define XEND (board-pos-part->coord (add1 COL)))
               (define YEND (board-pos-part->coord (add1 ROW)))]
              (and (< XSTART (posn-x pos) XEND)
                   (< YSTART (posn-y pos) YEND))))]
    (idx->board-pos (findf pos-in-bounds? (range 9)))))






(check-expect (board-pos->idx (make-board-pos 0 0)) (+ 0 (* 3 0)))
(check-expect (board-pos->idx (make-board-pos 1 0)) (+ 1 (* 3 0)))
(check-expect (board-pos->idx (make-board-pos 2 0)) (+ 2 (* 3 0)))
(check-expect (board-pos->idx (make-board-pos 0 1)) (+ 0 (* 3 1)))
(check-expect (board-pos->idx (make-board-pos 1 1)) (+ 1 (* 3 1)))
(check-expect (board-pos->idx (make-board-pos 2 1)) (+ 2 (* 3 1)))
(check-expect (board-pos->idx (make-board-pos 0 2)) (+ 0 (* 3 2)))
(check-expect (board-pos->idx (make-board-pos 1 2)) (+ 1 (* 3 2)))
(check-expect (board-pos->idx (make-board-pos 2 2)) (+ 2 (* 3 2)))
(define (board-pos->idx bpos)
  (+ (board-pos-col bpos) (* (board-pos-row bpos) 3)))






(check-expect (idx->board-pos (+ 0 (* 3 0))) (make-board-pos 0 0))
(check-expect (idx->board-pos (+ 1 (* 3 0))) (make-board-pos 1 0))
(check-expect (idx->board-pos (+ 2 (* 3 0))) (make-board-pos 2 0))
(check-expect (idx->board-pos (+ 0 (* 3 1))) (make-board-pos 0 1))
(check-expect (idx->board-pos (+ 1 (* 3 1))) (make-board-pos 1 1))
(check-expect (idx->board-pos (+ 2 (* 3 1))) (make-board-pos 2 1))
(check-expect (idx->board-pos (+ 0 (* 3 2))) (make-board-pos 0 2))
(check-expect (idx->board-pos (+ 1 (* 3 2))) (make-board-pos 1 2))
(check-expect (idx->board-pos (+ 2 (* 3 2))) (make-board-pos 2 2))
(define (idx->board-pos idx)
  (make-board-pos (remainder idx 3) (quotient idx 3)))







(check-expect (board-pos-part->coord 0) 0) 
(check-expect (board-pos-part->coord 1)
              (* (/ SIZE 3) 1))
(define (board-pos-part->coord part)
  (* (/ SIZE 3) part))





(check-expect (board-pos->posn (make-board-pos 1 1))
              (make-posn (+ XY-OFFSET (board-pos-part->coord 1))
                         (+ XY-OFFSET (board-pos-part->coord 1))))
(define (board-pos->posn bpos)
  (make-posn (+ XY-OFFSET (board-pos-part->coord (board-pos-col bpos)))
             (+ XY-OFFSET (board-pos-part->coord (board-pos-row bpos)))))






(define (get-random-open-position a-board)
  (local
    [(define shuffled-positions (shuffle (build-list 9 idx->board-pos)))
     (define (pos-open? pos) (is-position-open? a-board pos))]
    (findf pos-open? shuffled-positions)))

(define PB-TEST1 (list HUMAN-PLAYER COMPUTER-PLAYER COMPUTER-PLAYER
                       HUMAN-PLAYER #f HUMAN-PLAYER
                       #f HUMAN-PLAYER #f))
(define FB-TEST2 (list COMPUTER-PLAYER COMPUTER-PLAYER COMPUTER-PLAYER
                       HUMAN-PLAYER HUMAN-PLAYER HUMAN-PLAYER
                       COMPUTER-PLAYER COMPUTER-PLAYER COMPUTER-PLAYER))






(check-satisfied (get-random-open-position PB-TEST1) (lambda (x) (is-position-open? PB-TEST1 x)))
(check-expect (get-random-open-position FB-TEST2) #f)
(check-expect (is-position-open? FB-TEST2 (get-random-open-position FB-TEST2)) #f)
(define (is-position-open? a-board pos)
  (and (not (false? pos))
       (false? (list-ref a-board (board-pos->idx pos)))))






(check-expect (get-position-occupant PB-TEST1 (make-board-pos 1 2))
              HUMAN-PLAYER)
(check-expect (get-position-occupant FB-TEST2 (make-board-pos 1 0))
              COMPUTER-PLAYER)
(define (get-position-occupant a-board bpos)
  (list-ref a-board (board-pos->idx bpos)))








(check-expect (get-player-name HUMAN-PLAYER) "You")
(check-expect (get-player-name COMPUTER-PLAYER) "The computer")
(check-error (get-player-name #f))
(define (get-player-name player)
  (cond
    [(eq? player HUMAN-PLAYER) "You"]
    [(eq? player COMPUTER-PLAYER) "The computer"]))

(define FB-WON (list HUMAN-PLAYER HUMAN-PLAYER HUMAN-PLAYER
                     #f COMPUTER-PLAYER COMPUTER-PLAYER
                     #f #f #f))
(check-satisfied FB-WON is-board-won?)
(define WS-WON (make-world-state FB-WON HUMAN-PLAYER 0))

(define FB-DRAW (list HUMAN-PLAYER COMPUTER-PLAYER HUMAN-PLAYER
                      HUMAN-PLAYER COMPUTER-PLAYER COMPUTER-PLAYER
                      COMPUTER-PLAYER HUMAN-PLAYER HUMAN-PLAYER))
(check-satisfied FB-DRAW is-board-finished?)
(check-satisfied FB-DRAW (compose not is-board-won?))
(define WS-DRAW (make-world-state FB-DRAW HUMAN-PLAYER 0))

(define WS-ONGOING (make-world-state EMPTY-BOARD #f 0))








(define (draw-world state)
  (local [
          (define (get-end-result state)
            (local
              [(define board (world-state-board state))]
              (cond
                [(is-board-won? board)
                 (string-append (get-player-name (world-state-last-player state))
                                " won!")]
                [(is-board-finished? board) "It's a draw!"]
                [else ""])))
          (define STICK-LENGTH (* 10/900 SIZE))
          (define STICK1 (rectangle STICK-LENGTH (- SIZE XY-OFFSET) "solid" "black"))
          (define STICK2 (rectangle (- SIZE XY-OFFSET) STICK-LENGTH "solid" "black"))
          (define BOARD-IMG
            (place-images
             (list
              (text (get-end-result state) XO-SIZE "red")
              STICK1
              STICK1
              STICK2
              STICK2)
             (list
              (make-posn (/ SIZE 2) (/ SIZE 2))
              (make-posn (- (/ SIZE 3)
                            (+ STICK-LENGTH
                               (/ STICK-LENGTH 2)))
                         (* 3.35 XY-OFFSET))
              (make-posn (- (* 2 (/ SIZE 3))
                            (+ STICK-LENGTH (/ STICK-LENGTH 2)))
                         (* 3.35 XY-OFFSET))
              (make-posn (* 3.35 XY-OFFSET)
                         (- (/ SIZE 3) STICK-LENGTH))
              (make-posn (* 3.35 XY-OFFSET)
                         (- (* 2 (/ SIZE 3))
                            (+ STICK-LENGTH
                               (/ STICK-LENGTH 2)))))
             MTS))
          (define (choose-img input)
            
            
            
            
            
            (scale XO-SIZE (cond
                             [(false? input) (square 1 "solid" "transparent")]
                             [(eq? input HUMAN-PLAYER)    (text "X" 1 X-COLOR)]
                             [(eq? input COMPUTER-PLAYER) (text "O" 1 O-COLOR)])))
          (define positions (build-list 9 idx->board-pos))
          (define board (world-state-board state))
          (define (board-pos->img bpos)
            (choose-img (get-position-occupant board bpos)))]
    (place-images
     (map board-pos->img positions)
     (map board-pos->posn positions)
     BOARD-IMG)))





(define (make-logicalish-move state player-move-pos)
  (local [(define depth (world-state-ai-depth state))
          (define board (list-set (world-state-board state)
                                  (board-pos->idx player-move-pos)
                                  HUMAN-PLAYER))
          (define (is-winning-move? move)
            (is-board-won? (list-set board (board-pos->idx move) COMPUTER-PLAYER)))
          (define (is-blocking-move? move)
            (is-board-won? (list-set board (board-pos->idx move) HUMAN-PLAYER)))
          (define (choose-move depth)
            (local [(define open-moves (map idx->board-pos (indexes-where board false?)))
                    (define winning-move (findf is-winning-move? open-moves))
                    (define blocking-move (findf is-blocking-move? open-moves))]
              (cond
                [(not (false? winning-move)) winning-move]
                [(and (not (false? blocking-move)) (> depth 1)) blocking-move]
                [else (get-random-open-position board)])))
          (define computer-move-pos (choose-move depth))]
    (if (or (is-board-won? board) (false? computer-move-pos))
        (make-world-state board
                          HUMAN-PLAYER (world-state-ai-depth state))
        (make-world-state (list-set board
                                    (board-pos->idx computer-move-pos)
                                    COMPUTER-PLAYER)
                          COMPUTER-PLAYER
                          (world-state-ai-depth state)))))

(check-satisfied (make-logicalish-move WS-ONGOING (idx->board-pos 1))
                 (lambda (ns)
                   (and (not (equal? ns WS-ONGOING))
                        (not (false? (findf is-human-move?
                                            (world-state-board ns))))
                        (not (false? (findf is-computer-move?
                                            (world-state-board ns))))
                        (not (game-over? ns)))))






(check-satisfied WS-ONGOING (compose not game-over?))
(check-satisfied WS-WON game-over?)
(check-satisfied WS-DRAW game-over?)
(define (game-over? state)
  (or (is-board-finished? (world-state-board state))
      (is-board-won? (world-state-board state))))






(define testpos (make-board-pos 1 2))
(define testcoords (board-pos->posn testpos))
(check-satisfied (handle-mouse WS-ONGOING
                               (posn-x testcoords)
                               (posn-y testcoords)
                               "button-down")
                 (lambda (x)
                   (and (not (is-position-open? (world-state-board x)
                                                testpos))
                        (is-human-move? (get-position-occupant (world-state-board x) testpos)))))
(check-expect (handle-mouse WS-WON (posn-x testcoords) (posn-y testcoords) "button-down") WS-WON)
(check-expect (handle-mouse WS-ONGOING 1337 1337 "hacker") WS-ONGOING) 
(define (handle-mouse state x y evt)
  (local
    [(define (handle-button-down state x y)
       (local
         [(define square-index (posn->board-pos (make-posn x y)))]
         (if (and (not (game-over? state))
                  (not (false? square-index))
                  (is-position-open? (world-state-board state)
                                     square-index))
             (make-logicalish-move state square-index)
             state)))]
    (cond
      [(string=? evt "button-down") (handle-button-down state x y)]
      [else state])))








(check-expect (world-state-ai-depth (handle-key WS-ONGOING "0")) 0)
(check-expect (world-state-ai-depth (handle-key WS-ONGOING "1")) 1)
(check-expect (world-state-ai-depth (handle-key WS-ONGOING "2")) 2)
(check-expect (world-state-ai-depth (handle-key WS-ONGOING "3"))
              (world-state-ai-depth WS-ONGOING))
(define (handle-key state evt)
  (cond
    [(or (string=? evt "0")
         (string=? evt "1")
         (string=? evt "2"))
     (make-world-state (world-state-board state)
                       (world-state-last-player state)
                       (string->number evt))]
    [else state]))

(define START
  (make-world-state (make-list 9 #f) #f 0))




(define (main initial-state)
  (big-bang initial-state
    (to-draw draw-world)
    (on-mouse handle-mouse)
    (on-key handle-key)))