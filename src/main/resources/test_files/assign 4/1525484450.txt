

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tic tac toe minimax|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))
(define PEN (pen "orange" 10 "solid" "round" "bevel"))
(define LARGE-OFFSET (/ SIZE 3))
(define SMALL-OFFSET (/ SIZE 20))

(define STARTING-BOARD 
  (add-line
   (add-line
    (add-line
     (add-line MTS LARGE-OFFSET SMALL-OFFSET LARGE-OFFSET (- SIZE SMALL-OFFSET) PEN)
    (- SIZE LARGE-OFFSET) SMALL-OFFSET (- SIZE LARGE-OFFSET) (- SIZE SMALL-OFFSET) PEN)
   SMALL-OFFSET LARGE-OFFSET (- SIZE SMALL-OFFSET) LARGE-OFFSET PEN)
  SMALL-OFFSET (- SIZE LARGE-OFFSET) (- SIZE SMALL-OFFSET) (- SIZE LARGE-OFFSET) PEN))

(define FONT-SIZE (floor (* LARGE-OFFSET 2/3)))
(define X-IMAGE (text "X" FONT-SIZE "red"))
(define O-IMAGE (text "O" FONT-SIZE "blue"))
(define F false)
(define X "x")
(define O "o")
(define UNITS (list (list 0 1 2) (list 3 4 5) (list 6 7 8) 
                    (list 0 3 6) (list 1 4 7) (list 2 5 8) 
                    (list 0 4 8) (list 2 4 6)))            
(define tiles1 (list F F F
                     F F F
                     F F F))
(define tiles2 (list X O F
                     O X O
                     X X F))
(define tiles3 (list X O X
                     O X O
                     O X X))
(define tiles4 (list O X X
                     O F X
                     O F F))
(define tiles5 (list X O X
                     O X O
                     O X O))
(define tiles6 (list X F F
                     F O F
                     F F F))
(define tiles7 (list X O X
                     X O F
                     F F F))








 

 

(define-struct boardState (tiles difficulty))





(define START (make-boardState tiles1 0))

 

(define (main ws)
  (big-bang ws
    (on-draw render) 
    (on-mouse mouse-event) 
    (on-key key-event))) 




(check-expect (read-board tiles1 0) F)
(check-expect (read-board tiles2 5) O)
(check-expect (read-board tiles2 8) F)

(define (read-board tiles position)
  (list-ref tiles position))




(check-expect (write-to-board empty O 5) empty)
(check-expect (write-to-board tiles1 X 2) (list F F X
                                                F F F
                                                F F F))
(check-expect (write-to-board tiles2 O 8) (list X O F
                                                O X O
                                                X X O))

(define (write-to-board tiles tile position)
  (local [(define (write-to-board tiles current-pos)
            (cond [(empty? tiles) empty]
                  [(> position 8) (error "Position out of bounds")]
                  [(= position current-pos) (cons tile (rest tiles))]
                  [else
                   (cons (first tiles) (write-to-board (rest tiles) (add1 current-pos)))]))]
    (write-to-board tiles 0)))




(check-expect (get-number-of-tiles tiles1 X) 0)
(check-expect (get-number-of-tiles tiles3 X) 5)
(check-expect (get-number-of-tiles tiles4 O) 3)

(define (get-number-of-tiles tiles tile)
  (cond [(empty? tiles) 0]
        [else
         (if (equal-tile (first tiles) tile)
             (+ 1 (get-number-of-tiles (rest tiles) tile))
             (get-number-of-tiles (rest tiles) tile))]))





(check-expect (get-number-of-tiles (play-computer-move tiles1 1) O) 1)
(check-expect (get-number-of-tiles (play-computer-move tiles2 1) O) 4)
(check-expect (get-number-of-tiles (play-computer-move tiles4 1) O) 3)

(define (play-computer-move tiles difficulty)
  (if (string=? (game-is-terminal tiles) "")
      (local [(define empty-positions (return-empty-positions tiles))
              (define random-position
                (list-ref empty-positions (random (length empty-positions))))]
        (cond [(empty? tiles) empty]
              [else
               (write-to-board tiles O (minimax tiles difficulty))]))
      tiles))




(check-expect (return-empty-positions tiles1) (list 0 1 2 3 4 5 6 7 8))
(check-expect (return-empty-positions tiles2) (list 2 8))
(check-expect (return-empty-positions tiles3) empty)

(define (return-empty-positions tiles)
  (local [(define (return-empty-positions tiles position)
            (cond [(empty? tiles) empty]
                  [else
                   (if (false? (first tiles))
                       (cons position (return-empty-positions (rest tiles)
                                                              (add1 position)))
                       (return-empty-positions (rest tiles) (add1 position)))]))]
    (return-empty-positions tiles 0)))




(check-expect (game-is-terminal tiles1) "")
(check-expect (game-is-terminal tiles2) "")
(check-expect (game-is-terminal tiles3) "player")
(check-expect (game-is-terminal tiles4) "computer")
(check-expect (game-is-terminal tiles5) "tie")

(define (game-is-terminal tiles)
  (cond [(tile-won? tiles X) "player"]
        [(tile-won? tiles O) "computer"]
        [(andmap (λ (tile) (not (false? tile))) tiles) "tie"]
        [else ""]))





(check-expect (tile-won? tiles1 X) false)
(check-expect (tile-won? tiles2 O) false)
(check-expect (tile-won? tiles3 X) true)
(check-expect (tile-won? tiles3 O) false)
(check-expect (tile-won? tiles4 O) true)
(check-expect (tile-won? tiles4 X) false)

(define (tile-won? tiles winning-tile)
  (local [(define (equal-tile-inner tile)
            (equal-tile tile winning-tile))
          (define (get-tile-from-position position) 
            (read-board tiles position))
          (define (get-tiles-from-unit unit) 
            (map get-tile-from-position unit))
          (define (check-unit unit) 
            (= 3 (length (filter equal-tile-inner (get-tiles-from-unit unit)))))
          (define (check-units units) 
            (ormap check-unit units))]
    (check-units UNITS)))




(check-expect (equal-tile X "x") true)
(check-expect (equal-tile F "o") false)
(check-expect (equal-tile F false) false)
(check-expect (equal-tile O "o") true)

(define (equal-tile tile string)
  (if (false? tile)
      false
      (string=? tile string)))





(check-expect (place-tile 0 STARTING-BOARD X-IMAGE)
              (place-image X-IMAGE (/ LARGE-OFFSET 2) (/ LARGE-OFFSET 2) STARTING-BOARD))
(check-expect (place-tile 2 STARTING-BOARD O-IMAGE)
              (place-image O-IMAGE (- SIZE (/ LARGE-OFFSET 2)) (/ LARGE-OFFSET 2) STARTING-BOARD))

(define (place-tile position scene symbol)
  (local [(define offset (/ LARGE-OFFSET 2))
          (define xpos (+ (* (modulo position 3) LARGE-OFFSET) offset))
          (define ypos (+ (* (floor (/ position 3)) LARGE-OFFSET) offset))]
    (place-image symbol xpos ypos scene)))





(check-expect (mouse-to-pos (/ SIZE 6) (/ SIZE 6)) 0)
(check-expect (mouse-to-pos (* SIZE 5/6) (/ SIZE 2)) 5)

(define (mouse-to-pos xpos ypos)
  (+ (floor (/ xpos LARGE-OFFSET)) (* (floor (/ ypos LARGE-OFFSET)) 3)))




(check-random (mouse-event (make-boardState tiles1 0)
                           (/ SIZE 2) (/ SIZE 2) "button-down")
              (local [(define new-list (play-computer-move
                                        (write-to-board tiles1
                                                        X
                                                        (mouse-to-pos (/ SIZE 2)
                                                                      (/ SIZE 2))) 0))]
                (make-boardState new-list 0)))
(check-expect (mouse-event (make-boardState tiles3 0)
                           (/ SIZE 3) (/ SIZE 2) "button-down")
              (make-boardState tiles3 0))
(check-expect (mouse-event (make-boardState tiles2 0)
                           (/ SIZE 2) (/ SIZE 2) "button-down")
              (make-boardState tiles2 0))

(define (mouse-event bstate xpos ypos me)
  (local [(define pos (mouse-to-pos xpos ypos))]
    (cond [(or (> pos 8) (< pos 0)) bstate]
          [(not (string=? (game-is-terminal (boardState-tiles bstate)) "")) bstate] 
          [(not (false? (read-board (boardState-tiles bstate) pos))) bstate] 
          [(mouse=? me "button-down")
           (local [(define new-list (play-computer-move (write-to-board (boardState-tiles bstate)
                                                                        X
                                                                        pos)
                                                        (boardState-difficulty bstate)))]
             (make-boardState new-list (boardState-difficulty bstate)))]
          [else bstate])))




(define (render bstate)
  (local [(define (draw-text-on-screen winning-state scene)
            (local [(define result-image
                      (cond [(string=? winning-state "") empty-image]
                            [(string=? winning-state "player") (text "X WINS" FONT-SIZE "green")]
                            [(string=? winning-state "computer") (text "O WINS" FONT-SIZE "green")]
                            [(string=? winning-state "tie") (text "TIE" FONT-SIZE "green")]))]
              (place-image result-image (/ SIZE 2) (/ SIZE 2) scene)))
          (define (render-board tiles)
            (local [(define (tile->image tile)
                      (cond [(false? tile) empty-image]
                            [(string=? tile X) X-IMAGE]
                            [(string=? tile O) O-IMAGE]))
                    (define (render-tiles tiles acc)
                      (cond [(empty? tiles) STARTING-BOARD]
                            [else
                             (place-tile acc (render-tiles (rest tiles) (add1 acc))
                                         (tile->image (first tiles)))]))]
              (render-tiles tiles 0)))]
    (draw-text-on-screen (game-is-terminal (boardState-tiles bstate))
                         (render-board (boardState-tiles bstate)))))





(check-expect (search-depth-1 tiles7) 7)
(check-random (search-depth-1 tiles1)
              (list-ref (return-empty-positions tiles1)
                        (random (length (return-empty-positions tiles1)))))

(define (search-depth-1 tiles)
  (local [(define empty-positions (return-empty-positions tiles))
          (define random-position (list-ref empty-positions (random (length empty-positions))))
          (define (check-position pos previous)
            (if (string=? (game-is-terminal (write-to-board tiles O pos)) "computer")
                pos
                previous))]
    (foldr check-position random-position empty-positions)))






(check-expect (search-depth-2 tiles7) 7)
(check-random (search-depth-2 tiles1)
              (list-ref (return-empty-positions tiles1)
                        (random (length (return-empty-positions tiles1)))))

(define (search-depth-2 tiles)
  (local [(define empty-positions (return-empty-positions tiles))
          (define random-position (list-ref empty-positions (random (length empty-positions))))
          (define (check-position pos previous)
            (cond [(string=? (game-is-terminal (write-to-board tiles O pos)) "computer") pos]
                  [(string=? (game-is-terminal (write-to-board tiles O previous)) "computer") previous]
                  [(string=? (game-is-terminal (write-to-board tiles X pos)) "player") pos]
                  [else previous]))]
    (foldr check-position random-position empty-positions)))



(check-expect (key-event START "1") (make-boardState (boardState-tiles START) 1))
(check-expect (key-event START "0") START)
(check-expect (key-event START "j") START)

(define (key-event bstate ke)
  (cond [(not (false? (string->number ke)))
         (make-boardState (boardState-tiles bstate) (string->number ke))]
        [else bstate]))




(check-expect (minimax tiles7 1) 7)
(check-random (minimax tiles1 1)
              (list-ref (return-empty-positions tiles1)
                        (random (length (return-empty-positions tiles1)))))
(check-expect (minimax tiles7 9) 7)
(check-random (minimax tiles1 2)
              (list-ref (return-empty-positions tiles1)
                        (random (length (return-empty-positions tiles1)))))

(define (minimax tiles depth)
  (local [
          (define empty-positions (return-empty-positions tiles))
          (define random-position (list-ref empty-positions (random (length empty-positions))))
          (define (utility tiles)
            (cond [(string=? (game-is-terminal tiles) "computer") 1]
                  [(string=? (game-is-terminal tiles) "player") -1]
                  [(string=? (game-is-terminal tiles) "tie") 0]))
          (define (get-board-combinations tiles tile-to-place) 
            (local [(define empty-positions (return-empty-positions tiles))]
              (map (λ (pos) (write-to-board tiles tile-to-place pos)) empty-positions)))
          (define (get-max-score tiles depth)
            (cond [(= depth 0) 0]
                  [(not (string=? (game-is-terminal tiles) "")) (utility tiles)]
                  [else (apply max
                          (map (lambda(tiles) (get-min-score tiles (sub1 depth)))
                               (get-board-combinations tiles O)))]))
          (define (get-min-score tiles depth)
            (cond [(= depth 0) 0]
                  [(not (string=? (game-is-terminal tiles) "")) (utility tiles)]
                  [else (apply min
                          (map (lambda(tiles) (get-max-score tiles (sub1 depth)))
                               (get-board-combinations tiles X)))]))]
    
    (foldr (λ (pos previous)
             (if (> (get-min-score (write-to-board tiles O pos) depth)
                    (get-min-score (write-to-board tiles O previous) depth))
                 pos
                 previous)) random-position empty-positions)))


(main START)