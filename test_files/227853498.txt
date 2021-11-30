

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment-4-Final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 600) 
(define MTS (empty-scene SIZE SIZE))

(define LINE-WIDTH (round (/ SIZE 80)))
(define BOARD-PEN (make-pen "black" LINE-WIDTH "solid" "round" "bevel"))
(define X-PEN (make-pen "red" LINE-WIDTH "solid" "round" "bevel"))
(define O-PEN (make-pen "blue" LINE-WIDTH "solid" "round" "bevel"))
(define DEPTH-COUNTER-X (/ SIZE 9))
(define DEPTH-COUNTER-Y (/ SIZE 9))
(define LEFT-TOP (/ SIZE 5))
(define MIDDLE (/ SIZE 2))
(define RIGHT-BOT (- SIZE LEFT-TOP))
(define EMPTY-BOARD
  (add-line (add-line (add-line (add-line MTS (/ SIZE 20) (* SIZE (/ 7 20))
                                          (* SIZE (/ 19 20)) (* SIZE (/ 7 20)) BOARD-PEN)
                                (/ SIZE 20) (* SIZE (/ 13 20))
                                (* SIZE (/ 19 20)) (* SIZE (/ 13 20)) BOARD-PEN)
                      (* SIZE (/ 13 20)) (/ SIZE 20)
                      (* SIZE (/ 13 20)) (* SIZE (/ 19 20)) BOARD-PEN)
            (* SIZE (/ 7 20)) (/ SIZE 20)
            (* SIZE (/ 7 20)) (* SIZE (/ 19 20)) BOARD-PEN)
  )
(define X (add-line (line (/ SIZE 5) (/ SIZE 5) X-PEN) 0 (/ SIZE 5) (/ SIZE 5) 0 X-PEN))
(define O (circle (/ SIZE 10) "outline" O-PEN))
(define-struct coordinates (x y))
(define POSITIONS (list (make-coordinates LEFT-TOP LEFT-TOP)
                        (make-coordinates MIDDLE LEFT-TOP)
                        (make-coordinates RIGHT-BOT LEFT-TOP)
                        (make-coordinates LEFT-TOP MIDDLE)
                        (make-coordinates MIDDLE MIDDLE)
                        (make-coordinates RIGHT-BOT MIDDLE)
                        (make-coordinates LEFT-TOP RIGHT-BOT)
                        (make-coordinates MIDDLE RIGHT-BOT)
                        (make-coordinates RIGHT-BOT RIGHT-BOT)))





(define-struct ws (board gamestate turn depth))

(define START (make-ws (list "" "" "" "" "" "" "" "" "") false 0 0))





(define (main ws)
  (big-bang ws
    (on-tick   smart-ai)
    (to-draw   draw-board)
    (on-key    adjust-ai)
    (on-mouse  human-move)
    (stop-when (λ (ws)
                 (not (false? (game-over? ws))))
               (λ (ws)
                 (overlay (text (end-text ws) (/ SIZE 10) "GREEN") (draw-board ws))))
    ))





(define (draw-board ws)
  (local [
          (define (draw-move moves cpa)
            (cond
              [(empty? moves) EMPTY-BOARD]
              [(not (string=? (first moves) ""))
               (add-move (first moves) cpa (draw-move (rest moves) (+ 1 cpa)))]
              [else (draw-move (rest moves) (+ 1 cpa))]))
          
          (define (add-move move-type placement background)
            (if (string=? move-type "X")
                (place-image X (coordinates-x (get-coord placement POSITIONS))
                             (coordinates-y (get-coord placement POSITIONS)) background)
                (place-image O (coordinates-x (get-coord placement POSITIONS))
                             (coordinates-y (get-coord placement POSITIONS)) background)))
          
          (define (get-coord placement loc)
            (if (<= placement 0)
                (first loc)
                (get-coord (- placement 1) (rest loc))))]
    
    (place-image (text (string-append "DEPTH: " (number->string (ws-depth ws))) (/ SIZE 20)
                       "BLACK") DEPTH-COUNTER-X DEPTH-COUNTER-Y (draw-move (ws-board ws) 0))))














(define (insert move board pos)
  (local [
          (define (insert-board move board pos rsf)
            (cond
              [(= rsf 9) empty]
              [(= pos rsf)
               (cons move (insert-board move (rest board) pos (+ rsf 1)))]
              [else
               (cons (first board)
                     (insert-board move (rest board) pos (+ rsf 1)))]))]
    (insert-board move board pos 0)))                 



















(define (human-move ws x y event)
  (local[
         (define (pick-square x y)
           (+ (* (quotient y (/ SIZE 3)) 3)
              (quotient x (/ SIZE 3))))
         
         (define (new-board ws square event)
           (if (string=? (list-ref (ws-board ws) square) "")
               (make-ws (insert "X" (ws-board ws) square) "" 1 (ws-depth ws))
               ws))]
    
    (if (and (mouse=? event "button-down") (= 0 (ws-turn ws)))
        (new-board ws (pick-square x y) event)
        ws)))




















(define (game-over? ws)
  (local [
          (define (rows-columns? board step)
            (local [
                    (define (rc? board step rsf)
                      (cond
                        [(= rsf (/ 9 step)) false]
                        [(string=? (list-ref board rsf) "") (rc? board step (+ rsf (/ 3 step)))]
                        [(string=? (list-ref board rsf)
                                   (list-ref board (+ (* 1 step) rsf))
                                   (list-ref board (+ (* 2 step) rsf)))
                         (list-ref board rsf)]
                        [else (rc? board step (+ rsf (/ 3 step)))]))]
              (rc? board step 0)))
          
          (define rows? (λ (ws) (rows-columns? (ws-board ws) 1)))
          
          (define columns? (λ (ws) (rows-columns? (ws-board ws) 3)))

          (define (diagonals? ws)
            (cond
              [(string=? (list-ref (ws-board ws) 4) "") false]
              [(or (string=? (list-ref (ws-board ws) 0)
                             (list-ref (ws-board ws) 4)
                             (list-ref (ws-board ws) 8))
                   (string=? (list-ref (ws-board ws) 2)
                             (list-ref (ws-board ws) 4)
                             (list-ref (ws-board ws) 6)))
               (list-ref (ws-board ws) 4)]
              [else false]))]
    (cond
      [(not (false? (rows? ws))) (make-ws (ws-board ws) (rows? ws) -1 -1)]
      [(not (false? (columns? ws))) (make-ws (ws-board ws) (columns? ws) -1 -1)]
      [(not (false? (diagonals? ws))) (make-ws (ws-board ws) (diagonals? ws) -1 -1)]
      [(false? (member? "" (ws-board ws))) (make-ws (ws-board ws) "D" -1 -1)]
      [else false]
      )))









(define (end-text ws)
  (cond
    [(string=? (ws-gamestate (game-over? ws)) "D") "DRAW!"]
    [else (string-append (ws-gamestate (game-over? ws)) " WINS!")]))












(define (adjust-ai ws num)
  (cond
    [(number? (string->number num))
     (make-ws (ws-board ws) (ws-gamestate ws) (ws-turn ws) (string->number num))]
    [else ws]))











(define (smart-ai ws)
  (local [
          (define (find-open-pos board)
            (local [
                    (define (find-open board pos)
                      (cond
                        [(>= pos 9) empty]
                        [(string=? "" (list-ref board pos)) (cons pos (find-open board (add1 pos)))]
                        [else (find-open board (add1 pos))]))]
              (find-open board 0)))
          
          (define (gen-rand-pos board)
            (local [
                    (define open (find-open-pos board))]
              (list-ref open (random (length open)))))
          
          (define (random-move ws)
            (make-ws (insert "O" (ws-board ws) (gen-rand-pos (ws-board ws))) "" 0 (ws-depth ws)))

          (define (next-boards move ws lop)
            (cond
              [(empty? lop) empty]
              [else (cons (make-ws (insert move (ws-board ws) (first lop))
                                   (ws-gamestate ws) (add1 (ws-turn ws)) (ws-depth ws))
                          (next-boards move ws (rest lop)))]))
          
          (define possible-wins (next-boards "O" ws (find-open-pos (ws-board ws))))
          
          (define possible-losses (next-boards "X" ws (find-open-pos (ws-board ws))))]

    (if (= 1 (ws-turn ws))
        (cond
          [(= (ws-depth ws) 0)
           (random-move ws)]
          [(= (ws-depth ws) 1)
           (cond
             [(ormap (λ (x) (not (false? (game-over? x)))) possible-wins)
              (first (filter (λ (x) (string=? "O" (ws-gamestate (game-over? x))))
                             (filter (λ (x) (not (false? (game-over? x)))) possible-wins)))]
             [else (random-move ws)])]
          [(>= (ws-depth ws) 2)
           (cond
             [(ormap (λ (x) (not (false? (game-over? x)))) possible-wins)
              (first (filter (λ (x) (string=? "O" (ws-gamestate (game-over? x))))
                             (filter (λ (x) (not (false? (game-over? x)))) possible-wins)))]
             
             [(ormap (λ (x) (not (false? (game-over? x)))) possible-losses)
              (make-ws (insert "O" (ws-board ws)
                               (first (filter
                                       (λ (pos)
                                         (not (false? (game-over?
                                                       (make-ws (insert "X" (ws-board ws) pos)
                                                                (ws-gamestate ws)
                                                                (- (ws-turn ws) 1)
                                                                (ws-depth ws))))))
                                       (find-open-pos (ws-board ws)))))
                       (ws-gamestate ws)
                       (- (ws-turn ws) 1)
                       (ws-depth ws))]
             
             [else (random-move ws)])]
          )
        ws)))


