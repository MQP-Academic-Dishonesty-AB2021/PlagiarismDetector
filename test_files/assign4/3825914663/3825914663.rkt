

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname TicTacToe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)



(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))
(define SQUARE (square (quotient SIZE 3) "outline" "black"))
(define BOARD (above (beside SQUARE SQUARE SQUARE)
                     (beside SQUARE SQUARE SQUARE)
                     (beside SQUARE SQUARE SQUARE)))
(define WINS (list (list 0 1 2)
                   (list 3 4 5)
                   (list 6 7 8)
                   (list 0 3 6)
                   (list 1 4 7)
                   (list 2 5 8)
                   (list 0 4 8)
                   (list 2 4 6))) 













(define-struct ws (board turn diff))

(define (fn-for-ws ws)
  (...
   (fn-for-los (ws-board ws))
   (ws-turn ws)
   (ws-diff ws)))

(define (fn-for-los los)
  (cond
    [(empty? los) ...]
    [else
     (... (first los)
          (fn-for-los (rest los)))]))

(define START                    
  (make-ws (list "" "" ""
                 "" "" ""
                 "" "" "")
           0
           0))




(define (main x)
  (big-bang x
    (on-tick update)
    (on-mouse play)
    (to-draw render)
    (on-key set-difficulty)))





(check-expect (set-difficulty START "4")
              (make-ws (ws-board START) (ws-turn START) 4)) 
(check-expect (set-difficulty START "a") START) 

(define (set-difficulty ws event)
  (local [(define num (string->number event))]
    (if (not(false? num))
        (make-ws (ws-board ws) (ws-turn ws) num)
        ws)))





(check-expect (render START) (place-image (text "" 1 "white") 0 0 BOARD)) 

(define (render ws)
  (local [(define (fill-board board current-pos board-img)
            (if (empty? board)
                board-img
                (fill-board (rest board)
                            (+ current-pos 1)
                            (place-image (get-piece (first board))
                                         (pos->x current-pos)
                                         (pos->y current-pos)
                                         board-img))))
          (define (get-piece letter)
            (text letter (/ SIZE 4) "red"))
          (define (pos->x pos)
            (* SIZE (/ (+ 1 (* 2 (modulo pos 3))) 6)))
          (define (pos->y pos)
            (* SIZE (/ (+ 1 (* 2 (quotient pos 3))) 6)))]
    (if (< (ws-turn ws) 2)
        (fill-board (ws-board ws) 0 BOARD)
        (overlay (text (cond [(= (ws-turn ws) 2) "X wins"]
                             [(= (ws-turn ws) 3) "O wins"]
                             [(= (ws-turn ws) 4) "Draw"])
                       (quotient SIZE 4)
                       "Green")
                 (fill-board (ws-board ws) 0 BOARD)))))






(define (update ws)
  (local [(define winner (three-in-a-row (ws-board ws)))]
    (cond
      [(not (false? winner)) (make-ws (ws-board ws) winner (ws-diff ws))]
      [(= 1 (ws-turn ws)) (minimax-computer-turn ws)]
      [else ws])))




(check-expect (equall empty) true) 
(check-expect (equall (list "hi" "hi" "hi")) true) 
(check-expect (equall (list 1 2 1 1 1)) false) 

(define (equall list)
  (if (or (empty? list) (empty? (rest list))) true
      (and (equal? (first list) (first (rest list))) (equall (rest list)))))




(define WS1 (make-ws (list "X" "" ""
                           "" "O" ""
                           "X" "" "") 1 2))
(define WS2 (make-ws (list "X" "" ""
                           "O" "O" ""
                           "X" "" "") 0 2))
(check-expect (minimax-computer-turn WS1) WS2)

(define (minimax-computer-turn ws)
  (local [(define (utility board) 
            (cond [(false? (three-in-a-row board)) 0]
                  [(= 2 (three-in-a-row board)) 1]
                  [(= 3 (three-in-a-row board)) -1]
                  [else 0]))
          (define (computer-turn ws) 
            (make-ws (place-piece ws (first-empty-square (ws-board ws) 0))
                     0
                     (ws-diff ws)))
          (define (first-empty-square board pos) 
            (if (or (empty? board) (equal? (first board) ""))
                pos
                (first-empty-square (rest board) (+ pos 1))))
          (define (max-player-turn ws turns) 
            (if (= turns 1)
                (argmax (lambda (state) (utility (ws-board state)))
                        (all-next-moves ws))
                (argmax (lambda (state)
                          (if (= (utility (ws-board state)) 0)
                              (utility (ws-board (min-computer-turn state (sub1 turns))))
                              (* (+ 1 (num-empty-squares (ws-board state)))
                                 (utility (ws-board state)))))
                        (all-next-moves ws))))
          (define (min-computer-turn ws turns) 
            (if (= turns 1)
                (argmin (lambda (state) (utility (ws-board state)))
                        (all-next-moves ws))
                (argmin (lambda (state)
                          (if (= (utility (ws-board state)) 0)
                              (utility (ws-board (max-player-turn state (sub1 turns))))
                              (* (+ 1 (num-empty-squares (ws-board state)))
                                 (utility (ws-board state)))))
                        (all-next-moves ws))))
          (define (all-next-moves ws) 
            (foldr (lambda (pos res)
                     (if (equal? (list-ref (ws-board ws) pos) "")
                         (cons (make-ws (place-piece ws pos)
                                        (abs (sub1 (ws-turn ws)))
                                        (ws-diff ws))
                               res)
                         res))
                   empty (build-list 9 identity)))
          (define (num-empty-squares board) 
            (cond [(empty? board) 0]
                  [(equal? "" (first board)) (+ 1 (num-empty-squares (rest board)))]
                  [else (num-empty-squares (rest board))]))]
    (if (= (ws-diff ws) 0)
        (computer-turn ws)
        (min-computer-turn ws (apply min (list (num-empty-squares (ws-board ws))
                                               (ws-diff ws)))))))







(check-expect (three-in-a-row (ws-board START)) false) 
(check-expect (three-in-a-row (list "X" "O" ""
                                    "O" "O" ""
                                    "X" "X" "X"))
              2)  

(define (three-in-a-row board)
  (cond
    [(ormap (λ (lon) (equall (list (list-ref board (list-ref lon 0))
                                   (list-ref board (list-ref lon 1))
                                   (list-ref board (list-ref lon 2))
                                   "X")))
            WINS) 2]
    [(ormap (λ (lon) (equall (list (list-ref board (list-ref lon 0))
                                   (list-ref board (list-ref lon 1))
                                   (list-ref board (list-ref lon 2))
                                   "O")))
            WINS) 3]
    [(not (ormap (λ (square) (equal? square "")) board)) 4]
    [else false]))






(check-expect (play START (/ SIZE 2) (/ SIZE 2) "button-down")
              (make-ws (list "" "" ""
                             "" "X" ""
                             "" "" "")
                       1
                       0)) 


(define (play ws x y event)
  (if (and (equal? "button-down" event)            
           (equal? "" (list-ref (ws-board ws)
                                (square-pos x y))) 
           (= (ws-turn ws) 0))                     
      (make-ws (place-piece ws (square-pos x y))
               1
               (ws-diff ws))
      ws))





(check-expect (place-piece START 4) (list "" "" ""
                                          "" "X" ""
                                          "" "" "")) 

(define (place-piece ws pos)
  (local [(define (place-piece current-pos before-list after-list)
            (if (= pos current-pos)
                (append before-list
                        (list (if (= 0 (ws-turn ws))
                                  "X"
                                  "O"))
                        (rest after-list))
                (place-piece (+ current-pos 1)
                             (append before-list (list (first after-list)))
                             (rest after-list))))]
    (place-piece 0 empty (ws-board ws))))







(check-expect (square-pos 0 0) 0) 
(check-expect (square-pos 250 150) 5) 

(define (square-pos x y)
  (+ (* 3 (quotient y (quotient SIZE 3)))
     (quotient x (quotient SIZE 3))))