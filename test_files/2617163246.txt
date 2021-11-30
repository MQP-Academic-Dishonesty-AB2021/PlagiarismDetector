

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tic tac toe starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))








(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 



(define SIZE 533) 
(define MTS (empty-scene SIZE SIZE))

(define LINE-SIZE (quotient SIZE 40))
(define LINE-PEN (make-pen "gray" LINE-SIZE "solid" "round" "round"))
(define LINE-START (quotient SIZE 10))

(define LINE-SPACING (quotient (- SIZE (* LINE-START 2)) 3))
(define WIN-TEXT-SIZE (quotient SIZE 7))
(define X-O-SIZE (quotient SIZE 10))

(define X-COLOR "red")
(define O-COLOR "blue")

(define INDICES (build-list 9 identity))


(define WIN-STATES
  (list
    (list 1 1 1 0 0 0 0 0 0)  
    (list 0 0 0 1 1 1 0 0 0)
    (list 0 0 0 0 0 0 1 1 1)
    (list 1 0 0 1 0 0 1 0 0)
    (list 0 1 0 0 1 0 0 1 0)
    (list 0 0 1 0 0 1 0 0 1)
    (list 1 0 0 0 1 0 0 0 1)
    (list 0 0 1 0 1 0 1 0 0)))









(define-struct tt-state (board win-state depth))

 










(define-struct board (spaces player))

 


(define WS1 (make-tt-state (make-board (list 0 0 0 0 0 0 0 0 0) 0) 0 0))
(define WS2 (make-tt-state (make-board (list 1 0 0 0 0 0 0 0 0) 1) 0 1))
(define WS3 (make-tt-state (make-board (list 1 1 1 0 0 0 0 2 2) 1) 1 2))
(define WS4 (make-tt-state (make-board (list 2 2 2 1 0 0 0 1 1) 0) -1 5))
(define WS5 (make-tt-state (make-board (list 1 2 1 1 2 2 2 1 2) 0) 0 9))
(define WS6 (make-tt-state (make-board (list 1 2 0 1 2 0 2 1 2) 0) 0 9))
(define WS7 (make-tt-state (make-board (list 2 2 2 1 0 0 0 1 1) 0) 0 5))

(define bd1 (make-board (list 0 0 0 0 0 0 0 0 0) 0))
(define bd2 (make-board (list 1 2 1 0 2 0 0 0 0) 1))
(define bd3 (make-board (list 0 1 0 2 0 1 2 0 0) 0))
(define bd4 (make-board (list 0 1 0 0 2 0 1 0 2) 1))
(define bd5 (make-board (list 1 1 1 0 0 0 0 2 2) 1))
(define bd6 (make-board (list 2 2 2 1 0 0 0 1 1) 0))
(define bd7 (make-board (list 1 2 1 1 2 2 2 1 2) 0))


(define START (make-tt-state (make-board (list 0 0 0 0 0 0 0 0 0) 0) 0 0))





(define (main ws)
    (big-bang ws
        (on-tick tick-tt)
        (to-draw draw-tt)
        (on-mouse handle-mouse)
        (on-key handle-key)))











(check-expect (length (next-moves (tt-state-board (tick-tt WS2))))
              (sub1 (length (next-moves (tt-state-board WS2)))))


(check-expect (length (next-moves (tt-state-board (tick-tt WS3))))
              (length (next-moves (tt-state-board WS3))))


(check-expect (tt-state-win-state (tick-tt WS7)) -1)


(check-expect (length (next-moves (tt-state-board (tick-tt WS5))))
              (length (next-moves (tt-state-board WS5))))

(define (tick-tt ws)
    (local [(define updated-win-ws
                (make-tt-state 
                    (tt-state-board ws) 
                    (board-utility (board-spaces (tt-state-board ws)) WIN-STATES)
                    (tt-state-depth ws)))] 
            (if (and (= (board-player (tt-state-board updated-win-ws)) 1) 
                    (= (tt-state-win-state updated-win-ws) 0))
                (make-tt-state
                    (if (= (tt-state-depth ws) 0)
                        (random-player (tt-state-board ws))
                        (minimax (tt-state-board updated-win-ws) (tt-state-depth ws))) 
                    (tt-state-win-state updated-win-ws) (tt-state-depth updated-win-ws))
                updated-win-ws)))












(check-expect (draw-tt WS1) (draw-lines MTS))

(check-expect (draw-tt WS3) 
    (place-image
        (text "You Win!" WIN-TEXT-SIZE "black")
        (quotient SIZE 2) (quotient SIZE 2) 
        (draw-lines (draw-moves (board-spaces (tt-state-board WS3)) MTS))))

(check-expect (draw-tt WS4) 
    (place-image
        (text "You Lose!" WIN-TEXT-SIZE "black")
        (quotient SIZE 2) (quotient SIZE 2) 
        (draw-lines (draw-moves (board-spaces (tt-state-board WS4)) MTS))))

(check-expect (draw-tt WS5) 
    (place-image
        (text "Draw" WIN-TEXT-SIZE "black")
        (quotient SIZE 2) (quotient SIZE 2) 
        (draw-lines (draw-moves (board-spaces (tt-state-board WS5)) MTS))))

(check-expect (draw-tt WS6) (draw-lines (draw-moves (board-spaces (tt-state-board WS6)) MTS)))

(define (draw-tt ws)
    (local
      [(define (add-tt-text str img)
         (place-image
          (text str WIN-TEXT-SIZE "black")
          (quotient SIZE 2) (quotient SIZE 2) img))
      (define (draw-win-text win-state img)
                (cond 
                    [(= win-state 1) (add-tt-text "You Win!" img)]
                    [(= win-state -1) (add-tt-text "You Lose!" img)]
                    [(and (= win-state 0) (empty? (next-moves (tt-state-board ws))))
                     (add-tt-text "Draw" img)]
                    [(= win-state 0) img]))]
    (draw-win-text (tt-state-win-state ws)
                   (draw-lines (draw-moves (board-spaces (tt-state-board ws)) MTS)))))









(check-expect (draw-lines MTS)
              (add-line 
               (add-line 
                (add-line 
                 (add-line MTS 
                           (+ LINE-START LINE-SPACING) 
                           LINE-START 
                           (+ LINE-START LINE-SPACING)
                           (+ LINE-START (* 3 LINE-SPACING)) LINE-PEN) 
                 (+ LINE-START (* 2 LINE-SPACING)) 
                 LINE-START 
                 (+ LINE-START (* 2 LINE-SPACING))
                 (+ LINE-START (* 3 LINE-SPACING)) LINE-PEN)
                LINE-START
                (+ LINE-START LINE-SPACING)
                (+ LINE-START (* 3 LINE-SPACING))
                (+ LINE-START LINE-SPACING) LINE-PEN)
               LINE-START 
               (+ LINE-START (* 2 LINE-SPACING)) 
               (+ LINE-START (* 3 LINE-SPACING))
               (+ LINE-START (* 2 LINE-SPACING)) LINE-PEN))
              
(define (draw-lines img)
    (add-line 
        (add-line 
            (add-line 
                (add-line img 
                    (+ LINE-START LINE-SPACING) 
                    LINE-START 
                    (+ LINE-START LINE-SPACING)
                    (+ LINE-START (* 3 LINE-SPACING)) LINE-PEN) 
                (+ LINE-START (* 2 LINE-SPACING)) 
                LINE-START 
                (+ LINE-START (* 2 LINE-SPACING))
                (+ LINE-START (* 3 LINE-SPACING)) LINE-PEN)
            LINE-START
            (+ LINE-START LINE-SPACING)
            (+ LINE-START (* 3 LINE-SPACING))
            (+ LINE-START LINE-SPACING) LINE-PEN)
        LINE-START 
        (+ LINE-START (* 2 LINE-SPACING)) 
        (+ LINE-START (* 3 LINE-SPACING))
        (+ LINE-START (* 2 LINE-SPACING)) LINE-PEN))









(check-expect (draw-moves (list 0 0 0 0 0 0 0 0 0) MTS) MTS)

(check-expect (draw-moves (list 1 0 0 0 0 0 0 0 0) MTS) 
    (place-image
        (text "X" X-O-SIZE X-COLOR)
        (+ (+ LINE-START (/ LINE-SPACING 2)) (* (modulo 0 3) LINE-SPACING))
        (+ (+ LINE-START (/ LINE-SPACING 2)) (* (quotient 0 3) LINE-SPACING))
        MTS))

(check-expect (draw-moves (list 0 0 0 0 2 0 0 1 0) MTS) 
    (place-image
        (text "X" X-O-SIZE X-COLOR)
        (+ (+ LINE-START (/ LINE-SPACING 2)) (* (modulo 7 3) LINE-SPACING))
        (+ (+ LINE-START (/ LINE-SPACING 2)) (* (quotient 7 3) LINE-SPACING))
            (place-image
                (text "O" X-O-SIZE O-COLOR)
                (+ (+ LINE-START (/ LINE-SPACING 2)) (* (modulo 4 3) LINE-SPACING))
                (+ (+ LINE-START (/ LINE-SPACING 2)) (* (quotient 4 3) LINE-SPACING))
                 MTS)))


(define (draw-moves moves img)
  (local [(define (draw-x-or-o move)
            (cond [(= move 1) 
                   (text "X" X-O-SIZE X-COLOR)]
                  [(= move 2)
                   (text "O" X-O-SIZE O-COLOR)]
                  [else empty-image]))]
    (foldl (lambda (move index result)
             (place-image
              (draw-x-or-o move)
              (+ (+ LINE-START (/ LINE-SPACING 2)) (* (modulo index 3) LINE-SPACING))
              (+ (+ LINE-START (/ LINE-SPACING 2)) (* (quotient index 3) LINE-SPACING))
              result))
           img moves INDICES)))

  









(check-expect (handle-mouse START SIZE (quotient SIZE 2) "button-down") 
    (make-tt-state (make-board (list 0 0 0 0 0 1 0 0 0) 1) 0 0))

(check-expect (handle-mouse START 0 0 "button-down") 
    (make-tt-state (make-board (list 1 0 0 0 0 0 0 0 0) 1) 0 0))

(check-expect (handle-mouse START 0 0 "button-up") START)

(check-expect (handle-mouse START (quotient SIZE 2) (quotient SIZE 2) "button-down")
    (make-tt-state (make-board (list 0 0 0 0 1 0 0 0 0) 1) 0 0))

(define (handle-mouse ws x y mE)
  (if (and 
        (mouse=? mE "button-down") 
        (= (tt-state-win-state ws) 0) 
        (= (board-player (tt-state-board ws)) 0))
    (local [(define (between n a b) (cond [(> a n) a] [(< b n) b] [else n]))
            (define index
                (+ (between (quotient (- x LINE-START) LINE-SPACING) 0 2)
                (* 3 (between (quotient (- y LINE-START) LINE-SPACING) 0 2))))]
            (if (= 0 (list-ref (board-spaces (tt-state-board ws)) index))
                (make-tt-state (add-move (tt-state-board ws) index) 0 (tt-state-depth ws))
                ws)) ws))










(check-expect (handle-key START "g")
              START)

(check-expect (handle-key START "0")
              (make-tt-state (make-board (list 0 0 0 0 0 0 0 0 0) 0) 0 0))

(check-expect (handle-key START "5")
              (make-tt-state (make-board (list 0 0 0 0 0 0 0 0 0) 0) 0 5))

(define (handle-key ws kE)
    (local 
        [(define (handle-key-inner ws kE acc)
            (cond [(key=? kE (number->string acc))
                   (make-tt-state
                    (tt-state-board ws)
                    (tt-state-win-state ws)
                    acc)]
                  [(> acc 8) ws]
                  [else (handle-key-inner ws kE (add1 acc))]))]
        (handle-key-inner ws kE 0)))











(check-expect (board-utility (list 1 2 2 1 2 1 1 2 2) WIN-STATES) 1)

(check-expect (board-utility (list 2 1 1 0 2 1 0 1 2) WIN-STATES) -1)

(check-expect (board-utility (list 0 1 0 0 0 0 0 1 0) WIN-STATES) 0)

(define (board-utility moves win-states)
    (cond
        [(empty? win-states) 0]
         
        
        [else
            (local [(define filtered-moves
                      (filter (lambda (n) (> n 0))
                              (map * moves (first win-states))))]
                (cond 
                    [(equal? filtered-moves (list 1 1 1)) 1]
                    [(equal? filtered-moves (list 2 2 2)) -1]
                    [else (board-utility moves (rest win-states))]))]))









(check-expect (add-move bd1 2) (make-board (list 0 0 1 0 0 0 0 0 0) 1))

(check-expect (add-move bd2 3) (make-board (list 1 2 1 2 2 0 0 0 0) 0))

(check-expect (add-move bd3 8) (make-board (list 0 1 0 2 0 1 2 0 1) 1))

(check-expect (add-move bd7 1) (make-board (list 1 1 1 1 2 2 2 1 2) 1))

(define (add-move board pos)
  (make-board (map (lambda (move index)
                     (if (= index pos)
                         (+ 1 (board-player board))
                         move))
                   (board-spaces board)
                   INDICES)
              (cond [(= (board-player board) 0) 1] [else 0])))











(check-expect (next-moves bd1) (list 
                                    (make-board (list 1 0 0 0 0 0 0 0 0) 1)
                                    (make-board (list 0 1 0 0 0 0 0 0 0) 1)
                                    (make-board (list 0 0 1 0 0 0 0 0 0) 1)
                                    (make-board (list 0 0 0 1 0 0 0 0 0) 1)
                                    (make-board (list 0 0 0 0 1 0 0 0 0) 1)
                                    (make-board (list 0 0 0 0 0 1 0 0 0) 1)
                                    (make-board (list 0 0 0 0 0 0 1 0 0) 1)
                                    (make-board (list 0 0 0 0 0 0 0 1 0) 1)
                                    (make-board (list 0 0 0 0 0 0 0 0 1) 1)))

(check-expect (next-moves bd2) (list 
                                    (make-board (list 1 2 1 2 2 0 0 0 0) 0)
                                    (make-board (list 1 2 1 0 2 2 0 0 0) 0)
                                    (make-board (list 1 2 1 0 2 0 2 0 0) 0)
                                    (make-board (list 1 2 1 0 2 0 0 2 0) 0)
                                    (make-board (list 1 2 1 0 2 0 0 0 2) 0)))

(check-expect (next-moves bd3) (list 
                                    (make-board (list 1 1 0 2 0 1 2 0 0) 1)
                                    (make-board (list 0 1 1 2 0 1 2 0 0) 1)
                                    (make-board (list 0 1 0 2 1 1 2 0 0) 1)
                                    (make-board (list 0 1 0 2 0 1 2 1 0) 1)
                                    (make-board (list 0 1 0 2 0 1 2 0 1) 1)))

(check-expect (next-moves bd7) empty)

(define (next-moves board)
    (map (lambda (n)
            (add-move board n))
    (filter (lambda (n) (>= n 0))
        (map (lambda (space index)
                    (if (zero? space) index -1))
                (board-spaces board)
                INDICES))))









(check-expect (length
               (filter zero? (board-spaces (random-player
                                            (make-board (list 0 1 0 0 0 0 0 0 0) 1)))))
              (- 9 2))

(check-expect (length
               (filter zero? (board-spaces (random-player
                                            (make-board (list 0 1 0 1 0 2 0 0 0) 1)))))
              (- 9 4))


(check-expect (random-player (make-board (list 2 1 2 1 1 2 1 2 1) 1))
              (make-board (list 2 1 2 1 1 2 1 2 1) 1))

(define (random-player board)
    (local [(define available-moves (next-moves board))]
        (if (not (empty? available-moves))
            (list-ref available-moves (random (length available-moves)))
            board)))









(check-expect (minimax (make-board (list 0 0 1 0 0 1 0 0 0) 1) 2)
              (make-board (list 0 0 1 0 0 1 0 0 2) 0))

(check-expect (minimax (make-board (list 2 0 2 1 1 0 0 0 0) 1) 2)
              (make-board (list 2 2 2 1 1 0 0 0 0) 0))

(check-expect (minimax (make-board (list 0 2 1 0 2 1 0 0 0) 1) 6)
              (make-board (list 0 2 1 0 2 1 0 2 0) 0))

(check-expect (minimax (make-board (list 1 0 0 1 2 0 0 0 0) 1) 6)
              (make-board (list 1 0 0 1 2 0 2 0 0) 0))

(check-expect (minimax (make-board (list 1 0 0 0 0 0 0 0 0) 1) 6)
              (make-board (list 1 0 0 0 2 0 0 0 0) 0))

(check-expect (minimax (make-board (list 1 2 0 0 2 0 0 1 1) 1) 6)
              (make-board (list 1 2 0 0 2 0 2 1 1) 0))

(check-expect (minimax (make-board (list 1 1 0 0 2 1 0 0 2) 1) 6)
              (make-board (list 1 1 2 0 2 1 0 0 2) 0))





(define (minimax board-orig orig-depth)
  (local
    
    
    [(define-struct mm-data (board value))
     (define (minimax-inner board depth)
       
       
       
       (if (or (= depth 0)
               (not (= 0 (board-utility (board-spaces board) WIN-STATES)))
               (empty? (next-moves board)))
           (if (= depth orig-depth)
               board
               (* -1 (board-utility (board-spaces board) WIN-STATES)))
           (local
             
             
             [(define next-possible-moves 
                (map (lambda (b)
                       (make-mm-data b (minimax-inner b (- depth 1))))
                     (next-moves board)))
              
              
              (define (next-best-move op base)
                (foldl (lambda (set prev)
                         (if (op (mm-data-value set) (mm-data-value prev)) 
                             set prev))
                       (make-mm-data 0 base) next-possible-moves))]
             
             
             (if (= (board-player board) 1)
                 (local [(define next-max-move (next-best-move > -2))]
                   (if (= depth orig-depth)
                       (mm-data-board next-max-move)
                       (mm-data-value next-max-move)))
                 (local [(define next-min-move (next-best-move < 2))]
                   (if (= depth orig-depth)
                       (mm-data-board next-min-move)
                       (mm-data-value next-min-move)))))))]
    (minimax-inner board-orig orig-depth)))
                

(main START)