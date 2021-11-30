

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 4_DanielB_BenjaminA|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)





(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))


(define bd-img-source .)
(define x-img-source .)
(define o-img-source .)

(define bd-img (scale (/ SIZE (image-width bd-img-source)) bd-img-source))
(define x-img (scale
               (* 0.75 (/ SIZE (image-width bd-img-source))) x-img-source))
(define o-img (scale
               (* 0.75 (/ SIZE (image-width bd-img-source))) o-img-source))

(define X 1)
(define O 2)
(define B false) 

(define-struct ws (board depth))





(define START (make-ws (list B B B
                             B B B
                             B B B) 0)) 

(define BDE (list B B B
                  B B B
                  B B B))  

(define BD1 (list X B B
                  O O X
                  B B B))

(define BD2 (list O B X
                  X X O
                  O B B))

(define BD3 (list X X B
                  O X B
                  X O O))

(define BD4 (list O O B
                  O X X
                  X X B))

(define BDX (list O B X
                  X X X
                  O O B))

(define BDO (list O B X
                  O X O
                  O X B))

(define BDD (list O X O
                  X O O
                  X O X))





(check-expect (winning-cells BDE) empty)
(check-expect (winning-cells BD1) empty)
(check-expect (winning-cells BDX) (list 3 4 5))
(check-expect (winning-cells BDO) (list 0 3 6))

(define (winning-cells board) 
  
  (local 
    [(define win-positions (list 
                            (list 0 1 2)
                            (list 3 4 5)
                            (list 6 7 8)
                            (list 0 3 6)
                            (list 1 4 7)
                            (list 2 5 8)
                            (list 0 4 8)
                            (list 2 4 6)))
     (define (cells-equal? cell-indexes board)
       (if (and (equal? (list-ref board (first cell-indexes)) 
                        (list-ref board (second cell-indexes)))
                (equal? (list-ref board (second cell-indexes)) 
                        (list-ref board (third cell-indexes))))
           (list-ref board (first cell-indexes))
           false
           )
       )
     
     (define (winning-cells--inner board wps) 
       (cond [(empty? wps) empty]
             [else (if (not (false? (cells-equal? (first wps) board)))
                       (first wps)
                       (winning-cells--inner board (rest wps))
                       )]))]
    (winning-cells--inner board win-positions)))
  
  




(define (winner board) 
  
  (if (not (empty? (winning-cells board)))
      (list-ref board (first (winning-cells board)))
      false))





(define (blank-count board) 
  
  (length (filter false? board)))

(check-expect (blank-count BD1) 5) 





(define (cell-posn index) 
  
  (local [(define (scale-posn input-posn offset) 
            (make-posn (+ offset (* SIZE (/ (posn-x input-posn) 3)))
                       (+ offset (* SIZE (/ (posn-y input-posn) 3)))))]
    (scale-posn (make-posn (modulo index 3) (floor (/ index 3))) (/ SIZE 6))))

(check-expect (cell-posn 7)
              (make-posn (+ (/ SIZE 6) (* SIZE (/ 1 3)))
                         (+ (/ SIZE 6) (* SIZE (/ 2 3))))) 
(check-expect (cell-posn 0)
              (make-posn (+ (/ SIZE 6) (* SIZE (/ 0 3)))
                         (+ (/ SIZE 6) (* SIZE (/ 0 3))))) 

(check-expect (cell-posn 3)
              (make-posn (+ (/ SIZE 6) (* SIZE (/ 0 3)))
                         (+ (/ SIZE 6) (* SIZE (/ 1 3)))))


               





(check-random (handle-mouse (make-ws BDE 0) (/ SIZE 6) (/ SIZE 8) "button-down")
              (make-ws (Ai-move (make-ws (list-set BDE 0 X)
                                         0)) 0)) 

(check-expect (handle-mouse (make-ws BDE 0) 0 0 "button-up")
              (make-ws BDE 0)) 
(check-expect (handle-mouse (make-ws BDE 0) (+ SIZE 1) 0 "button-down")
              (make-ws BDE 0)) 
                              
(define (handle-mouse ws x y mouse-event)
  (local [(define (mouse-in-window? x y)
            (and (>= x 0) (>= y 0) (< x SIZE) (< y SIZE)))
          (define (get-cell-index x y)
            (+ (floor (/ x (/ SIZE 3)))
               (* 3 (floor (/ y (/ SIZE 3))))))]
    (if (and (member (get-cell-index x y)
                     (blank-indexes (ws-board ws)))
             (equal? mouse-event "button-down")
             (mouse-in-window? x y))
        (local
          [(define next-board (list-set (ws-board ws) (get-cell-index x y) X))]
          (make-ws 
           (if (end-condition? (make-ws next-board 0))
               next-board
               (Ai-move (make-ws next-board (ws-depth ws))))
           (ws-depth ws)))
        ws)
    )
  )




(define (handle-render ws)
  (local
    [
     (define (render-pieces board)
       (local [(define (cell-image board cell-index)
                 (cond [(equal? (list-ref board cell-index) X) x-img]
                       [(equal? (list-ref board cell-index) O) o-img]
                       [else empty-image]))]
         (place-images/align (map cell-image
                                  (make-list 9 board)
                                  (build-list 9 identity))
                             (map cell-posn (build-list 9 identity))
                             "center" "center" MTS)))]
    (overlay bd-img (render-pieces (ws-board ws)))))

(check-expect (handle-render (make-ws BDE 0))
              (overlay bd-img MTS)) 



(define (blank-indexes board) 
  
  (foldr (lambda (value acc) 
           (if (false? (list-ref board value))
               (cons value acc)
               acc)) empty (build-list 9 identity)))

(check-expect (blank-indexes BDE) (list 0 1 2 3 4 5 6 7 8))
(check-expect (blank-indexes BD1) (list 1 2 6 7 8))







(check-random (Ai-move (make-ws BDE 0))
              (list-set BDE (list-ref (blank-indexes BDE)
                                      (random (blank-count BDE))) O))
(check-random (Ai-move (make-ws BD1 0))
              (list-set BD1 (list-ref (blank-indexes BD1)
                                      (random (blank-count BD1))) O))
(check-random (Ai-move (make-ws BD2 0))
              (list-set BD2 (list-ref (blank-indexes BD2)
                                      (random (blank-count BD2))) O))
(check-expect (Ai-move (make-ws BD4 1)) 
              (list-set BD4 2 O))
(check-expect (Ai-move (make-ws BD4 2)) 
              (list-set BD4 2 O))
(check-expect (Ai-move (make-ws BD3 2)) 
              (list-set BD3 2 O))
(check-random (Ai-move (make-ws BD1 2)) 
              (list-set BD1 (list-ref (blank-indexes BD1)
                                      (random (blank-count BD1))) O))
 
(define (Ai-move ws) 
  
  (cond
    [(= (ws-depth ws) 0)
     (list-set (ws-board ws) (list-ref (blank-indexes (ws-board ws))
                                       (random (blank-count
                                                (ws-board ws)))) O)]
    [else 
     (local [(define (move-helper board depth) 
               (cond [(= depth 1)
                      (list-set board (find-board board max O) O) 
                      ]
                     [(= depth 2)
                      (cond [(= 1 (board-score
                                   (list-set board
                                             (find-board board max O) O)))
                             (list-set board (find-board board max O) O)]
                            [(= -1 (board-score
                                    (list-set board
                                              (find-board board min X) X)))
                             (list-set board (find-board board min X) O)]
                            [else (Ai-move (make-ws board 0))])
                      ]
                     )
               )
             
             (define (find-board board comp-fn piece) 
               (list-ref
                (blank-indexes board)
                (comp-index comp-fn
                            (map board-score
                                 (map (lambda (cell-index)
                                        (list-set board cell-index piece))
                                      (blank-indexes board)))) 
                ))
             (define (comp-index comp-fn lon)
               (index-of lon (apply comp-fn lon)))
             (define (board-score board)
               (local [(define this-board-winner (winner board))]
                 (cond
                   [(equal? this-board-winner X) -1]
                   [(equal? this-board-winner O) 1]
                   [else 0])))]
          
       (move-helper (ws-board ws) (ws-depth ws))) ]))




(check-expect (handle-last-image (make-ws BDO 0))
              (add-line (handle-render (make-ws BDO 0)) 
                        (posn-x (cell-posn 0)) (posn-y (cell-posn 0))
                        (posn-x (cell-posn 6)) (posn-y (cell-posn 6))
                        (make-pen "RED" (/ SIZE 50) "solid" "round" "round")))


(check-expect (handle-last-image (make-ws BDD 0))
              (overlay (text "Draw!" (/ SIZE 5) "PURPLE")
                       (handle-render (make-ws BDD 0))))


(check-expect (handle-last-image (make-ws BDX 0))
              (add-line (handle-render (make-ws BDX 0)) 
                        (posn-x (cell-posn 3)) (posn-y (cell-posn 3))
                        (posn-x (cell-posn 5)) (posn-y (cell-posn 5))
                        (make-pen "BLUE" (/ SIZE 50) "solid" "round" "round")))
 
(define (handle-last-image ws)
  (if (empty? (winning-cells (ws-board ws)))
      (overlay (text "Draw!" (/ SIZE 5) "PURPLE") (handle-render ws)) 
      (local 
        [(define line-cells (winning-cells (ws-board ws)))
         (define line-color (if (= (list-ref (ws-board ws)
                                             (first line-cells)) X)
                                "BLUE"
                                "RED"
                                ))
         (define cell-A (cell-posn (first line-cells)))
         (define cell-B (cell-posn (third line-cells)))]
        (add-line (handle-render ws) 
                  (posn-x cell-A) (posn-y cell-A)
                  (posn-x cell-B) (posn-y cell-B)
                  (make-pen line-color (/ SIZE 50) "solid" "round" "round"))
        )
      ))

(define (end-condition? ws)
  (or (not (empty? (winning-cells (ws-board ws))))
      (= 0 (blank-count (ws-board ws)))))

(check-expect (end-condition? START) false) 
(check-expect (end-condition? (make-ws BDX 0)) true) 
(check-expect (end-condition? (make-ws BDO 0)) true) 
(check-expect (end-condition? (make-ws BDD 0)) true) 
(check-expect (end-condition? (make-ws BD1 2)) false) 
              

(define (handle-key ws key-event)
  (if (member key-event (map number->string (build-list 3 identity)))
      (make-ws (ws-board ws) (string->number key-event))
      ws))

(check-expect (handle-key START "1") (make-ws BDE 1))
(check-expect (handle-key START "A") START)
(check-expect (handle-key START " ") START)
(check-expect (handle-key START "3") START)
(check-expect (handle-key (make-ws BDE 5) "0") (make-ws BDE 0))
       
(define (main ws)
  (big-bang ws
    (to-draw handle-render)
    (on-mouse handle-mouse)
    (on-key handle-key)
    (stop-when end-condition?
               handle-last-image)
    (name "Tic Tac Toe")
    
    ))




