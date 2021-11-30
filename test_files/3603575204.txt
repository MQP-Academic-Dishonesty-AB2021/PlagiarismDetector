

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |CS 1102 Assignment 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 


(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))
(define X (text "X" (floor (/ SIZE 5)) "red"))
(define O (text "O" (floor (/ SIZE 5)) "blue"))

(define BOARD-IMG
  (place-images
   (list (rectangle (/ SIZE 20) SIZE "solid" "Orange")
         (rectangle (/ SIZE 20) SIZE "solid" "Orange")
         (rectangle SIZE (/ SIZE 20) "solid" "Orange")
         (rectangle SIZE (/ SIZE 20) "solid" "Orange"))
   (list (make-posn (/ SIZE 3) (/ SIZE 2))
         (make-posn (* 2(/ SIZE 3)) (/ SIZE 2))
         (make-posn (/ SIZE 2) (/ SIZE 3))
         (make-posn (/ SIZE 2) (* 2 (/ SIZE 3))))
   MTS))


(define-struct block (x-min x-max y-min y-max filled num))




 


(define block1 (make-block 0 (/ SIZE 3) 0 (/ SIZE 3) "BLK" 1))
(define block2 (make-block (/ SIZE 3) (* 2 (/ SIZE 3)) 0 (/ SIZE 3) "BLK" 2))
(define block3 (make-block (* 2 (/ SIZE 3)) SIZE 0 (/ SIZE 3) "BLK" 3))

(define block4 (make-block 0 (/ SIZE 3) (/ SIZE 3) (* 2 (/ SIZE 3)) "BLK" 4))
(define block5 (make-block (/ SIZE 3) (* 2 (/ SIZE 3)) (/ SIZE 3) (* 2 (/ SIZE 3)) "BLK" 5))
(define block6 (make-block (* 2 (/ SIZE 3)) SIZE (/ SIZE 3) (* 2 (/ SIZE 3)) "BLK" 6))
  
(define block7 (make-block 0 (/ SIZE 3) (* 2 (/ SIZE 3)) SIZE "BLK" 7))
(define block8 (make-block (/ SIZE 3) (* 2 (/ SIZE 3)) (* 2 (/ SIZE 3)) SIZE "BLK" 8))
(define block9 (make-block (* 2 (/ SIZE 3)) SIZE  (* 2 (/ SIZE 3)) SIZE "BLK" 9))


(define-struct board (lob winner difficulty))





 

(define START (make-board (list block1 block2 block3 block4  block5 block6 block7 block8 block9)
                          BOARD-IMG 2))

(define 1-filled (make-board (list (make-block (block-x-min block1)
                                               (block-x-max block1)
                                               (block-y-min block1)
                                               (block-y-max block1)
                                               "X"
                                               (block-num block1))
                                   block2 block3 block4  block5 block6 block7 block8 block9)
                             "cont" 0))
(define 7-filled (make-board (list (make-block (block-x-min block1)
                                               (block-x-max block1)
                                               (block-y-min block1)
                                               (block-y-max block1)
                                               "X"
                                               (block-num block1))
                                   (make-block (block-x-min block2)
                                               (block-x-max block2)
                                               (block-y-min block2)
                                               (block-y-max block2)
                                               "O"
                                               (block-num block3))
                                   (make-block (block-x-min block3)
                                               (block-x-max block3)
                                               (block-y-min block3)
                                               (block-y-max block3)
                                               "X"
                                               (block-num block3))
                                   (make-block (block-x-min block4)
                                               (block-x-max block4)
                                               (block-y-min block4)
                                               (block-y-max block4)
                                               "X"
                                               (block-num block4))
                                   (make-block (block-x-min block5)
                                               (block-x-max block5)
                                               (block-y-min block5)
                                               (block-y-max block5)
                                               "X"
                                               (block-num block5))
                                   block6
                                   (make-block (block-x-min block7)
                                               (block-x-max block7)
                                               (block-y-min block7)
                                               (block-y-max block7)
                                               "O"
                                               (block-num block7))
                                   block8
                                   (make-block (block-x-min block9)
                                               (block-x-max block9)
                                               (block-y-min block9)
                                               (block-y-max block9)
                                               "O"
                                               (block-num block9)))
                             "cont" 2))
(define tie-board (make-board (list (make-block (block-x-min block1)
                                                (block-x-max block1)
                                                (block-y-min block1)
                                                (block-y-max block1)
                                                "X"
                                                (block-num block1))
                                    (make-block (block-x-min block2)
                                                (block-x-max block2)
                                                (block-y-min block2)
                                                (block-y-max block2)
                                                "X"
                                                (block-num block2))
                                    (make-block (block-x-min block3)
                                                (block-x-max block3)
                                                (block-y-min block3)
                                                (block-y-max block3)
                                                "O"
                                                (block-num block3))
                                    (make-block (block-x-min block4)
                                                (block-x-max block4)
                                                (block-y-min block4)
                                                (block-y-max block4)
                                                "O"
                                                (block-num block4))
                                    (make-block (block-x-min block5)
                                                (block-x-max block5)
                                                (block-y-min block5)
                                                (block-y-max block5)
                                                "X"
                                                (block-num block5))
                                    (make-block (block-x-min block6)
                                                (block-x-max block6)
                                                (block-y-min block6)
                                                (block-y-max block6)
                                                "X"
                                                (block-num block6))
                                    (make-block (block-x-min block7)
                                                (block-x-max block7)
                                                (block-y-min block7)
                                                (block-y-max block7)
                                                "X"
                                                (block-num block7))
                                    (make-block (block-x-min block8)
                                                (block-x-max block8)
                                                (block-y-min block8)
                                                (block-y-max block8)
                                                "O"
                                                (block-num block8))
                                    (make-block (block-x-min block9)
                                                (block-x-max block9)
                                                (block-y-min block9)
                                                (block-y-max block9)
                                                "O"
                                                (block-num block9)))
                              "cont" 0))



(define (main board)
  (big-bang board
    (stop-when halt renderboard)  
    (on-mouse move)               
    (to-draw renderboard)         
    (on-key level)                
    ))













(define (move board-in x y me)
  (if (equal? me "button-down") 
      (local [
              (define (check-block block x y)
                (and
                 (< (block-x-min block) x)
                 (> (block-x-max block) x)
                 (< (block-y-min block) y)
                 (> (block-y-max block) y)))
          
              (define clicked (first (filter (λ (b-in) (check-block b-in x y))
                                             (board-lob board-in)))) 
              (define (move0 board-in x y me) 
                (if (equal? (block-filled clicked) "BLK")
                    (make-board (cons (make-block (block-x-min clicked)
                                                  (block-x-max clicked)
                                                  (block-y-min clicked)
                                                  (block-y-max clicked)
                                                  "X"
                                                  (block-num clicked))
                                      (filter (λ (b-in) (not(equal? (block-num clicked)
                                                                    (block-num b-in))))
                                              (board-lob board-in)))
                                (board-winner board-in)
                                (board-difficulty board-in))
                    board-in))
              (define new-board (move0 board-in x y me))]
        (if (equal? new-board board-in)
            board-in
            (comp-turn new-board)))
      board-in))









(check-expect (comp-turn (make-board (list (make-block (block-x-min block1)
                                                       (block-x-max block1)
                                                       (block-y-min block1)
                                                       (block-y-max block1)
                                                       "X"
                                                       (block-num block1))
                                           (make-block (block-x-min block2)
                                                       (block-x-max block2)
                                                       (block-y-min block2)
                                                       (block-y-max block2)
                                                       "X"
                                                       (block-num block2))
                                           block3 block4  block5 block6 block7 block8 block9)
                                     "cont" 2))
              (make-board (list (make-block (block-x-min block3)
                                            (block-x-max block3)
                                            (block-y-min block3)
                                            (block-y-max block3)
                                            "O"
                                            (block-num block3))
                                (make-block (block-x-min block1)
                                            (block-x-max block1)
                                            (block-y-min block1)
                                            (block-y-max block1)
                                            "X"
                                            (block-num block1))
                                (make-block (block-x-min block2)
                                            (block-x-max block2)
                                            (block-y-min block2)
                                            (block-y-max block2)
                                            "X"
                                            (block-num block2))
                                block4  block5 block6 block7 block8 block9)
                          "cont" 2))
(check-expect (comp-turn (make-board (list block1
                                           (make-block (block-x-min block2)
                                                       (block-x-max block2)
                                                       (block-y-min block2)
                                                       (block-y-max block2)
                                                       "O"
                                                       (block-num block2))
                                           (make-block (block-x-min block3)
                                                       (block-x-max block3)
                                                       (block-y-min block3)
                                                       (block-y-max block3)
                                                       "O"
                                                       (block-num block3))
                                           block4 block5
                                           (make-block (block-x-min block6)
                                                       (block-x-max block6)
                                                       (block-y-min block6)
                                                       (block-y-max block6)
                                                       "X"
                                                       (block-num block6))
                                           block7
                                           (make-block (block-x-min block8)
                                                       (block-x-max block8)
                                                       (block-y-min block8)
                                                       (block-y-max block8)
                                                       "X"
                                                       (block-num block8))
                                           (make-block (block-x-min block9)
                                                       (block-x-max block9)
                                                       (block-y-min block9)
                                                       (block-y-max block9)
                                                       "X"
                                                       (block-num block9)))
                                     "cont" 1))
              (make-board (list (make-block (block-x-min block1)
                                            (block-x-max block1)
                                            (block-y-min block1)
                                            (block-y-max block1)
                                            "O"
                                            (block-num block1))
                                (make-block (block-x-min block2)
                                            (block-x-max block2)
                                            (block-y-min block2)
                                            (block-y-max block2)
                                            "O"
                                            (block-num block2))
                                (make-block (block-x-min block3)
                                            (block-x-max block3)
                                            (block-y-min block3)
                                            (block-y-max block3)
                                            "O"
                                            (block-num block3))
                                block4 block5
                                (make-block (block-x-min block6)
                                            (block-x-max block6)
                                            (block-y-min block6)
                                            (block-y-max block6)
                                            "X"
                                            (block-num block6))
                                block7
                                (make-block (block-x-min block8)
                                            (block-x-max block8)
                                            (block-y-min block8)
                                            (block-y-max block8)
                                            "X"
                                            (block-num block8))
                                (make-block (block-x-min block9)
                                            (block-x-max block9)
                                            (block-y-min block9)
                                            (block-y-max block9)
                                            "X"
                                            (block-num block9)))
                          "O" 1))

(define (comp-turn board-in)
  (cond [(check-win board-in "X") (make-board (board-lob board-in) "X" (board-difficulty board-in))]
        [(empty? (filter (λ (block) (equal? (block-filled block) "BLK")) (board-lob board-in)))
         (make-board (board-lob board-in) "draw" (board-lob board-in))]
        [else (local
                [(define (dif-1 board)
                   (first (board-lob (argmax (λ (b-new)
                                               (if (check-win b-new "O") 1 0))
                                             (generate-tree board "O")))))
  
                 (define (dif-2 board)
                   (if (empty? (filter (λ (b-new)
                                         (<= 1 (if (check-win b-new "O") 1 0)))
                                       (generate-tree board "O")))
      
                       (first (board-lob (argmax (λ (b-new)
                                                   (cond [(check-win b-new "O") 1]
                                                         [(check-win b-new "X") 0]
                                                         [else -1]))
                                                 (foldr append empty
                                                        (map (λ (b-in) (generate-tree b-in "X"))
                                                             (generate-tree board "O"))))))
                       (dif-1 board)))
                 (define (0-1-2 board)
                   (cond [(equal? 0 (board-difficulty board))
                          (list-ref empty-blocks (random (length empty-blocks)))]
                         [(equal? 1 (board-difficulty board))
                          (first (board-lob (argmax (λ (b-new)
                                                      (if (check-win b-new "O") 1 0))
                                                    (generate-tree board "O"))))]
                         [(equal? 2 (board-difficulty board))
                          (dif-2 board)]))
                 (define empty-blocks
                   (filter (λ (block) (equal? (block-filled block) "BLK"))
                           (board-lob board-in)))
                 (define c-play (0-1-2 board-in))
                 (define new-board 
                   (make-board (cons (make-block (block-x-min c-play)
                                                 (block-x-max c-play)
                                                 (block-y-min c-play)
                                                 (block-y-max c-play)
                                                 "O"
                                                 (block-num c-play))
                                     (filter (λ (b-in) (not(equal? (block-num c-play)
                                                                   (block-num b-in))))
                                             (board-lob board-in)))
                               (board-winner board-in)
                               (board-lob board-in)))
                 (define (c-turn board-input)
                   (make-board (board-lob board-input)
                               (if (check-win board-input "O")
                                   "O"
                                   "cont")
                               (board-difficulty board-in)))]
                (c-turn new-board))]))





(check-expect (generate-tree 7-filled "O")
              (list
               (make-board
                (list
                 (make-block (block-x-min block6)
                             (block-x-max block6)
                             (block-y-min block6)
                             (block-y-max block6)
                             "O"
                             (block-num block6))
                 (make-block (block-x-min block1)
                             (block-x-max block1)
                             (block-y-min block1)
                             (block-y-max block1)
                             "X"
                             (block-num block1))
                 (make-block (block-x-min block2)
                             (block-x-max block2)
                             (block-y-min block2)
                             (block-y-max block2)
                             "O"
                             (block-num block3))
                 (make-block (block-x-min block3)
                             (block-x-max block3)
                             (block-y-min block3)
                             (block-y-max block3)
                             "X"
                             (block-num block3))
                 (make-block (block-x-min block4)
                             (block-x-max block4)
                             (block-y-min block4)
                             (block-y-max block4)
                             "X"
                             (block-num block4))
                 (make-block (block-x-min block5)
                             (block-x-max block5)
                             (block-y-min block5)
                             (block-y-max block5)
                             "X"
                             (block-num block5))
                 (make-block (block-x-min block7)
                             (block-x-max block7)
                             (block-y-min block7)
                             (block-y-max block7)
                             "O"
                             (block-num block7))
                 block8
                 (make-block (block-x-min block9)
                             (block-x-max block9)
                             (block-y-min block9)
                             (block-y-max block9)
                             "O"
                             (block-num block9)))
                "cont" 1)
               (make-board
                (list
                 (make-block (block-x-min block8)
                             (block-x-max block8)
                             (block-y-min block8)
                             (block-y-max block8)
                             "O"
                             (block-num block8))
                 (make-block (block-x-min block1)
                             (block-x-max block1)
                             (block-y-min block1)
                             (block-y-max block1)
                             "X"
                             (block-num block1))
                 (make-block (block-x-min block2)
                             (block-x-max block2)
                             (block-y-min block2)
                             (block-y-max block2)
                             "O"
                             (block-num block3))
                 (make-block (block-x-min block3)
                             (block-x-max block3)
                             (block-y-min block3)
                             (block-y-max block3)
                             "X"
                             (block-num block3))
                 (make-block (block-x-min block4)
                             (block-x-max block4)
                             (block-y-min block4)
                             (block-y-max block4)
                             "X"
                             (block-num block4))
                 (make-block (block-x-min block5)
                             (block-x-max block5)
                             (block-y-min block5)
                             (block-y-max block5)
                             "X"
                             (block-num block5))
                 block6
                 (make-block (block-x-min block7)
                             (block-x-max block7)
                             (block-y-min block7)
                             (block-y-max block7)
                             "O"
                             (block-num block7))
                 (make-block (block-x-min block9)
                             (block-x-max block9)
                             (block-y-min block9)
                             (block-y-max block9)
                             "O"
                             (block-num block9)))
                "cont" 1)))
(check-expect (generate-tree tie-board "O") empty)

(define (generate-tree board player)
  (local [(define (fill-each board-in player list-of-blocks)
            (map
             (λ (block-in) (make-board (cons (make-block (block-x-min block-in)
                                                         (block-x-max block-in)
                                                         (block-y-min block-in)
                                                         (block-y-max block-in)
                                                         player
                                                         (block-num block-in))
                                             (filter (λ (b-in2) (not(equal? (block-num block-in)
                                                                            (block-num b-in2))))
                                                     (board-lob board-in))) 
                                       (board-winner board-in)
                                       (- (board-difficulty board-in) 1))) list-of-blocks))]

    (fill-each board player (filter (λ (b-in)
                                      (equal? (block-filled b-in) "BLK")) (board-lob board)))))
                                     






(check-expect (check-win START "X") false)
(check-expect (check-win 1-filled "O") false)
(check-expect (check-win
               (make-board (list (make-block (block-x-min block1)
                                             (block-x-max block1)
                                             (block-y-min block1)
                                             (block-y-max block1)
                                             "X"
                                             (block-num block1))
                                 (make-block (block-x-min block2)
                                             (block-x-max block2)
                                             (block-y-min block2)
                                             (block-y-max block2)
                                             "X"
                                             (block-num block2))
                                 (make-block (block-x-min block3)
                                             (block-x-max block3)
                                             (block-y-min block3)
                                             (block-y-max block3)
                                             "X"
                                             (block-num block3))
                                 block4 block5 block6 block7 block8 block9)
                           "cont" 0) "X") true)

(define (check-win board-in player)
  (local [(define held (map block-num (filter (λ (block) (equal? (block-filled block) player))
                                              (board-lob board-in))))
          (define (check-win0 lon)
            (or
             (and (member 1 lon) (member 2 held) (member 3 held))
             (and (member 4 lon) (member 5 held) (member 6 held))
             (and (member 7 lon) (member 8 held) (member 9 held))
             (and (member 1 lon) (member 4 held) (member 7 held))
             (and (member 2 lon) (member 5 held) (member 8 held))
             (and (member 3 lon) (member 6 held) (member 9 held))
             (and (member 1 lon) (member 5 held) (member 9 held))
             (and (member 7 lon) (member 5 held) (member 3 held))))]
    (check-win0 held)))






(check-expect (renderboard START) BOARD-IMG)
(check-expect (renderboard 1-filled)
              (place-image X  (/  (+ (block-x-min block1) (block-x-max block1)) 2)
                           (/ (+ (block-y-min block1) (block-y-max block1)) 2)
                           BOARD-IMG))
 
(define (renderboard board-in)
  (local [ (define (renderboard0 board-in )
             (place-images
              (map get-image (board-lob board-in))
              (map (λ (block) (make-posn (/  (+ (block-x-min block) (block-x-max block)) 2)
                                         (/ (+ (block-y-min block) (block-y-max block)) 2)))
                   (board-lob board-in))
              BOARD-IMG))
           (define (get-image block)
             (cond [(equal? (block-filled block) "X") X]
                   [(equal? (block-filled block) "O") O]
                   [(equal? (block-filled block) "BLK") empty-image]))] 
    (cond [(or (equal? (board-winner board-in) "O") (equal? (board-winner board-in) "X"))
           (place-image (text (string-append (board-winner board-in) " Won" )
                              (floor (/ SIZE 10))
                              "black")
                        (/ SIZE  2) (/  SIZE  2)
                        (renderboard0 board-in))]
          [(equal? (board-winner board-in) "draw")
           (place-image (text "draw" (floor (/ SIZE 10)) "black")
                        (/ SIZE  2)
                        (/  SIZE  2)
                        (renderboard0 board-in))]
          [else 
           (renderboard0 board-in)])))







(check-expect (halt START) false)
(check-expect (halt 1-filled) false)
(check-expect (halt 
               (make-board (list (make-block (block-x-min block1)
                                             (block-x-max block1)
                                             (block-y-min block1)
                                             (block-y-max block1)
                                             "X"
                                             (block-num block1))
                                 (make-block (block-x-min block2)
                                             (block-x-max block2)
                                             (block-y-min block2)
                                             (block-y-max block2)
                                             "X"
                                             (block-num block2))
                                 (make-block (block-x-min block3)
                                             (block-x-max block3)
                                             (block-y-min block3)
                                             (block-y-max block3)
                                             "X"
                                             (block-num block3))
                                 block4 block5 block6 block7 block8 block9)
                           "cont" 0)) true)

(define (halt board-in)
  (or (check-win board-in "X")
      (check-win board-in "O")
      (empty? (filter (λ (b-in) (not (equal? b-in "BLK"))) (board-lob board-in))))) 












(check-expect (level  START "1") (make-board
                          (board-lob START)
                          (board-winner START)
                          1))
(check-expect (level  START "2") (make-board
                          (board-lob START)
                          (board-winner START)
                          2))

(check-expect (level START "0")(make-board
                          (board-lob START)
                          (board-winner START)
                          0))

(define (level board key)
  (cond
    [(key=? "0" key) (make-board
                          (board-lob board)
                          (board-winner board)
                          0)]
    [(key=? "1" key) (make-board
                          (board-lob board)
                          (board-winner board)
                          1)]
    [(key=? "2" key) (make-board
                          (board-lob board)
                          (board-winner board)
                          2)]
    [else board]))


















































