

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |assignment 4 |) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






(require 2htdp/image)
(require 2htdp/universe)






(define SIZE 600) 
(define MTS (empty-scene SIZE SIZE))
(define H-TOKEN (text "X" (/ SIZE 4) "red"))
(define C-TOKEN (text "O" (/ SIZE 4) "blue"))
(define N-TOKEN (square 0 "solid" "white"))
(define MARGIN (/ SIZE 15)) 


(define LINE (make-pen "goldenrod"
                       (round (/ SIZE 70)) "solid" "round" "round"))

(define token-positions (list
                         (make-posn (/ SIZE 2) (/ SIZE 2)) 
                         (make-posn (* SIZE 1/6) (* SIZE 1/6))
                         (make-posn (* SIZE 3/6) (* SIZE 1/6))
                         (make-posn (* SIZE 5/6) (* SIZE 1/6))
                         (make-posn (* SIZE 1/6) (* SIZE 3/6))
                         (make-posn (* SIZE 3/6) (* SIZE 3/6))
                         (make-posn (* SIZE 5/6) (* SIZE 3/6))
                         (make-posn (* SIZE 1/6) (* SIZE 5/6))
                         (make-posn (* SIZE 3/6) (* SIZE 5/6))
                         (make-posn (* SIZE 5/6) (* SIZE 5/6))))


(define win-cases
  (list
   (list 0 1 2)
   (list 3 4 5)
   (list 6 7 8)
   (list 0 3 6)
   (list 1 4 7)
   (list 2 5 8)
   (list 0 4 8)
   (list 2 4 6)))









(define BS0 (list N-TOKEN N-TOKEN N-TOKEN
                  N-TOKEN N-TOKEN N-TOKEN
                  N-TOKEN N-TOKEN N-TOKEN)) 
(define BS1 (list N-TOKEN N-TOKEN N-TOKEN
                  N-TOKEN H-TOKEN N-TOKEN
                  N-TOKEN N-TOKEN N-TOKEN)) 
(define BS2 (list H-TOKEN N-TOKEN C-TOKEN
                  C-TOKEN H-TOKEN N-TOKEN
                  N-TOKEN N-TOKEN H-TOKEN)) 
(define BS3 (list C-TOKEN N-TOKEN H-TOKEN
                  C-TOKEN H-TOKEN H-TOKEN
                  C-TOKEN H-TOKEN N-TOKEN)) 
(define BS4 (list H-TOKEN N-TOKEN H-TOKEN
                  N-TOKEN C-TOKEN N-TOKEN
                  H-TOKEN C-TOKEN N-TOKEN)) 
(define BS5 (list H-TOKEN C-TOKEN H-TOKEN
                  H-TOKEN H-TOKEN C-TOKEN
                  C-TOKEN H-TOKEN C-TOKEN)) 







(define IBS0 (list  0  0  0
                    0  0  0
                    0  0  0)) 
(define IBS1 (list  0  0  0
                    0  1  0
                    0  0  0)) 
(define IBS2 (list  1  0 -1
                    -1  1  0
                    0  0  1)) 
(define IBS3 (list -1  0  1
                   -1  1  1
                   -1  1  0)) 
(define IBS4 (list  1  0  1
                    0 -1  0
                    1 -1  0)) 
(define IBS5 (list  1  0  0
                    0  0  0
                    1 -1  0)) 


(define-struct ws (bs human-turn? win-state depth))









(define WS0   (make-ws BS0 true  0 1))
(define WS1   (make-ws BS1 false 0 1))
(define WS2   (make-ws BS2 false 1 1))
(define WS3   (make-ws BS3 true -1 1))
(define WS4   (make-ws BS4 false 0 1))
(define WS4-2 (make-ws BS4 true  0 1))
(define WS5   (make-ws BS5 false 0 0))
(define START (make-ws BS0 true 0 1))

(define board
  (place-images
   (list
    (line 0 (- SIZE MARGIN) LINE)
    (line 0 (- SIZE MARGIN) LINE)
    (line (- SIZE MARGIN) 0 LINE)
    (line (- SIZE MARGIN) 0 LINE))
   (list
    (make-posn (/ SIZE 3) (/ SIZE 2))
    (make-posn (/ SIZE 1.5) (/ SIZE 2))
    (make-posn (/ SIZE 2) (/ SIZE 3))
    (make-posn (/ SIZE 2) (/ SIZE 1.5)))
   MTS))








(define lox (list 0 1 2 3 4 5 6 7 8 9))
(define los (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(check-expect (val-at 0 empty) '()) 
(check-expect (val-at 3 lox) 3) 
(check-expect (val-at 0 lox) 0) 
(check-expect (val-at 9 los) "9") 
(check-expect (val-at 6 los) "6") 

(define (val-at pos lox)
  (cond
    [(empty? lox) null]
    [(= pos 0) (first lox)]
    [else (val-at (- pos 1) (rest lox))]))







(check-expect (mutate 0 0 empty) empty) 
(check-expect (mutate 3 0 lox) (cons 3 (rest lox))) 
(check-expect (mutate "101" 3 los)
              (cons (first los) 
                    (cons (first (rest los))
                          (cons (first (rest (rest los)))
                                (cons "101"
                                      (rest (rest (rest (rest los)))))))))

(define (mutate value pos lox)
  (cond
    [(empty? lox) empty]
    [(= pos 0) (cons value (rest lox))]
    [else (cons (first lox) (mutate value (- pos 1) (rest lox)))]))
           






(define (key-handler ws ke)
  (local [(define num-key (string->number ke))]
    (cond [(integer? num-key)
           (make-ws
            (ws-bs ws)
            (ws-human-turn? ws)
            (ws-win-state ws)
            num-key)]
          [else
           ws])))

(check-expect (key-handler WS0 "i") WS0) 
(check-expect (key-handler WS0 "left") WS0)
(check-expect (key-handler WS0 "0") 
              (make-ws (ws-bs WS0) (ws-human-turn? WS0) (ws-win-state WS0) 0))
(check-expect (key-handler WS0 "9") 
              (make-ws (ws-bs WS0) (ws-human-turn? WS0) (ws-win-state WS0) 9))









(check-expect (click-handler WS0 0 0 "motion") WS0)
(check-expect (click-handler WS1 100 100 "button-down") WS1)

(check-expect (click-handler WS4 (+ (* SIZE 1/6) (* SIZE 1/3)) 
                             (+ (* SIZE 1/6) (* SIZE 1/3)) "button-down") WS4) 

(check-expect (click-handler (make-ws BS1 true 0 1)
                             (* SIZE 1/2)
                             (* SIZE 1/2)
                             "button-down")
              (make-ws BS1 true 0 1))


(check-expect (click-handler WS0 (* SIZE 1/3 3/4)
                             (* SIZE 1/3 3/4) "button-down") 
              (make-ws (list H-TOKEN N-TOKEN N-TOKEN
                             N-TOKEN N-TOKEN N-TOKEN
                             N-TOKEN N-TOKEN N-TOKEN) 
                       false (ws-win-state WS0) (ws-depth WS0)))

(check-expect (click-handler WS4-2 (+ (* SIZE 1/6) (* SIZE 1/3)) 
                             (- (* SIZE 1/3) (* SIZE 1/6)) "button-down")
              (make-ws (mutate H-TOKEN 1 (ws-bs WS4)) false 1 (ws-depth WS4-2)))


(define (click-handler ws x y mouse-event)
  (local [
          
          (define (square-clicked-on x y)
            (cond
              [(and (< x (* SIZE 1/3)) (< y (* SIZE 1/3))) 0]
              [(and (< x (* SIZE 2/3)) (< y (* SIZE 1/3))) 1]
              [(and (< x (* SIZE 3/3)) (< y (* SIZE 1/3))) 2]
              [(and (< x (* SIZE 1/3)) (< y (* SIZE 2/3))) 3]
              [(and (< x (* SIZE 2/3)) (< y (* SIZE 2/3))) 4]
              [(and (< x (* SIZE 3/3)) (< y (* SIZE 2/3))) 5]
              [(and (< x (* SIZE 1/3)) (< y (* SIZE 3/3))) 6]
              [(and (< x (* SIZE 2/3)) (< y (* SIZE 3/3))) 7]
              [else 8]))
          
          (define square
            (square-clicked-on x y))
          
          
          (define (update-bs bs)
            (mutate H-TOKEN square bs))
          
          (define new-bs (update-bs (ws-bs ws)))]

    
    (cond [(and (= 0 (ws-win-state ws))
                (ws-human-turn? ws)
                (string=? "button-down" mouse-event)
                (equal? N-TOKEN (val-at square (ws-bs ws))))
           (make-ws
            new-bs
            false
            (check-win new-bs)
            (ws-depth ws))]
          [else ws])))















(check-expect (update WS0) WS0)

(check-expect (update WS5) WS5) 

(check-expect (update WS2) WS2)

(check-expect (update WS3) WS3)

(check-expect (update (make-ws BS4 false 0 3))
              (make-ws (list H-TOKEN C-TOKEN H-TOKEN
                             N-TOKEN C-TOKEN N-TOKEN
                             H-TOKEN C-TOKEN N-TOKEN) true -1 3))

               
(define (update ws)
  (local
    [
     
     
     (define (image->int loi)
       (cond
         [(empty? loi) empty]
         [(equal? (first loi) H-TOKEN)
          (cons 1 (image->int (rest loi)))]
         [(equal? (first loi) C-TOKEN)
          (cons -1 (image->int (rest loi)))]
         [else (cons 0 (image->int (rest loi)))]))

     
     
     (define (int->image loi)
       (cond
         [(empty? loi) empty]
         [(equal? (first loi) 1)
          (cons H-TOKEN (int->image (rest loi)))]
         [(equal? (first loi) -1)
          (cons C-TOKEN (int->image (rest loi)))]
         [else (cons N-TOKEN (int->image (rest loi)))]))

     
     
     
     (define (space-on-board bs)
       (cond
         [(empty? bs) false]
         [(equal? (first bs) N-TOKEN) true]
         [else (space-on-board (rest bs))]))]
      
    (cond [(and (not (ws-human-turn? ws))
                (= 0 (ws-win-state ws))
                (space-on-board (ws-bs ws)))
           (local [(define new-board
                     (int->image
                      (minimax (image->int (ws-bs ws)) (ws-depth ws))))]
             (make-ws new-board
                      true
                      (check-win new-board)
                      (ws-depth ws)))]
          [else ws])))











(check-expect (check-win BS1) 0)
(check-expect (check-win BS2) 1)
(check-expect (check-win BS3) -1)

(define (check-win bs)
  (local
    [(define (winner bs lop)
       (cond
         [(empty? lop) false]
         [(and (not (equal? (val-at (val-at 0 (first lop)) bs) N-TOKEN))
               (equal? (val-at (val-at 0 (first lop)) bs)
                       (val-at (val-at 1 (first lop)) bs))
               (equal? (val-at (val-at 1 (first lop)) bs)
                       (val-at (val-at 2 (first lop)) bs)))
          (val-at (val-at 0 (first lop)) bs)]
         [else
          (winner bs (rest lop))]))
     
     (define who-won (winner bs win-cases))]
    (cond
      [(false? who-won) 0]
      [(equal? who-won H-TOKEN) 1]
      [else -1])))



















(check-expect (minimax IBS4 1) (list  1 -1  1
                                      0 -1  0
                                      1 -1  0))
(check-expect (minimax IBS4 2) (list  1 -1  1
                                      0 -1  0
                                      1 -1  0))
(check-expect (minimax IBS4 8) (list  1 -1  1
                                      0 -1  0
                                      1 -1  0))





(check-expect (minimax IBS5 2) (list  1  0  0
                                      -1  0  0
                                      1 -1  0))
(check-expect (minimax IBS5 3) (list  1  0  0
                                      -1  0  0
                                      1 -1  0))

(define (minimax ibs depth)
  (local
    [
     
     
     
     
     (define (winner ibs)
       (local
         [(define (winner ibs lop)
            (cond
              [(empty? lop) (/ (random 50) 100)]
              [(and (= (val-at (val-at 0 (first lop)) ibs)
                       (val-at (val-at 1 (first lop)) ibs)
                       (val-at (val-at 2 (first lop)) ibs))
                    (not (= (val-at (val-at 0 (first lop)) ibs) 0)))
               (val-at (val-at 0 (first lop)) ibs)]
              [else
               (winner ibs (rest lop))]))]
         (winner ibs win-cases)))
     
     
     
     
     
     (define (get-boards ibs token)
       (local
         [
          
          (define (null-poses ibs)
            (local [(define (null-poses-int ibs pos)
                      (cond [(empty? ibs) empty]
                            [(= (first ibs) 0)
                             (cons pos (null-poses-int (rest ibs) (add1 pos)))]
                            [else (null-poses-int (rest ibs) (add1 pos))]))]
              (null-poses-int ibs 0)))
          
          (define (get-boards ibs loi token)
            (cond
              [(empty? loi) empty]
              [else
               (cons
                (mutate token (first loi) ibs)
                (get-boards ibs (rest loi) token))]))]
         
         (get-boards ibs (null-poses ibs) token)))

     
     
     (define (board-full ibs)
       (not (= (foldr * 1 ibs) 0)))

     
     
     
     
     
     
     (define (best-move loibs depth)
       (foldr (λ (ibs1 ibs2)
                (if (< (move-val ibs1 depth true) (move-val ibs2 depth true))
                    ibs1
                    ibs2))                 
              (first loibs)
              (rest loibs)))

     
     
     (define (move-val ibs depth max?)
       (local
         [(define value (winner ibs))]
         (cond
           [(or (= depth 1) (board-full ibs) (= (abs value) 1))
            value]
           [max?
            (apply max (map (λ (ibs)
                              (move-val ibs (- depth 1) false))
                            (get-boards ibs 1)))]
           [else
            (apply min (map (λ (ibs)
                              (move-val ibs (- depth 1) true))
                            (get-boards ibs -1)))])))

     
     
     (define (random-move ibs)
       (local
         [(define move-list (get-boards ibs -1))]
         (val-at (random (length move-list)) move-list)))]
    
    (if (= depth 0)
        (random-move ibs)
        (best-move (get-boards ibs -1) depth))))










(check-expect (render WS1) (place-images 
                            (cons (text "" (/ SIZE 5) "black") BS1)
                            token-positions
                            board))
(check-expect (render WS2) (place-images 
                            (cons (text "X WINS!" (/ SIZE 5) "black") BS2)
                            token-positions
                            board))
(check-expect (render WS3) (place-images 
                            (cons (text "O WINS!" (/ SIZE 5) "black") BS3)
                            token-positions
                            board))
(check-expect (render WS5) (place-images 
                            (cons (text "TIE!" (/ SIZE 5) "black") BS5)
                            token-positions
                            board))

(define (render ws)
  (local
    [(define (contains? token bs)
       (cond [(empty? bs) false]
             [(image=? (first bs) token) true]
             [else (contains? token (rest bs))]))
     
     (define winner (text (cond
                            [(= (ws-win-state ws) 1) "X WINS!"]
                            [(= (ws-win-state ws) -1) "O WINS!"]
                            [(not (contains? N-TOKEN (ws-bs ws))) "TIE!"]
                            [else ""]) (/ SIZE 5) "black"))]
    (place-images
     (cons winner (ws-bs ws))
     token-positions
     board)))
          





(define (main ws)
  (big-bang ws
    (on-tick update)
    (to-draw render)
    (on-key key-handler)
    (on-mouse click-handler)))

